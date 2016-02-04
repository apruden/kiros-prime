package com.monolito.kiros.prime

import java.io._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import com.monolito.kiros.prime.data._
import com.monolito.kiros.prime.data.EsRepository._
import com.monolito.kiros.prime.model._
import akka.actor.Actor
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.unmarshalling._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing._
import scalaz._
import Scalaz._
import scalaz.OptionT._
import spray.routing.authentication._
import spray.http.HttpHeaders._
import shapeless._
import spray.http.StatusCodes.Forbidden

import spray.http.{HttpMethods, HttpMethod, HttpResponse, AllOrigins}
import spray.http.HttpHeaders._
import spray.http.HttpMethods._

import spray.httpx.marshalling.Marshaller

import SprayJsonSupport._

import com.fasterxml.uuid.Generators


class PrimeServiceActor extends Actor with PrimeService with ProdMyAppContextAware {
  import CustomRejectionHandler._

  def actorRefFactory = context

  def receive = runRoute(wikiRoutes)
}

object WikiJsonProtocol extends DefaultJsonProtocol {
  //implicit declaration order matters
  implicit object InstantJsonFormat extends RootJsonFormat[java.time.Instant] {
    def write(c: java.time.Instant) = {
      val a = java.time.LocalDateTime.ofInstant(c, java.time.ZoneId.of("GMT"))
      JsString(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS").format(a) + "Z")
    }

    def read(value: JsValue) = value match {
      case JsString(s) => java.time.Instant.parse(s)
      case _ => deserializationError("Valid values is an ISO date")
    }
  }

  implicit object AnyJsonFormat extends JsonFormat[Any] {
    def write(x: Any) = x match {
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case b: Boolean => if (b) JsTrue else JsFalse
      case q: Seq[Any] => JsArray(q.map(write(_)).toVector)
      case o: Map[String, Any] => JsObject(o.map(e => (e._1, write(e._2))))
      case x => JsString(x.toString)
    }

    def read(value: JsValue): Any = value match {
      case JsNumber(n) => n.intValue()
      case JsString(s) => s
      case JsTrue => true
      case JsFalse => false
      case x: JsArray => x.elements.map(read(_))
      case JsNull => null
      case o: JsObject => o.fields.map(e => (e._1, read(e._2)))
    }
  }

  implicit val userFormat = jsonFormat2(User)
  implicit val attachmentFormat = jsonFormat3(Attachment)
  implicit val commentFormat = jsonFormat7(Comment)
  implicit val activityFormat = jsonFormat2(Activity)
  implicit val blockerFormat = jsonFormat1(Blocker)
  implicit val articleFormat = jsonFormat10(Article)
  implicit val reportFormat = jsonFormat12(Report)
  implicit val resultFormat = jsonFormat2(SearchResult)

  /*implicit object AnyJsonFormat extends JsonFormat[Any] {
    def write(x: Any) = x match {
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case x => JsString(x.toString)
    }

    def read(value: JsValue) = ???
  }*/
}

trait MyAppContextAware {
  val appContext: MyAppContext
}

trait MyAppContext {
  val articles: ArticleRepository
  val reports: ReportRepository
}

class EsMyAppContext extends MyAppContext {
  val articles = new EsArticleRepository
  val reports = new EsReportRepository
}

trait ProdMyAppContextAware extends MyAppContextAware {
  val appContext = new EsMyAppContext
}

trait PrimeService extends HttpService with CORSSupport { self: MyAppContextAware =>
  import WikiJsonProtocol._
  import com.monolito.kiros.prime.conf
  val rootPath = conf.getString("kiros.prime.root-path")
  val generator = Generators.timeBasedGenerator()
  val appContext: MyAppContext

  val authenticated: Directive1[OAuthCred] = authenticate(OAuth2Auth(validateToken, "prime"))

  val authorized: List[String] => Directive1[OAuthCred] = (scope:List[String]) => authenticated.hflatMap {
    case c :: HNil =>  {
      if (c.anyScope(scope)) provide(c) else reject
    }
    case _ => reject
  }

  val wikiRoutes = getFromDirectory (rootPath) ~
      pathSingleSlash {
        getFromFile(List(rootPath,"index.html").mkString("/"))
      } ~
      cors {
      path("assets")  {
          entity(as[MultipartFormData]) { formData =>
              onComplete (saveAttachment(formData)) {
                case scala.util.Success(fileNames) => complete(JsObject("fileNames" -> new JsArray(fileNames.map(JsString(_)).toVector)))
                case scala.util.Failure(ex) => complete (StatusCodes.InternalServerError, s"Error saving files ${ex.getMessage}")
              }
        }
      } ~
        path ("agg") {
          get {
            parameters('query.as[String], 'field.as[String]) {
              (query, field) => complete(getAgg(query, field)(appContext))
            }
          }
        } ~
      path("search") {
        pathEnd {
          get {
            parameters('offset.as[Int] ? 0, 'length.as[Int] ? 20, 'query.as[String]) {
              (offset, length, query) =>
                complete(search(query, offset, length)(appContext))
            }
          }
        }
      } ~
      pathPrefix("articles") {
        pathEnd {
          (post | put) {
            authorized(List("prime")) {
              cred => {
                entity(as[Article]) {
                  article =>
                    onSuccess(saveOrUpdateArticle(article, cred)(appContext)) {
                      _ => complete("OK")
                    }
                }
              }
            }
          } ~
          get {
            parameters('offset.as[Int] ? 0, 'length.as[Int] ? 20, 'query.as[String] ? ) {
              (offset, length, query) => authorized(List("prime")) {
                  _ => complete(searchArticles(offset, length, query)(appContext))
                }
            }
          }
        } ~
        pathPrefix(Segment) { articleId =>
          pathEnd {
            delete {
              authorized(List("prime_admin")) {
                _ => onSuccess(deleteArticle(articleId)(appContext)) {
                  _ => complete("OK")
                }
              }
            } ~
            get {
              complete(getArticle(articleId)(appContext))
            }
          }
        }
      } ~
      pathPrefix ("comments") {
        pathEnd {
          post {
            authorized(List("prime")) { _ =>
              entity(as[Comment]) {comment =>
                onSuccess(addComment(comment)(appContext)) { _ =>
                  complete("OK")
                }
              }
            }
          }
        } ~
        pathPrefix (Segment) { commentId =>
          delete {
            authorized(List("prime")) {
              _ => onSuccess(deleteComment(commentId)(appContext)) {
                _ => complete ("OK")
              }
            }
          }
        }
      } ~
      pathPrefix("reports") {
        pathEnd {
          (post | put) {
            authorized(List("prime")) {
              cred => {
                entity(as[Report]) {
                  report =>
                    onSuccess(saveOrUpdateReport(report, cred)(appContext)) {
                      _ => complete("OK")
                    }
                }
              }
            }
          } ~
          get {
            parameters('offset.as[Int] ? 0, 'length.as[Int] ? 20, 'query.as[String] ? ) {
              (offset, length, query) => authorized(List("prime")) {
                  _ => complete(searchReports(offset, length, query)(appContext))
                }
            }
          }
        } ~
        pathPrefix(Segment) { reportId =>
          pathEnd {
            delete {
              authorized(List("prime_admin")) {
                _ => onSuccess(deleteReport(reportId)(appContext)) {
                  _ => complete("OK")
                }
              }
            } ~
            get {
              complete(getReport(reportId)(appContext))
            }
          }
        }
      }
    }

  def search(q: String, offset: Int = 0, size:Int = 20): MyAppContext #> SearchResult =
    ReaderTFuture { ctx =>
      for {
        r <- esQuery(q, offset, size)
      } yield r
    }

  def getAgg(query: String, field: String): MyAppContext #> List[Map[String, Any]] =
    ReaderTFuture { ctx =>
      for {
        r <- fieldAgg(query, field)
      } yield r
    }

  def addComment(comment:Comment): MyAppContext #> Try[Unit] = {
    val commentToSave = if (comment.id == "") comment.copy(id = generator.generate().toString) else comment
    ReaderTFuture { ctx: MyAppContext =>
      for {
        r <- if (comment.targetType == "article")
                ctx.articles.updateComment(comment.targetId, commentToSave)
             else
                ctx.reports.updateComment(comment.targetId, commentToSave)
      } yield r
    }
  }

  def deleteComment(commentId: String): MyAppContext #> Try[Unit] = ???

  def searchArticles(offset: Int, length: Int, query: Option[String]): MyAppContext #> List[Article] =
    ReaderTFuture(ctx => ctx.articles.findAll(offset, length, query))

  def getArticle(id: String): MyAppContext #> Option[Article] =
    ReaderTFuture(ctx => ctx.articles.find(id))

  def saveOrUpdateArticle(article: Article, cred: OAuthCred): MyAppContext #> Try[Unit] = {
    if (article.id != "" &&  cred.id =/= article.modifiedBy.userId )
      throw new Exception("Unauthorized Error")

    val articleToSave = article.copy(id=if (article.id == "") generator.generate().toString else article.id, modified=java.time.Instant.now)

    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.articles.save(articleToSave) }
    } yield c
  }

  def deleteArticle(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.articles.del(id) }

  def saveAttachment(formData: MultipartFormData): Future[List[String]] = {
    val fileNames = formData.fields.map {
      case bp: BodyPart =>
        val savedFilename = generator.generate().toString
        for {
          r <- saveAttachment(savedFilename, bp.filename, bp.entity.data.toByteArray)
          q <- Future.successful { savedFilename }
        } yield q
        //saveAttachment(savedFilename, new ByteArrayInputStream(bp.entity.data.toByteArray), bp.filename)
    }

    Future.sequence(fileNames.toList)
  }

  private def saveAttachment(savedFilename: String, fileName: Option[String], content: Array[Byte]): Future[Unit] = {
    import java.nio.file.Files
    import java.nio.file.Paths
    import S3Client._

    putObject(savedFilename, content, Files.probeContentType(Paths.get(fileName.getOrElse("filename.bin").toLowerCase)))
  }

  private def saveAttachment(fileName: String, content: InputStream, filename: Option[String]): Boolean =
    saveAttachment[InputStream](fileName, content, {(is, os) =>
      val buffer = new Array[Byte](16384)
      Iterator
        .continually (is.read(buffer))
        .takeWhile (-1 !=)
        .foreach (os.write(buffer, 0, _))
    })

  private def saveAttachment[T](fileName: String, content: T, writeFile: (T, OutputStream) => Unit): Boolean =
    try {
      val fos = new java.io.FileOutputStream(conf.getString("kiros.prime.temp-path") + fileName)
      writeFile(content, fos)
      fos.close()
      true
    } catch {
      case _ : Exception => false
    }

  def searchReports(offset: Int, length: Int, query: Option[String]): MyAppContext #> List[Report] =
    ReaderTFuture(ctx => ctx.reports.findAll(offset, length, query))

  def getReport(id: String): MyAppContext #> Option[Report] =
    ReaderTFuture(ctx => ctx.reports.find(id))

  def saveOrUpdateReport(report: Report, cred: OAuthCred): MyAppContext #> Try[Unit] = {
    val reportToSave = report.copy(id=if (report.id == "") generator.generate().toString else report.id, modified=java.time.Instant.now)

    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.reports.save(reportToSave) }
    } yield c
  }

  def deleteReport(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.reports.del(id) }
}
