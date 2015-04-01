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


class PrimeServiceActor extends Actor with PrimeService with ProdMyAppContextAware {
  import CustomRejectionHandler._

  def actorRefFactory = context

  def receive = runRoute(wikiRoutes)
}

object WikiJsonProtocol extends DefaultJsonProtocol {
  //implicit declaration order matters
  implicit object InstantJsonFormat extends RootJsonFormat[java.time.Instant] {
    def write(c: java.time.Instant) = JsString(c.toString)
    def read(value: JsValue) = value match {
      case JsString(s) => java.time.Instant.parse(s)
      case _ => deserializationError("Valid values is an ISO date")
    }
  }

  implicit val userFormat = jsonFormat2(User)
  implicit val attachmentFormat = jsonFormat3(Attachment)
  implicit val commentFormat = jsonFormat7(Comment)
  implicit val activityFormat = jsonFormat2(Activity)
  implicit val blockerFormat = jsonFormat1(Blocker)
  implicit val articleFormat = jsonFormat8(Article)
  implicit val reportFormat = jsonFormat8(Report)
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
              complete {
                val fileNames = formData.fields.map {
                  case BodyPart(entity, headers) =>
                    val tmp = java.util.UUID.randomUUID.toString
                    saveAttachment(tmp, new ByteArrayInputStream(entity.data.toByteArray))
                    tmp
                }

                JsObject("fileNames" -> new JsArray(fileNames.map(JsString(_)).toVector))
              }
        }
      } ~
      path("search") {
        get {
          parameters('offset.as[Int] ? 0, 'length.as[Int] ? 20, 'query.as[String]) {
            (offset, length, query) =>
              complete(search(query, offset, length)(appContext))
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

  def search(q: String, offset: Int = 0, size:Int = 20): MyAppContext #> SearchResult = {
    ReaderTFuture { ctx =>
      for {
        r <- esQuery(q, offset, size)
      } yield r
    }
  }

  def addComment(comment:Comment): MyAppContext #> Try[Unit] = {
    val commentToSave = if (comment.id == "") comment.copy(id = java.util.UUID.randomUUID.toString) else comment
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
    val articleToSave = article.copy(id=if (article.id == "") java.util.UUID.randomUUID.toString else article.id, modified=java.time.Instant.now)
    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.articles.save(articleToSave) }
    } yield c
  }

  def deleteArticle(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.articles.del(id) }

  private def saveAttachment(fileName: String, content: InputStream): Boolean =
    saveAttachment[InputStream](fileName, content, {(is, os) =>
      val buffer = new Array[Byte](16384)
      Iterator
        .continually (is.read(buffer))
        .takeWhile (-1 !=)
        .foreach (read => os.write(buffer,0,read))
    })

  private def saveAttachment[T](fileName: String, content: T, writeFile: (T, OutputStream) => Unit): Boolean =
    try {
      val fos = new java.io.FileOutputStream("/home/alex/" + fileName)
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
    val reportToSave = report.copy(id=if (report.id == "") java.util.UUID.randomUUID.toString else report.id, modified=java.time.Instant.now)
    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.reports.save(reportToSave) }
    } yield c
  }

  def deleteReport(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.reports.del(id) }
}
