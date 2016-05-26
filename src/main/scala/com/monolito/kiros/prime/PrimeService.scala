package com.monolito.kiros.prime

import java.io._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent._
import scala.util.Try
import com.monolito.kiros.prime.data._
import com.monolito.kiros.prime.data.EsRepository._
import com.monolito.kiros.prime.model._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directive1
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.FileIO

import com.fasterxml.uuid.Generators
import spray.json._
import scalaz._
import Scalaz.{get => _, put => _, _}

import com.monolito.kiros.commons.CorsSupport
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model._

import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.HttpResponse
import akka.stream.scaladsl.FileIO
import akka.stream.scaladsl.StreamConverters
import java.util.concurrent.TimeUnit
import com.monolito.kiros.commons.OAuthCred
import com.monolito.kiros.commons.OAuth2Support


object WikiJsonProtocol {
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

trait PrimeService extends CorsSupport with SprayJsonSupport with OAuth2Support { self: MyAppContextAware =>
  import spray.json.DefaultJsonProtocol._
  import WikiJsonProtocol._
  import com.monolito.kiros.prime.conf
  implicit val timeout = Timeout(5.seconds)
  implicit val system = ActorSystem("on-spray-can")
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit val userFormat = jsonFormat2(User)
  implicit val attachmentFormat = jsonFormat3(Attachment)
  implicit val commentFormat = jsonFormat7(Comment)
  implicit val activityFormat = jsonFormat2(Activity)
  implicit val blockerFormat = jsonFormat1(Blocker)
  implicit val articleFormat = jsonFormat10(Article)
  implicit val reportFormat = jsonFormat12(Report)
  implicit val resultFormat = jsonFormat2(SearchResult)
  
  tryCreateIndex()
  
  val rootPath = conf.getString("kiros.prime.root-path")
  val generator = Generators.timeBasedGenerator()
  val appContext: MyAppContext

  val wikiRoutes = getFromDirectory(rootPath) ~
    pathSingleSlash {
      getFromFile(List(rootPath, "index.html").mkString("/"))
    } ~
    path("conf") {
       complete(Map("files" -> conf.getString("kiros.services.files"),
          "auth" -> conf.getString("kiros.services.auth"),
          "search" -> conf.getString("kiros.services.search"),
          "prime" -> conf.getString("kiros.services.prime")))
    } ~
    cors {
      path("assets") {
        entity(as[FormData]) { formData =>
          onComplete(saveAttachment(formData)) {
            case scala.util.Success(fileNames) => complete(Map("fileNames" -> fileNames))
            case scala.util.Failure(ex) => complete(s"Error saving files ${ex.getMessage}")
          }
        }
      } ~
        path("agg") {
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
              authenticateOAuth2Async("", authenticator("prime")) {
                cred =>
                  {
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
                parameters('offset.as[Int] ? 0, 'length.as[Int] ? 20, 'query.as[String] ?) {
                  (offset, length, query) =>
                    authenticateOAuth2Async("", authenticator("prime")) {
                      _ => complete(searchArticles(offset, length, query)(appContext))
                    }
                }
              }
          } ~
            pathPrefix(Segment) { articleId =>
              pathEnd {
                delete {
                  authenticateOAuth2Async("", authenticator("prime_admin")) {
                    _ =>
                      onSuccess(deleteArticle(articleId)(appContext)) {
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
        pathPrefix("comments") {
          pathEnd {
            post {
              authenticateOAuth2Async("", authenticator("prime")) { _ =>
                entity(as[Comment]) { comment =>
                  onSuccess(addComment(comment)(appContext)) { _ =>
                    complete("OK")
                  }
                }
              }
            }
          } ~
            pathPrefix(Segment) { commentId =>
              delete {
                authenticateOAuth2Async("", authenticator("prime")) {
                  _ =>
                    onSuccess(deleteComment(commentId)(appContext)) {
                      _ => complete("OK")
                    }
                }
              }
            }
        } ~
        pathPrefix("reports") {
          pathEnd {
            (post | put) {
              authenticateOAuth2Async("", authenticator("prime")) {
                cred =>
                  {
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
                parameters('offset.as[Int] ? 0, 'length.as[Int] ? 20, 'query.as[String] ?) {
                  (offset, length, query) =>
                    authenticateOAuth2Async("", authenticator("prime")) {
                      _ => complete(searchReports(offset, length, query)(appContext))
                    }
                }
              }
          } ~
            pathPrefix(Segment) { reportId =>
              pathEnd {
                delete {
                  authenticateOAuth2Async("", authenticator("prime_admin")) {
                    _ =>
                      onSuccess(deleteReport(reportId)(appContext)) {
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

  def search(q: String, offset: Int = 0, size: Int = 20): MyAppContext #> SearchResult =
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

  def addComment(comment: Comment): MyAppContext #> Try[Unit] = {
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
    if (article.id != "" && cred.id =/= article.modifiedBy.userId)
      throw new Exception("Unauthorized Error")

    val articleToSave = article.copy(id = if (article.id == "") generator.generate().toString else article.id, modified = java.time.Instant.now)

    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.articles.save(articleToSave) }
    } yield c
  }

  def deleteArticle(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.articles.del(id) }

  def saveAttachment(formData: FormData): Future[List[String]] = {
    val res = formData.parts.map(bodyPart =>
      (bodyPart.entity.dataBytes.runWith(StreamConverters.asInputStream(FiniteDuration(3, TimeUnit.SECONDS))), bodyPart.filename)).map((x: (InputStream, Option[String])) => {
      val savedFilename = generator.generate().toString
      var ba = Stream.continually(x._1.read).takeWhile(_ != -1).map(_.toByte).toArray
      saveAttachment(savedFilename, x._2, ba)
      savedFilename
    })

    res.runFold(List[String]())((x: List[String], y: String) => y :: x)
  }

  private def saveAttachment(savedFilename: String, fileName: Option[String], content: Array[Byte]): Future[Any] = {
    import java.nio.file.Files
    import java.nio.file.Paths
    import S3Client._
    putObject(savedFilename, content, Files.probeContentType(Paths.get(fileName.getOrElse("filename.bin").toLowerCase)))
  }

  def searchReports(offset: Int, length: Int, query: Option[String]): MyAppContext #> List[Report] =
    ReaderTFuture(ctx => ctx.reports.findAll(offset, length, query))

  def getReport(id: String): MyAppContext #> Option[Report] =
    ReaderTFuture(ctx => ctx.reports.find(id))

  def saveOrUpdateReport(report: Report, cred: OAuthCred): MyAppContext #> Try[Unit] = {
    val reportToSave = report.copy(id = if (report.id == "") generator.generate().toString else report.id, modified = java.time.Instant.now)

    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.reports.save(reportToSave) }
    } yield c
  }

  def deleteReport(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.reports.del(id) }
}
