package com.monolito.kiros.prime

import java.io._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import com.monolito.kiros.prime.data._
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
  implicit object InstantJsonFormat extends RootJsonFormat[java.time.Instant] {
    def write(c: java.time.Instant) = JsString(c.toString)
    def read(value: JsValue) = value match {
      case JsString(s) => java.time.Instant.parse(s)
      case _ => deserializationError("Valid values is an ISO date")
    }
  }

  //implicit declaration order matters
  implicit val userFormat = jsonFormat2(User)
  implicit val attachmentFormat = jsonFormat3(Attachment)
  implicit val commentFormat = jsonFormat6(Comment)
  implicit val articleFormat = jsonFormat8(Article)
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

  val appContext: MyAppContext

  val authenticated: Directive1[OAuthCred] = authenticate(OAuth2Auth(validateToken, "prime"))

  val authorized: List[String] => Directive1[OAuthCred] = (scope:List[String]) => authenticated.hflatMap {
    case c :: HNil =>  {
      if (c.anyScope(scope)) provide(c) else reject
    }
    case _ => reject
  }

  val wikiRoutes = cors {
      pathSingleSlash {
        get {
          respondWithMediaType(`text/html`) {
            complete(html.index(None).toString)
          }
        }
      } ~
      path("assets") {
        post {
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
          } ~
          pathPrefix ("comments") {
            pathEnd {
              post {
                authorized(List("prime")) {
                  _ => entity(as[Comment]) {
                    comment => onSuccess(addComment(articleId, comment)(appContext)) {
                      _ => complete("OK")
                    }
                  }
                }
              }
            } ~
            pathPrefix (Segment) { commentId =>
              delete {
                authorized(List("prime")) {
                  _ => onSuccess(deleteComment(articleId, commentId)(appContext)) {
                    _ => complete ("OK")
                  }
                }
              }
            }
          }
        }
      }
    }

  def addComment(articleId: String, comment:Comment): MyAppContext #> Try[Unit] = {
    val commentToSave = if (comment.id == "") comment.copy(id = java.util.UUID.randomUUID.toString) else comment
    ReaderTFuture { ctx: MyAppContext =>
      for {
        r <- ctx.articles.updateComment(articleId, commentToSave)
      } yield r
    }
  }

  def deleteComment (articleId: String, commentId: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { ctx: MyAppContext =>
      for {
        r  <- ctx.articles.delComment(articleId, commentId)
      } yield r
    }

  def searchArticles(offset: Int, length: Int, query: Option[String]): MyAppContext #> List[Article] =
    ReaderTFuture(ctx => ctx.articles.findAll(offset, length, query))

  def getArticle(id: String): MyAppContext #> Option[Article] =
    ReaderTFuture(ctx => ctx.articles.find(id))

  def saveOrUpdateArticle(article: Article, cred: OAuthCred): MyAppContext #> Try[Unit] = {
    val articleToSave = article.copy(id=if (article.id == "") java.util.UUID.randomUUID.toString else article.id, lastEdit=java.time.Instant.now)
    for {
      c <- ReaderTFuture { (ctx: MyAppContext) => ctx.articles.save(articleToSave) }
    } yield c
  }

  def deleteArticle(id: String): MyAppContext #> Try[Unit] =
    ReaderTFuture { (r: MyAppContext) => r.articles.del(id) }

  private def saveAttachment(fileName: String, content: InputStream): Boolean = {
    saveAttachment[InputStream](fileName, content,
    { (is, os) =>
      val buffer = new Array[Byte](16384)
      Iterator
        .continually (is.read(buffer))
        .takeWhile (-1 !=)
        .foreach (read=>os.write(buffer,0,read))
    }
    )
  }

  private def saveAttachment[T](fileName: String, content: T, writeFile: (T, OutputStream) => Unit): Boolean = {
    try {
      val fos = new java.io.FileOutputStream("/home/alex/" + fileName)
      writeFile(content, fos)
      fos.close()
      true
    } catch {
      case _ : Exception => false
    }
  }
}
