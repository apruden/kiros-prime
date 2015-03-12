package com.monolito.kiros.prime

import org.specs2.mutable.Specification
import org.specs2.mock._
import org.mockito.Matchers._
import spray.http._
import scala.util.Try
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import spray.testkit.Specs2RouteTest
import StatusCodes._
import spray.routing.authentication._
import spray.http.HttpHeaders._
import com.monolito.kiros.prime.data._
import com.monolito.kiros.prime.model._

trait TestMyAppContextAware extends MyAppContextAware {
  val appContext = new StubbedAppContext
}

class StubbedAppContext extends MyAppContext with Mockito {
  val articles = mock[ArticleRepository]

  articles.find(anyString) returns Future { Some(Article("id", "toto", "adf content", List(), User("id", "name"), User("Id", "name"), 1, java.time.Instant.now(), true, List(), List())) }

  articles.save(any[Article]) returns Future { scala.util.Success(()) }
}

class WikiServiceSpec extends Specification with Specs2RouteTest with WikiService with TestMyAppContextAware {
  def actorRefFactory = system

  "AuthService" should {

    "return a greeting for GET requests to the root path" in {
      Get() ~> wikiRoutes ~> check {
        responseAs[String] must contain("wiki")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/non_existant/url") ~> wikiRoutes ~> check {
        handled must beFalse
      }
    }

    "return a MethodNotAllowed error for PUT requests to the root path" in {
      Put() ~> sealRoute(wikiRoutes) ~> check {
        status === MethodNotAllowed
        responseAs[String] === "HTTP method not allowed, supported methods: GET"
      }
    }

    "post article" in {
      Post("/articles", HttpEntity(MediaTypes.`application/json`, """{"articleId":"123", "title": "test", "content": "tata", "tags":[]}""")) ~> addHeader("Authorization", "Bearer dWlkOnVzZXJAdGVzdC5jb206d2lraXxobWFjdmFsdWU=") ~> wikiRoutes ~> check {
        responseAs[String] === "OK"
      }
    }

    "get article" in {
      Get("/articles/toto") ~> wikiRoutes ~> check {
        responseAs[String] must contain("toto")
      }
    }
  }
}
