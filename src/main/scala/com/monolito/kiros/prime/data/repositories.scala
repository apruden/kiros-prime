package com.monolito.kiros.prime.data

import com.monolito.kiros.prime._
import com.monolito.kiros.prime.model._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.time.Instant

trait Repository[T] {
  def find(tid: String): Future[Option[T]]
  def findAll(offset: Int, limit: Int, query:Option[String]=None): Future[List[T]]
  def save(t: T): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

trait ArticleRepository extends Repository[Article] {
  def updateComment(id: String, comment: Comment): Future[Try[Unit]]

  def delComment(id: String, commentId: String): Future[Try[Unit]]
}

trait ReportRepository extends Repository[Report] {
  def updateComment(id: String, comment: Comment): Future[Try[Unit]]

  def delComment(id: String, commentId: String): Future[Try[Unit]]
}

trait CommentRepository extends Repository[Comment]

trait EsRepository[T<:Entity] extends Repository[T] {
  import EsClient._

  val indexName: String
  val docType: String
  val typeId: String

  implicit val mapper: MapConvert[T]

  def find(tid: String): Future[Option[T]] =
    for {
      m <- get (docType, tid)
      r <- query ("comments", Map( "query" -> Map("term" -> Map ("targetId" -> tid), "sort" -> Map("_timestamp" -> Map("order" -> "desc")))))
      z <- Future.successful { (m.get + ("comments" -> r)).convert[T] }
    } yield Some(z)

  def findAll(offset: Int, limit: Int, q:Option[String]=None): Future[List[T]] =
      for {
        c <- query(docType, Map("query" -> Map("term" -> Map ("typeId" -> typeId)), "sort" -> Map("_timestamp" -> Map("order" -> "desc"))))
        u <- query("comments", Map("query" -> Map("terms" -> Map("targetId" -> c.map(_.get("id").get))), "sort" -> Map("_timestamp" -> Map("order" -> "desc")) ))
        x <- Future.successful { c.map(h => {
          val xx = u.filter(_.get("targetId") == h.get("id"))
          h + ("comments" -> xx )
        }) }
      } yield x.map(_.convert[T]).toList

  def save(t: T): Future[Try[Unit]] =
    for {
      x <- esSave("primve_rev", docType, t.map)
      c <- put(docType, t.getId, t.map)
    } yield scala.util.Success(())

  def del(tid: String): Future[Try[Unit]] = ???

}

object EsRepository {
  import EsClient._

  def esQuery(q: String, offset: Int, size: Int): Future[SearchResult] =
    for {
      r <- query("documents", Map("query"-> Map("query_string" -> Map("query" -> q))))
    } yield (SearchResult.apply _)
      .tupled(
        r.partition(
          _.get("typeId") == Some("article")) match {
            case (m, n) =>
              (m.map(_.convert[Article]), n.map(_.convert[Report]))
          })
/*
  def createIndex(indexName: String) = client.execute {
      create index indexName mappings (
        "documents" source true timestamp true dynamic true dateDetection true dynamicDateFormats("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") as (
          "id" typed StringType index "not_analyzed",
          "typeId" typed StringType index "not_analyzed",
          "modifiedBy" typed ObjectType as (
            "userId" typed StringType index "not_analyzed",
            "username" typed StringType index "not_analyzed"
            ),
          "attachments" typed NestedType as (
            "id" typed StringType index "not_analyzed"
          )
        ),
      "comments" source true timestamp true dynamic true dateDetection true dynamicDateFormats("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") as (
        "id" typed StringType index "not_analyzed",
        "targetId" typed StringType index "not_analyzed",
        "modifiedBy" typed ObjectType as (
          "userId" typed StringType index "not_analyzed",
          "username" typed StringType index "not_analyzed"
        )
      )
      ) shards 1 replicas 1
    }.await

  try {
    if (!client.execute { status() }.await.getIndices().contains("prime"))
      createIndex("prime")
      createIndex("prime_rev")
  } catch {
    case e: Throwable => {
      println (e)
    }
  }*/
}
