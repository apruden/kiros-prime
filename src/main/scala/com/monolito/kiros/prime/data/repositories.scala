package com.monolito.kiros.prime.data

import com.monolito.kiros.prime._
import com.monolito.kiros.prime.model._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.source.DocumentMap
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.mappings.FieldType._
import org.elasticsearch.action.get.GetResponse
import scala.collection.JavaConversions._
import java.time.Instant

trait Repository[T] {
  def find(tid: String): Future[Option[T]]
  def findAll(offset: Int, limit: Int, query:Option[String]=None): Future[List[T]]
  def save(t: T): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

trait ArticleRepository extends Repository[Article]

trait CommentRepository extends Repository[Comment] {
  def findByTarget(targetId: String): Future[List[Comment]]
}

trait EsRepository[T<:DocumentMap with Entity] extends Repository[T] {
  import EsRepository._

  val indexName: String
  val docType: String
  implicit val mapper: MapConvert[T]

  def find(tid: String): Future[Option[T]] =
    for {
      c <- client.execute { get id tid from indexName -> docType }
    } yield Some(c.getSource.toMap.convert[T])

  def findAll(offset: Int, limit: Int, query:Option[String]=None): Future[List[T]] =
      for {
        r <- client.execute { search in indexName -> docType }
        c <- Future.successful { r.getHits.getHits }
        x <- Future.successful { c.map(y => y.getSource).toList }
      } yield x.map(z => z.toMap.convert[T])

  def save(t: T): Future[Try[Unit]] =
    for {
      c <- client.execute(index into indexName -> docType doc t id t.getId)
    } yield scala.util.Success(())

  def del(tid: String): Future[Try[Unit]] =
    for {
      c <- client.execute(delete id tid from indexName -> docType)
    } yield scala.util.Success(())
}

object EsRepository {
  val client = ElasticClient.remote("localhost", 9300)

  def createIndex() = client.execute {
      create index "wiki" mappings (
        "articles" source true dynamic true dateDetection true dynamicDateFormats("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") as (
          "articleId" typed StringType index "not_analyzed",
          "createdBy" typed ObjectType as (
            "userId" typed StringType index "not_analyzed",
            "username" typed StringType index "not_analyzed"
            ),
          "lastEditBy" typed ObjectType as (
            "userId" typed StringType index "not_analyzed",
            "username" typed StringType index "not_analyzed"
            ),
          "attachments" typed NestedType as (
            "attachmentId" typed StringType index "not_analyzed"
          )
        ),
        "comments" source true dynamic true dateDetection true dynamicDateFormats("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") as (
          "commentId" typed StringType index "not_analyzed",
          "targetId" typed StringType index "not_analyzed",
          "postedBy" typed ObjectType as (
            "userId" typed StringType index "not_analyzed",
            "username" typed StringType index "not_analyzed"
          ),
          "attachments" typed NestedType as (
            "attachmentId" typed StringType index "not_analyzed"
          )
        )
      ) shards 4
    }.await

  if (!client.execute { status() }.await.getIndices().contains("wiki")) createIndex()
}
