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
import org.elasticsearch.search.sort.SortOrder
import org.elasticsearch.common.settings.ImmutableSettings
import scala.collection.JavaConversions._
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

trait ReportRepository extends Repository[Report]

trait CommentRepository extends Repository[Comment]

trait EsRepository[T<:DocumentMap with Entity] extends Repository[T] {
  import EsRepository._

  val indexName: String
  val docType: String
  val typeId: String

  implicit val mapper: MapConvert[T]

  def find(tid: String): Future[Option[T]] =
    for {
      c <- client.execute { get id tid from indexName -> docType }
    } yield Some(c.getSource.toMap.convert[T])

  def findAll(offset: Int, limit: Int, query:Option[String]=None): Future[List[T]] =
      for {
        r <- client.execute { search in indexName -> docType query termQuery("typeId", typeId) sort { by field "_timestamp" order SortOrder.DESC } }
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
  //val client = ElasticClient.remote("localhost", 9300)
  val settings = ImmutableSettings.settingsBuilder()
  .put("http.enabled", true)

  val client = ElasticClient.local(settings.build)

  def query(q: String, offset: Int, size: Int): Future[SearchResult] =
    for {
      r<- client.execute { search in "prime"->"documents" query q }
      x <- Future.successful { r.getHits.getHits.map(_.getSource).toList }
      } yield (SearchResult.apply _)
        .tupled(
          x.map(_.toMap)
          .partition(
            _.get("typeId") == "article") match {
                case (m, n) =>
                  (m.map(_.convert[Article]), n.map(_.convert[Report]))
              })

  def createIndex() = client.execute {
      create index "prime" mappings (
        "documents" source true timestamp true dynamic true dateDetection true dynamicDateFormats("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") as (
          "id" typed StringType index "not_analyzed",
          "typeId" typed StringType index "not_analyzed",
          "modifiedBy" typed ObjectType as (
            "userId" typed StringType index "not_analyzed",
            "username" typed StringType index "not_analyzed"
            ),
          "comments" typed NestedType as (
            "id" typed StringType index "not_analyzed",
            "targetId" typed StringType index "not_analyzed",
            "modifiedBy" typed ObjectType as (
              "userId" typed StringType index "not_analyzed",
              "username" typed StringType index "not_analyzed"
            )
          ),
          "attachments" typed NestedType as (
            "id" typed StringType index "not_analyzed"
          )
        )
      ) shards 1 replicas 1
    }.await

  try {
    if (!client.execute { status() }.await.getIndices().contains("prime"))
      createIndex()
  } catch {
    case e: Throwable => {
      println (e)
    }
  }
}
