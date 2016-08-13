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
import com.monolito.kiros.commons.EsClient

trait Repository[T] {
  def find(tid: String): Future[Option[T]]
  def findAll(offset: Int, limit: Int, query:Option[String]=None): Future[List[T]]
  def save(t: T): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

trait BeatRepository extends Repository[Beat] {
  import EsClient._

  def find(tid: String): Future[Option[Beat]] = ???

  def findAll(offset: Int, limit: Int, query:Option[String]=None): Future[List[Beat]] = ???

  def save(t: Beat): Future[Try[Unit]] = {
    for {
      c <- esSave("beats", "beats", t.map)
    } yield scala.util.Success(())
  }

  def getAggregation(query: String): Future[List[Map[String, Any]]] =
    for {
      r <- aggs("beats", "beats", Map(
        "query" -> Map("query_string" -> Map("query" -> query)),
        "aggs"-> Map(
          "result" -> Map(
            "date_histogram" -> Map("interval"->"day", "field"->"timestamp"),
            "aggs" -> Map(
              "presence" -> Map(
                "terms" -> Map("field" -> "id"),
                "aggs" -> Map(
                  "min_timestamp" -> Map(
                    "min" -> Map("field" -> "timestamp")
                  ),
                  "max_timestamp" -> Map(
                    "max" -> Map("field" -> "timestamp")
                  )
                )
              )
            )
          )
        )
      ))
    } yield r

  def del(id: String): Future[Try[Unit]] = ???
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
      m <- get (indexName, docType, tid)
      r <- query (indexName, "comments", Map(
        "query" -> Map(
          "term" -> Map("targetId" -> tid)),
          "sort" -> Map("modified" -> Map("order" -> "desc"))))
      z <- Future.successful { (m.get + ("comments" -> r)).convert[T] }
    } yield Some(z)

  def findAll(offset: Int, limit: Int, q:Option[String]=None): Future[List[T]] =
      for {
        c <- query(indexName, docType, Map(
          "from"->offset,
          "size"->limit,
          "query" -> Map(
            "term" -> Map("typeId" -> typeId)),
            "sort" -> Map("modified" -> Map("order" -> "desc"))
          ))
        u <- query(indexName, "comments", Map(
          "query" -> Map(
            "terms" -> Map("targetId" -> c.map(_.get("id").get))),
            "sort" -> Map("modified" -> Map("order" -> "desc"))
          ))
        x <- Future.successful { c.map(h => {
          val xx = u.filter(_.get("targetId") == h.get("id"))
          h + ("comments" -> xx )
        })}
      } yield x.map(_.convert[T]).toList

  def save(t: T): Future[Try[Unit]] =
    for {
      x <- esSave("prime_rev", docType, t.map)
      c <- put(indexName, docType, t.getId, t.map)
    } yield scala.util.Success(())

  def del(tid: String): Future[Try[Unit]] = ???

}

object EsRepository {
  import EsClient._

  def esQuery(idx:String, q: String, offset: Int, size: Int): Future[SearchResult] =
    for {
      r <- query(idx, "documents", Map(
        "from"->offset,
        "size"->size,
        "query"-> Map(
          "query_string" -> Map("query" -> q)),
          "sort" -> Map("modified" -> Map("order" -> "desc"))
        ))
    } yield (SearchResult.apply _)
      .tupled(
        r.partition(
          _.get("typeId") == Some("article")) match {
            case (m, n) =>
              (m.map(_.convert[Article]), n.map(_.convert[Report]))
          })

  def fieldAgg(idx:String, query:String, field: String): Future[List[Map[String, Any]]] =
    for {
      r <- aggs(idx, "documents", Map(
        "query" -> Map("query_string" -> Map("query" -> query)),
        "aggs"-> Map("result" ->
          Map("terms" ->
            Map("field" -> field)
            )
          )
        )
      )
    } yield r

  def tryCreateIndex() = {
    logger.info("Creating index ....")
    createIndex("beats", Map(
      "settings" -> Map(
        "number_of_shards" -> 1,
        "number_of_replicas" -> 1
      ),
      "mappings" -> Map(
        "beats" -> Map(
          "properties" -> Map(
            "id" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "timestamp" -> Map("type" -> "date", "index"-> "not_analyzed")
          )
        )
      )
    ))
    createIndex("prime", Map(
      "settings" -> Map(
        "number_of_shards" -> 1,
        "number_of_replicas" -> 1
      ),
      "mappings" -> Map(
        "documents" -> Map(
          "date_detection" -> true,
          "dynamic_date_formats" -> "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'",
          "properties" -> Map(
            "id" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "modified" -> Map("type" -> "date", "index"-> "not_analyzed"),
            "typedId" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "modifiedBy" -> Map(
              "type" -> "object",
              "properties" -> Map(
                "userId" -> Map("type" -> "string", "index"-> "not_analyzed"),
                "username" -> Map("type" -> "string", "index"-> "not_analyzed")
              )
            ),
            "attachments" -> Map(
              "type" -> "nested",
              "properties" -> Map(
                "id" -> Map("type" -> "string", "index"-> "not_analyzed")
              )
            )
          )
        ),
        "comments" -> Map(
          "date_detection" -> true,
          "dynamic_date_formats" -> "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'",
          "properties" -> Map(
            "id" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "targetId" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "modified" -> Map("type" -> "date", "index"-> "not_analyzed"),
            "modifiedBy" -> Map(
              "type" -> "object",
              "properties" -> Map(
                "userId" -> Map("type" -> "string", "index"-> "not_analyzed"),
                "username" -> Map("type" -> "string", "index"-> "not_analyzed")
              )
            )
          )
        )
      )
    ))
  }
}
