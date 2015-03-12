package com.monolito.kiros.prime.data

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.collection.JavaConversions._
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import org.elasticsearch.action.get.GetResponse
import com.monolito.kiros.prime._
import com.monolito.kiros.prime.model._
import java.time.Instant

class EsCommentRepository extends EsRepository[Comment] with CommentRepository {
  import EsRepository._

  val indexName = "wiki"
  val docType = "comments"
  implicit val mapper: MapConvert[Comment] = new MapConvert[Comment] {
    def conv(values: Map[String, Any]): Comment = Comment(
      values.get("commentId").get.toString,
      values.get("targetId").get.toString,
      values.get("content").get.toString,
      mapAsScalaMap(values.get("postedBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
      Instant.parse(values.get("posted").get.toString),
      collectionAsScalaIterable(values.get("attachments").get.asInstanceOf[java.util.List[Map[String, Any]]]).toList.map {_.convert[Attachment]}
    )
  }

  def findByTarget(targetId: String): Future[List[Comment]] = {
    for {
      r <- client.execute { search in indexName -> docType query termQuery("targetId", targetId)}
      c <- Future.successful { r.getHits.getHits }
      x <- Future.successful { c.map(y => y.getSource).toList }
    } yield x.map(z => z.toMap.convert[Comment])
  }
}

class EsArticleRepository extends EsRepository[Article] with ArticleRepository {
  import com.monolito.kiros.prime.data._

  val indexName = "wiki"
  val docType = "articles"
  implicit val mapper = mapperArticle
}
