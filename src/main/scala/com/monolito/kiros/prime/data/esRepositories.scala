package com.monolito.kiros.prime.data

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import com.monolito.kiros.prime._
import com.monolito.kiros.prime.model._
import java.time.Instant


class EsArticleRepository extends EsRepository[Article] with ArticleRepository {
  import EsClient._
  import com.monolito.kiros.prime.data._

  val indexName = "prime"
  val docType = "documents"
  val typeId = "article"

  implicit val mapper = mapperArticle

  def updateComment(articleId: String, comment: Comment): Future[Try[Unit]] =
    for {
      r <- put("comments", comment.getId, comment.map)
    } yield scala.util.Success(())

  def delComment(id: String, commentId: String): Future[Try[Unit]] = ???

}

class EsReportRepository extends EsRepository[Report] with ReportRepository {
  import EsClient._
  import com.monolito.kiros.prime.data._

  val indexName = "prime"
  val docType = "documents"
  val typeId = "report"
  implicit val mapper = mapperReport

  def updateComment(reportId: String, comment: Comment): Future[Try[Unit]] =
    for {
      r <- put("comments", comment.getId, comment.map)
    } yield scala.util.Success(())

  def delComment(id: String, commentId: String): Future[Try[Unit]] = ???
}
