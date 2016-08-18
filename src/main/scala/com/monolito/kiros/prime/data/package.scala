package com.monolito.kiros.prime

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import com.monolito.kiros.prime.model._
import com.monolito.kiros.prime.S3Client.getSignedUrl

import java.time.Instant

package object data {

  implicit val mapperUser: MapConvert[User] = new MapConvert[User] {
    def conv(values: Map[String, Any]): User = User(
      values.get("userId").get.toString,
      values.get("username").get.toString
    )
  }

  implicit val mapperAttachment: MapConvert[Attachment] = new MapConvert[Attachment] {
    def conv(values: Map[String, Any]): Attachment = Attachment(
      values.get("id").get.toString,
      values.get("filename").get.toString,
      Instant.parse(values.get("modified").get.toString),
      getSignedUrl(values.get("id").get.toString, values.get("filename").get.toString)
    )
  }

  implicit val mapperActivity: MapConvert[Activity] = new MapConvert[Activity] {
    def conv(values: Map[String, Any]): Activity = Activity(
      values.get("content").get.toString,
      values.get("duration").get.toString.toFloat
    )
  }

  implicit val mapperBlocker: MapConvert[Blocker] = new MapConvert[Blocker] {
    def conv(values: Map[String, Any]): Blocker = Blocker(
      values.get("content").get.toString
    )
  }

  implicit val mapperArticle: MapConvert[Article] = new MapConvert[Article] {
    def conv(v: Map[String, Any]): Article =
      Article(
        v.get("id").get.toString,
        v.get("title").get.toString,
        v.get("content").get.toString,
        v.get("tags").get.asInstanceOf[Seq[String]].toList,
        v.get("createdBy").get.asInstanceOf[Map[String, Any]].convert[User],
        v.get("modifiedBy").get.asInstanceOf[Map[String, Any]].convert[User],
        Instant.parse(v.get("modified").get.toString),
        v.getOrElse("comments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Comment]},
        v.getOrElse("attachments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Attachment]},
        v.get("_version").asInstanceOf[Option[Int]]
      )
  }

  implicit val mapperReport: MapConvert[Report] = new MapConvert[Report] {
    def conv(v: Map[String, Any]): Report =
      Report(
        v.get("id").get.toString,
        Instant.parse(v.get("date").get.toString),
        v.getOrElse("client", "N/A").toString,
        v.getOrElse("team", "N/A").toString,
        v.get("activities").get.asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Activity]},
        v.get("blockers").get.asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Blocker]},
        v.get("createdBy").get.asInstanceOf[Map[String, Any]].convert[User],
        v.get("modifiedBy").get.asInstanceOf[Map[String, Any]].convert[User],
        Instant.parse(v.get("modified").get.toString),
        v.getOrElse("attachments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Attachment]},
        v.getOrElse("comments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Comment]},
        v.get("_version").asInstanceOf[Option[Int]]
      )
  }

  implicit val mapperComment: MapConvert[Comment] = new MapConvert[Comment] {
    def conv(v: Map[String, Any]): Comment =
      Comment(
        v.get("id").get.toString,
        v.get("targetId").get.toString,
        v.get("targetType").get.toString,
        v.get("content").get.toString,
        v.get("modifiedBy").get.asInstanceOf[Map[String, Any]].convert[User],
        Instant.parse(v.get("modified").get.toString),
        v.get("attachments").get.asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Attachment]}
      )
  }

  implicit val mapperBeat: MapConvert[Beat] = new MapConvert[Beat] {
    def conv(v: Map[String, Any]): Beat = 
      Beat(
        v.get("id").get.toString,
        Instant.parse(v.get("timestamp").get.toString)
      )
  }

}
