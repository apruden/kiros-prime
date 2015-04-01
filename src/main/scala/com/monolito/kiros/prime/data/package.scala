package com.monolito.kiros.prime

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import com.monolito.kiros.prime.model._

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
      Instant.parse(values.get("modified").get.toString)
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
    def conv(values: Map[String, Any]): Article =
      Article(
        values.get("id").get.toString,
        values.get("title").get.toString,
        values.get("content").get.toString,
        values.get("tags").get.asInstanceOf[Seq[String]].toList,
        values.get("modifiedBy").get.asInstanceOf[Map[String, Any]].convert[User],
        Instant.parse(values.get("modified").get.toString),
        values.getOrElse("comments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Comment]},
        values.getOrElse("attachments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Attachment]}
      )
  }

  implicit val mapperReport: MapConvert[Report] = new MapConvert[Report] {
    def conv(v: Map[String, Any]): Report =
      Report(
        v.get("id").get.toString,
        Instant.parse(v.get("date").get.toString),
        v.get("activities").get.asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Activity]},
        v.get("blockers").get.asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Blocker]},
        v.get("modifiedBy").get.asInstanceOf[Map[String, Any]].convert[User],
        Instant.parse(v.get("modified").get.toString),
        v.getOrElse("attachments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Attachment]},
        v.getOrElse("comments", List()).asInstanceOf[Seq[Map[String, Any]]].toList.map {_.convert[Comment]}
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
}
