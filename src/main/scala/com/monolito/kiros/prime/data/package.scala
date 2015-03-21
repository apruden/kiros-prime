package com.monolito.kiros.prime

import com.sksamuel.elastic4s.source.DocumentMap
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
        collectionAsScalaIterable(values.get("tags").get.asInstanceOf[java.util.List[String]]).toList,
        mapAsScalaMap(values.get("modifiedBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
        Instant.parse(values.get("modified").get.toString),
        collectionAsScalaIterable(values.get("comments").get.asInstanceOf[java.util.List[java.util.Map[String, Any]]]).toList.map { c => mapAsScalaMap(c).toMap.convert[Comment]},
        collectionAsScalaIterable(values.get("attachments").get.asInstanceOf[java.util.List[java.util.Map[String, Any]]]).toList.map {c => mapAsScalaMap(c).toMap.convert[Attachment]}
      )
  }

  implicit val mapperReport: MapConvert[Report] = new MapConvert[Report] {
    def conv(values: Map[String, Any]): Report =
      Report(
        values.get("id").get.toString,
        Instant.parse(values.get("date").get.toString),
        collectionAsScalaIterable(values.get("activities").get.asInstanceOf[java.util.List[java.util.Map[String, Any]]]).toList.map {m => mapAsScalaMap(m).toMap.convert[Activity]},
        collectionAsScalaIterable(values.get("blockers").get.asInstanceOf[java.util.List[java.util.Map[String, Any]]]).toList.map {m => mapAsScalaMap(m).toMap.convert[Blocker]},
        mapAsScalaMap(values.get("modifiedBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
        Instant.parse(values.get("modified").get.toString),
        collectionAsScalaIterable(values.get("attachments").get.asInstanceOf[java.util.List[Map[String, Any]]]).toList.map {_.convert[Attachment]},
        collectionAsScalaIterable(values.get("comments").getOrElse(new java.util.ArrayList).asInstanceOf[java.util.List[java.util.Map[String, Any]]]).toList.map { c => mapAsScalaMap(c).toMap.convert[Comment]}
      )
  }

  implicit val mapperComment: MapConvert[Comment] = new MapConvert[Comment] {
    def conv(values: Map[String, Any]): Comment =
      Comment(
        values.get("id").get.toString,
        values.get("targetId").get.toString,
        values.get("targetType").get.toString,
        values.get("content").get.toString,
        mapAsScalaMap(values.get("modifiedBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
        Instant.parse(values.get("modified").get.toString),
        collectionAsScalaIterable(values.get("attachments").get.asInstanceOf[java.util.List[Map[String, Any]]]).toList.map {_.convert[Attachment]}
      )
  }
}
