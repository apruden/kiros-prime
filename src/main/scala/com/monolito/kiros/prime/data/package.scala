package com.monolito.kiros.prime

import com.sksamuel.elastic4s.source.DocumentMap
import scala.collection.JavaConversions._
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
      Instant.parse(values.get("created").get.toString)
      )
  }

  implicit val mapperArticle: MapConvert[Article] = new MapConvert[Article] {
    def conv(values: Map[String, Any]): Article =
      Article(
        values.get("id").get.toString,
        values.get("title").get.toString,
        values.get("content").get.toString,
        collectionAsScalaIterable(values.get("tags").get.asInstanceOf[java.util.List[String]]).toList,
        mapAsScalaMap(values.get("createdBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
        mapAsScalaMap(values.get("lastEditBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
        Instant.parse(values.get("lastEdit").get.toString),
        collectionAsScalaIterable(values.get("attachments").get.asInstanceOf[java.util.List[Map[String, Any]]]).toList.map {_.convert[Attachment]}
        )
  }

  implicit val mapperReport: MapConvert[Report] = new MapConvert[Report] {
    def conv(values: Map[String, Any]): Report =
      Report(
        values.get("id").get.toString,
        Instant.parse(values.get("date").get.toString),
        collectionAsScalaIterable(values.get("activities").get.asInstanceOf[java.util.List[String]]).toList,
        collectionAsScalaIterable(values.get("blockers").get.asInstanceOf[java.util.List[String]]).toList,
        mapAsScalaMap(values.get("createdBy").get.asInstanceOf[java.util.Map[String, Any]]).toMap.convert[User],
        Instant.parse(values.get("lastEdit").get.toString),
        collectionAsScalaIterable(values.get("attachments").get.asInstanceOf[java.util.List[Map[String, Any]]]).toList.map {_.convert[Attachment]}
        )
  }
}
