package com.monolito.kiros.prime.model

import java.time.Instant
import scala.collection.JavaConverters._


trait Entity {
  def getId: String

  def map: Map[String, Any] = Map()
}

case class Article (
  id: String,
  title: String,
  content: String,
  tags: List[String],
  createdBy: User,
  modifiedBy: User,
  modified: Instant,
  comments: List[Comment],
  attachments: List[Attachment],
  version: Option[Int]
) extends Entity {
  override def map = Map[String, Any](
    "id" -> id,
    "typeId" -> "article",
    "title" -> title,
    "content" -> content,
    "tags" -> tags,
    "createdBy" -> createdBy.asInstanceOf[Entity].map,
    "modifiedBy" -> modifiedBy.asInstanceOf[Entity].map,
    "modified" -> modified.toString,
    "attachments" -> attachments.map(_.asInstanceOf[Entity].map)
  )

  def getId = id
}

case class Activity (
  content: String,
  duration: Float
  ) extends Entity{
  def getId = content

  override def map = Map (
    "content" -> content,
    "duration" -> duration
    )
}

case class Blocker (
  content: String
  ) extends Entity {
  def getId = content

  override def map = Map (
    "content" -> content
    )
}

case class Report (
  id: String,
  date: Instant,
  client: String,
  team: String,
  activities: List[Activity],
  blockers: List[Blocker],
  createdBy: User,
  modifiedBy: User,
  modified: Instant,
  attachments: List[Attachment],
  comments: List[Comment],
  version: Option[Int]
) extends Entity {

  override def map = Map[String, Any](
    "id" -> id,
    "typeId" -> "report",
    "client" -> client,
    "team" -> team,
    "date" -> {
      val a = java.time.LocalDateTime.ofInstant(date, java.time.ZoneId.of("GMT"))
      java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS").format(a) + "Z"
    },
    "activities" -> activities.map(_.asInstanceOf[Entity].map),
    "blockers" -> blockers.map(_.asInstanceOf[Entity].map),
    "createdBy" -> createdBy.asInstanceOf[Entity].map,
    "modifiedBy" -> modifiedBy.asInstanceOf[Entity].map,
    "modified" -> modified.toString,
    "attachments" -> attachments.map(_.asInstanceOf[Entity].map)
  )

  def getId = id
}

case class Comment (
  id: String,
  targetId: String,
  targetType: String,
  content:String,
  modifiedBy: User,
  modified: Instant,
  attachments: List[Attachment]
) extends Entity {

  override def map = Map[String, Any](
    "id" -> id,
    "targetId" -> targetId,
    "targetType" -> targetType,
    "content" -> content,
    "modifiedBy" -> modifiedBy.asInstanceOf[Entity].map,
    "modified" -> modified.toString,
    "attachments" -> attachments.map(_.asInstanceOf[Entity].map)
  )

  def getId = id
}

case class Beat (id: String, timestamp: Instant) extends Entity {
  def getId = id

  override def map = Map (
    "id" -> id,
    "timestamp" -> timestamp.toString
  )
}

case class Attachment (
  id: String,
  filename: String,
  modified: Instant
) extends Entity {

  def getId = id

  override def map = Map (
    "id" -> id,
    "filename" -> filename,
    "modified" -> modified.toString
    )
}

case class User (
  userId: String,
  username: String
) extends Entity {

  def getId = userId

  override def map = Map (
    "userId" -> userId,
    "username" -> username
    )
}

case class SearchResult (
  articles: List[Article],
  reports: List[Report])
