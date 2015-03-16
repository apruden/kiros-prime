package com.monolito.kiros.prime.model

import java.time.Instant
import scala.collection.JavaConverters._
import com.sksamuel.elastic4s.source.DocumentMap

case class Article (
  id: String,
  title: String,
  content: String,
  tags: List[String],
  modifiedBy: User,
  modified: Instant,
  comments: List[Comment],
  attachments: List[Attachment]
) extends DocumentMap with Entity {
  override def map = Map[String, Any](
    "id" -> id,
    "typeId" -> "article",
    "title" -> title,
    "content" -> content,
    "tags" -> tags.toArray,
    "modifiedBy" -> modifiedBy.asInstanceOf[DocumentMap].map.asJava,
    "modified" -> modified.toString,
    "comments" -> comments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = id
}

case class Activity (
  content: String,
  duration: Float
  ) extends DocumentMap {
  override def map = Map (
    "content" -> content,
    "duration" -> duration
    )
}

case class Report (
  id: String,
  date: Instant,
  activities: List[Activity],
  blockers: List[String],
  modifiedBy: User,
  modified: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {
  override def map = Map[String, Any](
    "id" -> id,
    "typeId" -> "report",
    "date" -> date.toString,
    "activities" -> activities.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray,
    "blockers" -> blockers.toArray,
    "modifiedBy" -> modifiedBy.asInstanceOf[DocumentMap].map.asJava,
    "modified" -> modified.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = id
}

case class Comment (
  id: String,
  targetId: String,
  content:String,
  modifiedBy: User,
  modified: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {

  override def map = Map[String, Any](
    "id" -> id,
    "targetId" -> targetId,
    "content" -> content,
    "modifiedBy" -> modifiedBy.asInstanceOf[DocumentMap].map.asJava,
    "modified" -> modified.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = id
}

case class Attachment (
  id: String,
  filename:String,
  modified: Instant
) extends DocumentMap {
  override def map = Map (
    "id" -> id,
    "filename" -> filename,
    "modified" -> modified.toString
    )
}

case class User (
  userId: String,
  username: String
) extends DocumentMap {
  override def map = Map (
    "userId" -> userId,
    "username" -> username
    )
}


case class SearchResult (
  articles: List[Article],
  reports: List[Report])
