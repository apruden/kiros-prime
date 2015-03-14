package com.monolito.kiros.prime.model

import java.time.Instant
import scala.collection.JavaConverters._
import com.sksamuel.elastic4s.source.DocumentMap

case class Article (
  id: String,
  title: String,
  content: String,
  tags: List[String],
  createdBy: User,
  lastEditBy: User,
  lastEdit: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {
  override def map = Map[String, Any](
    "id" -> id,
    "typeId" -> "article",
    "title" -> title,
    "content" -> content,
    "tags" -> tags.toArray,
    "createdBy" -> createdBy.asInstanceOf[DocumentMap].map.asJava,
    "lastEditBy" -> lastEditBy.asInstanceOf[DocumentMap].map.asJava,
    "lastEdit" -> lastEdit.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = id
}

case class Report (
  id: String,
  date: Instant,
  activities: List[String],
  blockers: List[String],
  createdBy: User,
  lastEdit: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {
  override def map = Map[String, Any](
    "id" -> id,
    "typeId" -> "report",
    "date" -> date.toString,
    "activitie" -> activities.toArray,
    "blockers" -> blockers.toArray,
    "createdBy" -> createdBy.asInstanceOf[DocumentMap].map.asJava,
    "lastEdit" -> lastEdit.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = id
}

case class Comment (
  id: String,
  targetId: String,
  content:String,
  postedBy: User,
  posted: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {

  override def map = Map[String, Any](
    "id" -> id,
    "targetId" -> targetId,
    "content" -> content,
    "postedBy" -> postedBy.asInstanceOf[DocumentMap].map.asJava,
    "posted" -> posted.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = id
}

case class Attachment (
  id: String,
  filename:String,
  created: Instant
) extends DocumentMap {
  override def map = Map (
    "id" -> id,
    "filename" -> filename,
    "created" -> created.toString
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

