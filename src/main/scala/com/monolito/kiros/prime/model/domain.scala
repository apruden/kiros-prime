package com.monolito.kiros.prime.model

import java.time.Instant
import scala.collection.JavaConverters._
import com.sksamuel.elastic4s.source.DocumentMap

case class Article (
  articleId: String,
  title: String,
  content: String,
  tags: List[String],
  createdBy: User,
  lastEditBy: User,
  lastEdit: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {
  override def map = Map[String, Any](
    "articleId" -> articleId,
    "title" -> title,
    "content" -> content,
    "tags" -> tags.toArray,
    "createdBy" -> createdBy.asInstanceOf[DocumentMap].map.asJava,
    "lastEditBy" -> lastEditBy.asInstanceOf[DocumentMap].map.asJava,
    "lastEdit" -> lastEdit.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = articleId
}

case class Comment (
  commentId: String,
  targetId: String,
  content:String,
  postedBy: User,
  posted: Instant,
  attachments: List[Attachment]
) extends DocumentMap with Entity {

  override def map = Map[String, Any](
    "commentId" -> commentId,
    "targetId" -> targetId,
    "content" -> content,
    "postedBy" -> postedBy.asInstanceOf[DocumentMap].map.asJava,
    "posted" -> posted.toString,
    "attachments" -> attachments.map(x => x.asInstanceOf[DocumentMap].map.asJava).toArray
  )

  def getId = commentId
}

case class Attachment (
  attachmentId: String,
  filename:String,
  created: Instant
) extends DocumentMap {
  override def map = Map (
    "attachmentId" -> attachmentId,
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

