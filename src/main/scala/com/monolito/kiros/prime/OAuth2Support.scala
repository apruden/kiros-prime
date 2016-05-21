package com.monolito.kiros.prime

import scala.concurrent.Future

trait OAuth2Support {
  def validateToken(tok: Option[String], scope: String): Future[Option[OAuthCred]] = {
    import java.util.Base64
    val cred = if (tok.nonEmpty) {
      new String(Base64.getDecoder.decode(tok.get), "UTF-8").split('|').toList match {
        case List(data, hmac) =>
          data.split(':').toList match {
            case List(uid, scopes, expire) => {
              if (true)
                Some(OAuthCred(uid, scopes.split(' ').toList, expire.toLong))
              else
                None
            }
            case _ => None
          }
        case _ => None
      }
    } else None

    Future.successful { cred match {
      case cred: Some[OAuthCred] => if(cred.get.anyScope(List(scope))) cred else None
      case _ => None
      }
    }
  }
}

case class OAuthCred(id: String, scopes: List[String], expire: Long) {
  def anyScope(requiredScopes: List[String]): Boolean = !scopes.intersect(requiredScopes).isEmpty
}
