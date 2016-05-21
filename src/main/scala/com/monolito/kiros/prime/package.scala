package com.monolito.kiros

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._
import scala.util.Try
import com.typesafe.config._
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext

package object prime {
  val conf = ConfigFactory.load()
  val logger = LoggerFactory.getLogger("prime")

  type #>[E, A] = Kleisli[Future, E, A]

  object ReaderTFuture extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Future[B]): A #> B = Kleisli(f)
    def pure[A, B](r: => Future[B]): A #> B = Kleisli(_ => r)
  }

  implicit def toReader[C, R](f: C => R) = Reader(f)
  implicit def toReaderFutureT[C, R](f: Reader[C, Future[R]]) = ReaderTFuture(f)

  trait MapConvert[A] {
    def conv(values: Map[String, Any]): A
  }

  implicit class Map2Class(values: Map[String, Any]){
    def convert[A](implicit mapper: MapConvert[A]) = mapper conv (values)
  }
}
