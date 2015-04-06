package com.monolito.kiros.prime

import akka.actor.{ ActorSystem, Props }
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import spray.io.ServerSSLEngineProvider


object Boot extends App { //with PrimeSslConfiguration {
  import com.monolito.kiros.prime.conf
  import data.EsRepository._

  implicit val system = ActorSystem("on-spray-can")

  val service = system.actorOf(Props[PrimeServiceActor], "kiros-prime-service")

  implicit val timeout = Timeout(5.seconds)

  tryCreateIndex()

  IO(Http) ? Http.Bind(service, interface = conf.getString("kiros.prime.host"), port = conf.getInt("kiros.prime.port"))
}
