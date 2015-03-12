package com.monolito.kiros.prime

import akka.actor.{ ActorSystem, Props }
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import spray.io.ServerSSLEngineProvider


object Boot extends App with PrimeSslConfiguration {

  implicit val system = ActorSystem("on-spray-can")

  val service = system.actorOf(Props[PrimeServiceActor], "kiros-prime-service")

  implicit val timeout = Timeout(5.seconds)

  IO(Http) ? Http.Bind(service, interface = "localhost", port = 20001)
}
