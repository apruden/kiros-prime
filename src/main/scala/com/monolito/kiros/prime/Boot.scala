package com.monolito.kiros.prime

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.RouteResult.route2HandlerFlow
import com.typesafe.config.ConfigFactory


object Boot extends App with PrimeService with ProdMyAppContextAware { //with PrimeSslConfiguration {
  import com.monolito.kiros.prime.conf
  import data.EsRepository._

  val (host, port) = (conf.getString("kiros.prime.host"), conf.getInt("kiros.prime.port"))
  val bindingFuture = Http().bindAndHandle(handler=wikiRoutes, host, port) //, serverContext)
  println(s"Server online at http://$host:$port/ ...")
  sys.addShutdownHook(() => bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate()))
}
