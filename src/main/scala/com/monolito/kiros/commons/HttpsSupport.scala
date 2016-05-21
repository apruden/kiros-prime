package com.monolito.kiros.commons

import java.security.KeyStore
import java.io.FileInputStream
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.TrustManagerFactory
import javax.net.ssl.SSLContext
import akka.http.scaladsl.HttpsConnectionContext

trait HttpsSupport {
  val serverContext = {
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType())
    keyStore.load(new FileInputStream("keystore.jks"), "Admin123".toCharArray)
    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm())
    kmf.init(keyStore, "Admin123".toCharArray)
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    tmf.init(keyStore)
    val context = SSLContext.getInstance("TLS")
    context.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null)
    new HttpsConnectionContext(context)
  }
}