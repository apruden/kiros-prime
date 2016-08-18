package com.monolito.kiros.prime

import akka.actor.ActorSystem
import akka.io.IO

import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import java.net.URL
import java.util.Date
import java.nio.charset.StandardCharsets
import java.util.Base64
import java.time.Instant

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.concurrent.Future
import scala.util._
import scala.collection.immutable._
import java.net.URL
import java.net.URLEncoder
import java.io.UnsupportedEncodingException
import java.util.Locale;
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaType
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model.HttpMethod
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.ContentTypes
import akka.util.ByteString

object AWS4Signer {
  def urlEncode(url: String, keepPathSlash: Boolean = false) =
    try {
      if (keepPathSlash)
        URLEncoder.encode(url, "UTF-8").replace("%2F", "/")
      else
        URLEncoder.encode(url, "UTF-8")
    } catch {
      case e: UnsupportedEncodingException =>
        throw new RuntimeException("UTF-8 encoding is not supported.", e)
    }

  def toHex(data: Array[Byte]): String = {
    val sb = new StringBuilder(data.length * 2)

    data.map(Integer.toHexString(_))
      .map(hex => {
        if (hex.length() == 1) {
          sb.append("0")
          sb.append(hex)
        } else if (hex.length() == 8) {
          sb.append(hex.substring(6))
        } else {
          sb.append(hex)
        }
      })

    sb.toString().toLowerCase(Locale.getDefault())
  }

  val EMPTY_BODY_SHA256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  val UNSIGNED_PAYLOAD = "UNSIGNED-PAYLOAD"
  val SCHEME = "AWS4"
  val ALGORITHM = "HMAC-SHA256"
  val TERMINATOR = "aws4_request"
  val ISO8601BasicFormat = "yyyyMMdd'T'HHmmss'Z'"
  val DateStringFormat = "yyyyMMdd"
  val dateTimeFormat = new SimpleDateFormat(ISO8601BasicFormat)
  dateTimeFormat.setTimeZone(new SimpleTimeZone(0, "UTC"))
  val dateStampFormat = new SimpleDateFormat(DateStringFormat)
  dateStampFormat.setTimeZone(new SimpleTimeZone(0, "UTC"))

  def getCanonicalizeHeaderNames(headers: Map[String, String]) =
    TreeMap(headers.toSeq: _*).map(_._1.toLowerCase).mkString(";")

  def getCanonicalizedHeaderString(headers: Map[String, String]) =
    headers.toList match {
      case List() => ""
      case _ =>
        // step1: sort the headers by case-insensitive order
        // step2: form the canonical header:value entries in sorted order.
        // Multiple white spaces in the values should be compressed to a single
        // space.
        val buffer = new StringBuilder

        TreeMap(headers.toSeq: _*).map(e => {
          buffer.append(e._1.toLowerCase().replaceAll("\\s+", " ") + ":" + e._2.replaceAll("\\s+", " "))
          buffer.append("\n")
        })

        buffer.toString()
    }

  def getCanonicalRequest(endpoint: URL,
    httpMethod: String,
    queryParameters: String,
    canonicalizedHeaderNames: String,
    canonicalizedHeaders: String,
    bodyHash: String) = s"""$httpMethod
${getCanonicalizedResourcePath(endpoint)}
$queryParameters
$canonicalizedHeaders
$canonicalizedHeaderNames
$bodyHash"""

  def getCanonicalizedResourcePath(endpoint: URL) = {
    if (endpoint == null) {
      "/"
    } else {
      val path = endpoint.getPath()

      if (path == null || path.isEmpty()) {
        "/"
      } else {
        val encodedPath = urlEncode(path, true)
        if (encodedPath.startsWith("/")) encodedPath else "/".concat(encodedPath)
      }
    }
  }

  def getCanonicalizedQueryString(parameters: Map[String, String]) =
    parameters.toList match {
      case List() => ""
      case _ => TreeMap(parameters.toSeq: _*)
        .map(e => s"${urlEncode(e._1, false)}=${urlEncode(e._2, false)}")
        .mkString("&")
    }

  def getStringToSign(scheme: String, algorithm: String, dateTime: String, scope: String, canonicalRequest: String) =
    s"$scheme-$algorithm\n$dateTime\n$scope\n${toHex(hash(canonicalRequest))}"

  def hash(text: String): Array[Byte] =
    try {
      val md = MessageDigest.getInstance("SHA-256")
      md.update(text.getBytes("UTF-8"))
      md.digest()
    } catch {
      case e: Exception =>
        throw new RuntimeException(s"Unable to compute hash while signing request", e)
    }

  def hash(data: Array[Byte]): Array[Byte] =
    try {
      val md = MessageDigest.getInstance("SHA-256")
      md.update(data)
      md.digest()
    } catch {
      case e: Exception =>
        throw new RuntimeException(s"Unable to compute hash while signing request: ${e.getMessage()}", e)
    }

  def sign(stringData: String, key: Array[Byte], algorithm: String): Array[Byte] =
    try {
      val data = stringData.getBytes("UTF-8")
      val mac = Mac.getInstance(algorithm)
      mac.init(new SecretKeySpec(key, algorithm))
      mac.doFinal(data)
    } catch {
      case e: Exception =>
        throw new RuntimeException("Unable to calculate a request signature: " + e.getMessage(), e)
    }

  def computeSignature(headers: Map[String, String],
    queryParameters: Map[String, String],
    bodyHash: String,
    awsAccessKey: String,
    awsSecretKey: String,
    endpointUrl: URL,
    httpMethod: String,
    serviceName: String,
    regionName: String,
    dateTimeStamp: String,
    dateStamp: String) = {
    val canonicalizedHeaderNames = getCanonicalizeHeaderNames(headers)
    val canonicalizedHeaders = getCanonicalizedHeaderString(headers)
    val canonicalizedQueryParameters = getCanonicalizedQueryString(queryParameters)
    val canonicalRequest = getCanonicalRequest(endpointUrl, httpMethod,
      canonicalizedQueryParameters, canonicalizedHeaderNames,
      canonicalizedHeaders, bodyHash)

    val scope = s"$dateStamp/$regionName/$serviceName/$TERMINATOR"
    val stringToSign = getStringToSign(SCHEME, ALGORITHM, dateTimeStamp, scope, canonicalRequest)
    val kSecret = (SCHEME + awsSecretKey).getBytes()
    val kDate = sign(dateStamp, kSecret, "HmacSHA256")
    val kRegion = sign(regionName, kDate, "HmacSHA256")
    val kService = sign(serviceName, kRegion, "HmacSHA256")
    val kSigning = sign(TERMINATOR, kService, "HmacSHA256")
    val signature = sign(stringToSign, kSigning, "HmacSHA256")
    val credentialsAuthorizationHeader = s"Credential=$awsAccessKey/$scope"
    val signedHeadersAuthorizationHeader = s"SignedHeaders=$canonicalizedHeaderNames"
    val signatureAuthorizationHeader = s"Signature=${toHex(signature)}"

    s"$SCHEME-$ALGORITHM $credentialsAuthorizationHeader, $signedHeadersAuthorizationHeader, $signatureAuthorizationHeader"
  }
}

object S3Client {
  import concurrent.ExecutionContext.Implicits._
  import AWS4Signer._
  val bucketName = "kiros-prime"
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def base64Encode(data: Array[Byte]): String =
    Base64.getEncoder().encodeToString(data)

  def sha1(data: String, key: String): Array[Byte] = {
    val keySpec = new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "HmacSHA1")
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(keySpec)
    mac.doFinal(data.getBytes(StandardCharsets.UTF_8))
  }

  def getSignedUrl(id: String, filename: String): String = {
    val awsId = conf.getString("kiros.prime.aws-key-id")
    val awsSecret = conf.getString("kiros.prime.aws-secret-key")
    val disposition = "attachment;filename=\"" + filename + "\""
    val expires = Instant.now().toEpochMilli / 1000 + 3600
    val stringToSign = s"GET\n\n\n$expires\n/$bucketName/$id?response-content-disposition=$disposition"
    val digest  = base64Encode(sha1(stringToSign, awsSecret))
    val url = s"http://s3.amazonaws.com/$bucketName/$id"

    s"$url?response-content-disposition=${urlEncode(disposition)}&AWSAccessKeyId=$awsId&Expires=$expires&Signature=${urlEncode(digest)}"
  }


  def putObject(key: String, obj: Array[Byte], contentType: String): Future[String] = {
    val url = s"http://s3.amazonaws.com/$bucketName/$key"
    //val url = s"http://localhost:8888/$bucketName/$key"
    val endpointUrl = new URL(url)
    val headers = getHeaders(
          endpointUrl,
          conf.getString("kiros.prime.aws-key-id"),
          conf.getString("kiros.prime.aws-secret-key"),
          obj,
          contentType)
    Http().singleRequest(
      HttpRequest(
        uri = url,
        method=HttpMethods.PUT,
        headers=headers,
        entity=HttpEntity.Strict(ContentTypes.`application/octet-stream`, ByteString(obj))
      )).map(r => {
        key
      })
  }

  def getHeaders(endpointUrl: URL, awsAccessKey: String, awsSecretKey: String, objectContent: Array[Byte], contentType: String) = {
    val contentHash = hash(objectContent)
    val contentHashString = toHex(contentHash)
    val now = new Date
    val dateTimeStamp = dateTimeFormat.format(now)
    val dateStamp = dateStampFormat.format(now)

    val hostHeader = endpointUrl.getHost()
    val port = endpointUrl.getPort()

    if (port > -1) {
      hostHeader.concat(":" + Integer.toString(port))
    }

    val headers = Map(
      "content-length" -> s"${objectContent.length}",
      "content-type" -> "application/octet-stream", //s"$contentType",
      "host" -> hostHeader,
      "x-amz-content-sha256" -> contentHashString,
      "x-amz-date" -> dateTimeStamp,
      "x-amz-acl" -> "public-read",
      "x-amz-storage-class" -> "REDUCED_REDUNDANCY")

    val authorization = computeSignature(
      headers,
      Map(),
      contentHashString,
      awsAccessKey,
      awsSecretKey,
      endpointUrl,
      "PUT",
      "s3",
      "us-east-1",
      dateTimeStamp,
      dateStamp)

    (headers + ("Authorization" -> authorization)).map((e) => RawHeader(e._1, e._2)).to[collection.immutable.Seq]
  }
}
