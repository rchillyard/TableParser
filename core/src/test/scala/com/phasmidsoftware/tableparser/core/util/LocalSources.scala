package com.phasmidsoftware.tableparser.core.util

import com.sun.net.httpserver.HttpServer
import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets
import java.util.jar.{JarEntry, JarOutputStream}
import scala.util.Using

/**
 * Sources of tables at URLs other than file: ones, for tests (#128): a local HTTP server, and a jar.
 */
object LocalSources:

  /**
   * Serve some files over HTTP, from a server on a free port of localhost, while f runs.
   *
   * @param files the files, each a path (such as "/points.csv") and its contents.
   * @param f     a function of the server's base URI (such as http://localhost:51234).
   * @tparam X the result type.
   * @return the result of f.
   */
  def withHttpServer[X](files: Map[String, String])(f: URI => X): X =
    val server = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
    files.foreach: (path, contents) =>
      server.createContext(path, exchange =>
        val bytes = contents.getBytes(StandardCharsets.UTF_8)
        exchange.getResponseHeaders.add("Content-Type", "text/csv")
        exchange.sendResponseHeaders(200, bytes.length.toLong)
        Using.resource(exchange.getResponseBody)(_.write(bytes))
      )
    server.start()
    try f(URI.create(s"http://localhost:${server.getAddress.getPort}"))
    finally server.stop(0)

  /**
   * Make a (temporary) jar which holds one file, and give the jar: URI of that file.
   *
   * @param name     the name of the file in the jar.
   * @param contents its contents.
   * @return a URI such as jar:file:/tmp/sources123.jar!/points.csv.
   */
  def jarURI(name: String, contents: String): URI =
    val jar = File.createTempFile("sources", ".jar")
    jar.deleteOnExit()
    Using.resource(new JarOutputStream(new java.io.FileOutputStream(jar))): out =>
      out.putNextEntry(new JarEntry(name))
      out.write(contents.getBytes(StandardCharsets.UTF_8))
      out.closeEntry()
    URI.create(s"jar:${jar.toURI}!/$name")
