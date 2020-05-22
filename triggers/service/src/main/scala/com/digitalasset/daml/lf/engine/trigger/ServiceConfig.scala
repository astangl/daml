// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine.trigger

import java.nio.file.{Path, Paths}
import java.time.Duration

import com.daml.platform.services.time.TimeProviderType
import scalaz.Show

case class ServiceConfig(
    // For convenience, we allow passing in a DAR on startup
    // as opposed to uploading it dynamically.
    darPath: Option[Path],
    httpPort: Int,
    ledgerHost: String,
    ledgerPort: Int,
    maxInboundMessageSize: Int,
    timeProviderType: TimeProviderType,
    commandTtl: Duration,
    init: Boolean,
    jdbcConfig: Option[JdbcConfig],
    serviceDbUser: Option[DbUser],
)

final case class JdbcConfig(
    url: String,
    user: String,
    password: String,
)

object JdbcConfig {
  implicit val showInstance: Show[JdbcConfig] =
    Show.shows(a => s"JdbcConfig(url=${a.url}, user=${a.user})")

  val driver: String = "org.postgresql.Driver"

  def create(x: Map[String, String]): Either[String, JdbcConfig] =
    for {
      url <- requiredField(x)("url")
      user <- requiredField(x)("user")
      password <- requiredField(x)("password")
    } yield
      JdbcConfig(
        url = url,
        user = user,
        password = password,
      )

  private def requiredField(m: Map[String, String])(k: String): Either[String, String] =
    m.get(k).filter(_.nonEmpty).toRight(s"Invalid JDBC config, must contain '$k' field")

  lazy val usage: String = helpString("<JDBC connection url>", "<user>", "<password>")

  lazy val help: String =
    "Contains comma-separated key-value pairs. Where:\n" +
      s"${indent}url -- JDBC connection URL, beginning with jdbc:postgresql,\n" +
      s"${indent}user -- user name for database user with permissions to create tables,\n" +
      s"${indent}password -- password of database user,\n" +
      s"${indent}Example: " + helpString(
      "jdbc:postgresql://localhost:5432/triggers",
      "operator",
      "password")

  private def helpString(url: String, user: String, password: String): String =
    s"""\"url=$url,user=$user,password=$password\""""

  private val indent: String = List.fill(8)(" ").mkString
}

final case class DbUser(
    user: String,
    password: String,
)

object DbUser {
  def create(x: Map[String, String]): Either[String, DbUser] =
    for {
      user <- requiredField(x)("user")
      password <- requiredField(x)("password")
    } yield
      DbUser(
        user = user,
        password = password,
      )

  private def requiredField(m: Map[String, String])(k: String): Either[String, String] =
    m.get(k).filter(_.nonEmpty).toRight(s"Invalid database user, must contain '$k' field")

  lazy val usage: String = helpString("<user>", "<password>")

  lazy val help: String =
    "Contains comma-separated key-value pairs. Where:\n" +
      s"${indent}user -- user name for database user to create with permissions to read and write tables,\n" +
      s"${indent}password -- password of database user to be created,\n" +
      s"${indent}Example: " + helpString(
      "service",
      "servicepass")

  private def helpString(user: String, password: String): String =
    s"""\"user=$user,password=$password\""""

  private val indent: String = List.fill(8)(" ").mkString
}

object ServiceConfig {
  val DefaultHttpPort: Int = 8088
  val DefaultMaxInboundMessageSize: Int = RunnerConfig.DefaultMaxInboundMessageSize

  private val parser = new scopt.OptionParser[ServiceConfig]("trigger-service") {
    head("trigger-service")

    opt[String]("dar")
      .optional()
      .action((f, c) => c.copy(darPath = Some(Paths.get(f))))
      .text("Path to the dar file containing the trigger")

    opt[Int]("http-port")
      .optional()
      .action((t, c) => c.copy(httpPort = t))
      .text(s"Optional HTTP port. Defaults to ${DefaultHttpPort}")

    opt[String]("ledger-host")
      .required()
      .action((t, c) => c.copy(ledgerHost = t))
      .text("Ledger hostname")

    opt[Int]("ledger-port")
      .required()
      .action((t, c) => c.copy(ledgerPort = t))
      .text("Ledger port")

    opt[Int]("max-inbound-message-size")
      .action((x, c) => c.copy(maxInboundMessageSize = x))
      .optional()
      .text(
        s"Optional max inbound message size in bytes. Defaults to ${DefaultMaxInboundMessageSize}")

    opt[Unit]('w', "wall-clock-time")
      .action { (t, c) =>
        c.copy(timeProviderType = TimeProviderType.WallClock)
      }
      .text("Use wall clock time (UTC). When not provided, static time is used.")

    opt[Long]("ttl")
      .action { (t, c) =>
        c.copy(commandTtl = Duration.ofSeconds(t))
      }
      .text("TTL in seconds used for commands emitted by the trigger. Defaults to 30s.")

    opt[Map[String, String]]("jdbc")
      .action((x, c) =>
        c.copy(jdbcConfig = Some(JdbcConfig.create(x).fold(sys.error, identity))))
      .optional()
      .valueName(JdbcConfig.usage)
      .text("JDBC configuration parameters. If omitted the service runs without a database.")

    cmd("init-db")
      .action((_, c) => c.copy(init = true))
      .text("Initialize database and terminate.")
      .children(
        opt[Map[String, String]]("service-db-user")
          .action((x, c) => c.copy(serviceDbUser = Some(DbUser.create(x).fold(sys.error, identity))))
          .valueName(DbUser.usage)
          .text("Database user to be created with permissions to read and write to tables.")
      )
  }

  def parse(args: Array[String]): Option[ServiceConfig] =
    parser.parse(
      args,
      ServiceConfig(
        darPath = None,
        httpPort = DefaultHttpPort,
        ledgerHost = null,
        ledgerPort = 0,
        maxInboundMessageSize = DefaultMaxInboundMessageSize,
        timeProviderType = TimeProviderType.Static,
        commandTtl = Duration.ofSeconds(30L),
        init = false,
        jdbcConfig = None,
        serviceDbUser = None,
      ),
    )
}
