// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.resources

import com.daml.resources.HasExecutionContext

import scala.concurrent.ExecutionContext

final case class Context(executionContext: ExecutionContext)

object Context {

  implicit def executionContext(implicit context: Context): ExecutionContext =
    context.executionContext

  implicit object `Context has ExecutionContext` extends HasExecutionContext[Context] {
    override def executionContext(context: Context): ExecutionContext = context.executionContext
  }

}
