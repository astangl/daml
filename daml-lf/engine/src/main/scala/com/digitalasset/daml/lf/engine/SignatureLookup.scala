// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine

import com.daml.lf.data.ImmArray
import com.daml.lf.data.Ref._
import com.daml.lf.language.Ast._

object SignatureLookup {
  def lookupDefinition(
      pkg: PackageSignature,
      identifier: QualifiedName,
  ): Either[Error, DefinitionSignature] =
    pkg.lookupIdentifier(identifier).fold(err => Left(Error(err)), Right(_))

  def lookupDataType(
      pkg: PackageSignature,
      identifier: QualifiedName,
  ): Either[Error, DDataTypeSignature] =
    for {
      defn <- lookupDefinition(pkg, identifier)
      dataTyp <- defn match {
        case dataType: DDataTypeSignature => Right(dataType)
        case _: DValueSignature =>
          Left(Error(s"Got value definition instead of datatype when looking up $identifier"))
        case _: DTypeSyn =>
          Left(
            Error(s"Got type synonym definition instead of datatype when looking up $identifier"))
      }
    } yield dataTyp

  def lookupRecord(
      pkg: PackageSignature,
      identifier: QualifiedName,
  ): Either[Error, (ImmArray[(TypeVarName, Kind)], DataRecordSignature)] =
    lookupDataType(pkg, identifier).flatMap { dataTyp =>
      dataTyp.cons match {
        case rec: DataRecordSignature =>
          Right((dataTyp.params, rec))
        case _: DataVariant =>
          Left(Error(s"Expecting record for identifier $identifier, got variant"))
        case _: DataEnum =>
          Left(Error(s"Expecting record for identifier $identifier, got enum"))
      }
    }

  def lookupVariant(
      pkg: PackageSignature,
      identifier: QualifiedName,
  ): Either[Error, (ImmArray[(TypeVarName, Kind)], DataVariant)] =
    lookupDataType(pkg, identifier).flatMap { dataTyp =>
      dataTyp.cons match {
        case v: DataVariant =>
          Right((dataTyp.params, v))
        case _: DataRecordSignature =>
          Left(Error(s"Expecting variant for identifier $identifier, got record"))
        case _: DataEnum =>
          Left(Error(s"Expecting variant for identifier $identifier, got enum"))
      }
    }

  def lookupEnum(pkg: PackageSignature, identifier: QualifiedName): Either[Error, DataEnum] =
    lookupDataType(pkg, identifier).flatMap { dataTyp =>
      dataTyp.cons match {
        case v: DataEnum =>
          Right(v)
        case _: DataVariant =>
          Left(Error(s"Expecting enum for identifier $identifier, got variant"))
        case _: DataRecordSignature =>
          Left(Error(s"Expecting enum for identifier $identifier, got record"))

      }
    }

  def lookupTemplate(
      pkg: PackageSignature,
      identifier: QualifiedName,
  ): Either[Error, TemplateSignature] =
    for {
      dataTyp <- lookupDataType(pkg, identifier)
      tpl <- dataTyp.cons match {
        case DataRecordSignature(_, Some(template)) => Right(template)
        case DataRecordSignature(_, None) =>
          Left(Error(s"Got record with no template when looking up $identifier"))
        case _: DataVariant =>
          Left(Error(s"Expecting template for identifier $identifier, got variant"))
        case _: DataEnum =>
          Left(Error(s"Expecting template for identifier $identifier, got enum"))
      }
    } yield tpl
}
