/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// @ts-ignore
import { JsonSchemaJsFacade } from '@diesel-parser/json-schema-facade-js';

import { DieselParserFacade } from '@diesel-parser/ts-facade';

export interface JsValidationError {
  readonly path: string;
  readonly message: string;
}

export interface JsonValue {
  readonly astValue: any;
}

export function setLang(language: string): void {
  // @ts-ignore
  return JsonSchemaJsFacade.setLang(language);
}

export interface JsValidationResult {
  readonly schema: any;
  readonly value: any;
  readonly res: any;
}

export function parseValue(value: string): JsonValue {
  // @ts-ignore
  return JsonSchemaJsFacade.parseValue(value);
}

export function stringifyValue(value: JsonValue): string {
  // @ts-ignore
  return JsonSchemaJsFacade.stringifyValue(value);
}

export function validate(schema: JsonValue, value: JsonValue): JsValidationResult {
  // @ts-ignore
  return JsonSchemaJsFacade.validate(schema, value);
}

export function toJsonValue(value: JsonValue): any {
  // @ts-ignore
  return JsonSchemaJsFacade.toJsonValue(value);
}

export function getErrors(
  res: JsValidationResult,
): ReadonlyArray<JsValidationError> {
  // @ts-ignore
  return JsonSchemaJsFacade.getErrors(res);
}

export function propose(
  res: JsValidationResult,
  path: string,
  maxDepth = -1,
): ReadonlyArray<JsonValue> {
  // @ts-ignore
  return JsonSchemaJsFacade.propose(res, path, maxDepth);
}

export interface JsRenderer {
  readonly key: string;
  readonly schemaValue: any;
}

export function getRenderers(
  res: JsValidationResult,
): ReadonlyMap<string, JsRenderer | undefined> {
  // @ts-ignore
  return JsonSchemaJsFacade.getRenderers(res);
}

export function getFormats(
  res: JsValidationResult,
  path: string,
): ReadonlyArray<string> {
  // @ts-ignore
  return JsonSchemaJsFacade.getFormats(res, path);
}

export function getJsonParser(schema: JsonValue): DieselParserFacade {
  return JsonSchemaJsFacade.getJsonParser(schema) as DieselParserFacade;
}
