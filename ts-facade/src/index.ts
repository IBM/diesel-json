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

export function setLang(language: string): void {
  // @ts-ignore
  return JsonSchemaJsFacade.setLang(language);
}

export interface JsValidationResult {
  readonly schema: any;
  readonly value: any;
  readonly res: any;

  getErrors(path: string): readonly JsValidationError[];
  propose(path: string, maxDepth: number): readonly any[];
  getFormats(path: string): readonly string[];

}

export function validate(schema: any, value: any): JsValidationResult {
  // @ts-ignore
  return JsonSchemaJsFacade.validate(schema, value);
}

export interface JsRenderer {
  readonly key: string;
  readonly schemaValue: any;
}

export function getRenderers(
    res: JsValidationResult
): ReadonlyMap<string, JsRenderer | undefined> {
  // @ts-ignore
  return JsonSchemaJsFacade.getRenderers(res)
}

export function getJsonParser(schema: any): DieselParserFacade {
  return JsonSchemaJsFacade.getJsonParser(schema) as DieselParserFacade;
}