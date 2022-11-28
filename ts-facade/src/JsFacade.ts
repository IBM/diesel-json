/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-I23
 * Copyright IBM Corp. 2021
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S Copyright Office.
 */

// @ts-ignore
import { JsonSchemaJsFacade } from 'diesel-json-schema-facade-js';

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
}

export function validate(schema: any, value: any): JsValidationResult {
  // @ts-ignore
  return JsonSchemaJsFacade.validate(schema, value);
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
): ReadonlyArray<any> {
  // @ts-ignore
  return JsonSchemaJsFacade.propose(res, path, maxDepth);
}

export function getFormats(
  res: JsValidationResult,
  path: string,
): ReadonlyArray<string> {
  // @ts-ignore
  return JsonSchemaJsFacade.getFormats(res, path);
}
