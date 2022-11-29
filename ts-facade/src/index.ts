/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-I23
 * Copyright IBM Corp. 2021
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S Copyright Office.
 */

// @ts-ignore
import { diesel, JsonSchemaJsFacade } from '@diesel-parser/json-schema-facade-js';

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

// parsing

export const DieselParsers = {
  createParseRequest(text: string): ParseRequest {
    return diesel.createParseRequest(text) as ParseRequest;
  },
  createPredictRequest(text: string, offset: number): PredictRequest {
    return diesel.createPredictRequest(text, offset) as PredictRequest;
  }
};

export interface ParseRequest {
  readonly text: string;
  readonly axiom?: string;
}

export interface HasRange {
  readonly offset: number;
  readonly length: number;
}

export interface PredictRequest {
  readonly parseRequest: ParseRequest;
  readonly offset: number;
}

export interface DieselMarker extends HasRange {
  readonly severity: string;
  getMessage(locale: string): string;
}

export interface DieselStyle extends HasRange {
  readonly name: string;
}

export interface HasSuccessAndError {
  readonly success: boolean;
  readonly error?: string;
}

export interface DieselParseResult extends HasSuccessAndError {
  readonly markers: ReadonlyArray<DieselMarker>;
  readonly styles: ReadonlyArray<DieselStyle>;
}

export interface DieselCompletionProposal {
  readonly text: string;
  readonly replace?: HasRange;
}

export interface DieselPredictResult extends HasSuccessAndError {
  readonly proposals: ReadonlyArray<DieselCompletionProposal>;
}

export interface DieselParserFacade {
  parse(request: ParseRequest): DieselParseResult;
  predict(request: PredictRequest): DieselPredictResult;
}

export function getJsonParser(schema: any): DieselParserFacade {
  return JsonSchemaJsFacade.getJsonParser(schema) as DieselParserFacade;
}