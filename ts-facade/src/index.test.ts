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

import {validate, getErrors, JsValidationError, propose, getJsonParser } from './index';

function withErrors(
  schema: any,
  value: any,
  f: (errors: ReadonlyArray<JsValidationError>) => void,
) {
  const res = validate(schema, value);
  const errors = getErrors(res);
  expect(schema).toBe(res.schema);
  expect(value).toBe(res.value);
  f(errors);
}

describe('validate', () => {
  test('string ok', () => {
    withErrors(
      {
        type: 'string',
      },
      'toto',
      (errors) => expect(errors.length).toBe(0),
    );
  });
  test('string ko', () => {
    withErrors(
      {
        type: 'string',
      },
      123,
      (errors) => {
        expect(errors.length).toBe(1);
        expect(errors[0].path).toBe('');
        expect(errors[0].message).toBe('Invalid type: expected string');
      },
    );
  });
});

function withProposals(
  schema: any,
  value: any,
  path: string,
  maxDepth: number,
  f: (proposals: ReadonlyArray<any>) => void,
) {
  const res = validate(schema, value);
  expect(schema).toBe(res.schema);
  expect(value).toBe(res.value);
  const proposals = propose(res, path, maxDepth);
  f(proposals);
}

describe('propose', () => {
  test('string', () => {
    withProposals(
      {
        type: 'string',
      },
      'foo',
      '',
      -1,
      (proposals) => {
        expect(proposals.length).toBe(1);
        expect(proposals[0]).toBe('');
      },
    );
  });

  const objectSchemaFooBar = {
    properties: {
      foo: {
        type: 'string',
      },
      bar: {
        type: 'number',
      },
    },
  };

  test('object depth 1', () => {
    withProposals(objectSchemaFooBar, {}, '', -1, (proposals) => {
      expect(proposals.length).toBe(1);
      expect(proposals[0]).toEqual({ foo: null, bar: null });
    });
  });
  test('object depth 2', () => {
    withProposals(objectSchemaFooBar, {}, '', 2, (proposals) => {
      expect(proposals.length).toBe(1);
      expect(proposals[0]).toEqual({ foo: '', bar: 0 });
    });
  });
});

describe('parse', () => {
  test("parser should be defined", () => {
    const p = getJsonParser({});
    expect(p).toBeDefined();
  });
  test("parser should parse", () => {
    const parseRequest = { text: "{}" };
    const res = getJsonParser({}).parse(parseRequest);
    expect(res.error).toBeUndefined();
    expect(res.success).toBe(true);
    expect(res.markers.length).toEqual(0);
    expect(res.styles.length).toEqual(0);
  });
  test("parser should validate", () => {
    const parseRequest = { text: "true" };
    const res = getJsonParser({
      type: "string"
    }).parse(parseRequest);
    expect(res.error).toBeUndefined();
    expect(res.success).toBe(true);
    expect(res.markers.length).toEqual(1);
    const m0 = res.markers[0];
    expect(m0.offset).toBe(0);
    expect(m0.length).toBe(4);
    expect(m0.severity).toBe("error");
    expect(m0.getMessage("en")).toBe("Invalid type: expected string")
  });
  test("parser should predict", () => {
    const predictRequest = { text: "", offset: 0 };
    const res = getJsonParser({}).predict(predictRequest);
    expect(res.error).toBeUndefined();
    expect(res.success).toBe(true);
    expect(res.proposals.length).toEqual(7);
  });
});
