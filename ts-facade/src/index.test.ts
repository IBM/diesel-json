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

import {
  parseValue,
  stringifyValue,
  validate,
  getErrors,
  JsValidationError,
  propose,
  getJsonParser,
  getRenderers,
  toJsonValue,
  JsonValue,
} from './index';

function parseFromNative(value: any): JsonValue {
  const s = JSON.stringify(value);
  return parseValue(s);
}

function withErrors(
  s: any,
  v: any,
  f: (errors: ReadonlyArray<JsValidationError>) => void,
) {
  const schema = parseFromNative(s);
  const value = parseFromNative(v);
  const res = validate(schema, value);
  const errors = getErrors(res);
  expect(schema).toBe(res.schema);
  expect(value).toBe(res.value);
  f(errors);
}

describe('parse / stringify', () => {
  test('roundtrip', () => {
    const parsed = parseValue('123');
    const stringified = stringifyValue(parsed);
    expect(stringified).toBe('123');
  });

  test('to json value', () => {
    const parsed = parseValue('123');
    const jv = toJsonValue(parsed);
    expect(jv).toEqual({ tag: 'jv-number', value: '123' });
  });
});

describe('validate', () => {
  test('string ok', () => {
    withErrors(
      {
        type: 'string',
      },
      'toto',
      (errors) => {
        expect(errors.length).toBe(0);
      },
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

describe('renderers', function () {
  test('get renderers 1', () => {
    const schema = parseFromNative({
      type: 'string',
      renderer: 'Yalla',
    });
    const res = validate(schema, parseFromNative('Yo'));
    const renderers = getRenderers(res);
    expect(renderers.size).toBe(1);
    const r = renderers.get('');
    expect(r).toBeDefined();
    expect(r?.key).toBe('Yalla');
    expect(r?.schemaValue).toEqual(schema);
    expect(renderers.get('yolo')).toBeUndefined();
  });

  test('get renderers 2', () => {
    const renderer = {
      key: 'Yalla',
      foo: 123,
    };

    const schema = parseFromNative({
      type: 'string',
      renderer,
    });

    const res = validate(schema, parseFromNative('Yo'));
    const renderers = getRenderers(res);
    expect(renderers.size).toBe(1);
    const r = renderers.get('');
    expect(r).toBeDefined();
    expect(r?.key).toBe('Yalla');
    expect(r?.schemaValue).toEqual(schema);
    expect(renderers.get('yolo')).toBeUndefined();
  });
});

function withProposals(
  s: any,
  v: any,
  path: string,
  maxDepth: number,
  f: (proposals: ReadonlyArray<JsonValue>) => void,
) {
  const schema = parseFromNative(s);
  const value = parseFromNative(v);
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
        expect(stringifyValue(proposals[0])).toBe('""');
      },
    );
  });

  test('convert proposal to JsonValue', () => {
    withProposals({type:'string'}, 'foo', '', -1, proposals => {
      expect(proposals.length).toBe(1);
      const jv: JsonValue = proposals[0];
      const jsv = toJsonValue(jv);
      expect(jsv.tag).toBe("jv-string");
      expect(jsv.value).toBe("");
    })
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
      expect(stringifyValue(proposals[0])).toEqual(
        JSON.stringify({ bar: null, foo: null }),
      );
    });
  });
  test('object depth 2', () => {
    withProposals(objectSchemaFooBar, {}, '', 2, (proposals) => {
      expect(proposals.length).toBe(1);
      expect(stringifyValue(proposals[0])).toEqual(
        JSON.stringify({ bar: 0, foo: '' }),
      );
    });
  });
  test('using facade parser', () => {
    const schema = parseFromNative({
      type: 'object',
      properties: {
        foo: {
          type: 'string',
        },
      },
    })
    const parser = getJsonParser(schema);
    const res = parser.predict({ text: '{}', offset: 1 });
    expect(res.success).toBe(true);
    expect(res.error).toBeUndefined;
    expect(res.proposals.map(p => p.text)).toEqual(["}", "\"foo\"", "\"\""])
  })
});

describe('parse', () => {
  test('parser should be defined', () => {
    const p = getJsonParser(parseFromNative({}));
    expect(p).toBeDefined();
  });
  test('parser should parse', () => {
    const parseRequest = { text: '{}' };
    const res = getJsonParser(parseFromNative({})).parse(parseRequest);
    expect(res.error).toBeUndefined();
    expect(res.success).toBe(true);
    expect(res.markers.length).toEqual(0);
    expect(res.styles.length).toEqual(0);
  });
  test('parser should validate', () => {
    const parseRequest = { text: 'true' };
    const res = getJsonParser(
      parseFromNative({
        type: 'string',
      }),
    ).parse(parseRequest);
    expect(res.error).toBeUndefined();
    expect(res.success).toBe(true);
    expect(res.markers.length).toEqual(1);
    const m0 = res.markers[0];
    expect(m0.offset).toBe(0);
    expect(m0.length).toBe(4);
    expect(m0.severity).toBe('error');
    expect(m0.getMessage('en')).toBe('Invalid type: expected string');
  });
  test('parser should predict', () => {
    const predictRequest = { text: '{}', offset: 1 };
    const res = getJsonParser(parseFromNative({
      type: 'object',
      properties: {
        foo: {
          type: 'string',
        },
      },
    })).predict(predictRequest);
    expect(res.error).toBeUndefined();
    expect(res.success).toBe(true);
    expect(res.proposals.map(p => p.text)).toEqual(["}", "\"foo\"", "\"\""]);
  });
});
