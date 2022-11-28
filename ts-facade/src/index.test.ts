/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-I23
 * Copyright IBM Corp. 2021
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S Copyright Office.
 */

import { validate, getErrors, JsValidationError, propose } from './index';

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
