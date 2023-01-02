[![CircleCI](https://dl.circleci.com/status-badge/img/gh/IBM/diesel-json/tree/develop.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/IBM/diesel-json/tree/develop)

[![npm version](https://badge.fury.io/js/@diesel-parser%2Fjson-schema-facade-js.svg)](https://badge.fury.io/js/@diesel-parser%2Fjson-schema-facade-js)

[![npm version](https://badge.fury.io/js/@diesel-parser%2Fjson-schema-facade-ts.svg)](https://badge.fury.io/js/@diesel-parser%2Fjson-schema-facade-ts)


# diesel-json

This repo contains :
* an Scala implementation of JSON Schema
* a diesel DSL for JSON, with Schema-based completion/validation
* façades for use in TS/JS

## Build

See `build.sh`.

The scala part needs to be built first. It creates the `js-facade` CommonJS module, containing everything needed in JS land.

    ./build-sbt.sh

Then you can build the `ts-facade` and run its tests. 

    ./build-ts.sh

## Using the lib

Using the lib depends on the target. Scala users will probably use the Scala lib 
directly, whereas TS (or JS) users will find it easier to depend on the npm modules.

test
