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

package diesel.json.jsonschema

object Examples {

  val Long: String = """{
                       |  "type": [
                       |    "integer",
                       |    "null"
                       |  ],
                       |  "format": "int64"
                       |}""".stripMargin

  val String: String = """{
                         |  "type": [
                         |    "string",
                         |    "null"
                         |  ]
                         |}""".stripMargin

  val EnumArray: String = """{
                            |  "type": [
                            |    "array",
                            |    "null"
                            |  ],
                            |  "items": {
                            |    "type": [
                            |      "string",
                            |      "null"
                            |    ],
                            |    "enum": [
                            |      "FOO",
                            |      "BAR"
                            |    ]
                            |  }
                            |}""".stripMargin

  val EnumArrayWithoutNull: String = """{
                                       |  "type": "array",
                                       |  "items": {
                                       |    "type": "string",
                                       |    "enum": [
                                       |      "FOO",
                                       |      "BAR"
                                       |    ]
                                       |  }
                                       |}""".stripMargin

  val BeanContainingOtherBean: String = """{
                                          |  "$schema": "https://json-schema.org/draft/2019-09/schema",
                                          |  "$id": "http://schema.BeanWithBean",
                                          |  "type": "object",
                                          |  "properties": {
                                          |    "customer": {
                                          |      "$ref": "#/definitions/schema.Customer"
                                          |    }
                                          |  },
                                          |  "definitions": {
                                          |    "schema.Customer": {
                                          |      "type": "object",
                                          |      "properties": {
                                          |        "firstName": {
                                          |          "type": [
                                          |            "string",
                                          |            "null"
                                          |          ]
                                          |        },
                                          |        "lastName": {
                                          |          "type": [
                                          |            "string",
                                          |            "null"
                                          |          ]
                                          |        },
                                          |        "amount": {
                                          |          "type": "number",
                                          |          "format": "double"
                                          |        },
                                          |        "age": {
                                          |          "type": "integer",
                                          |          "format": "int32"
                                          |        }
                                          |      }
                                          |    }
                                          |  }
                                          |}""".stripMargin

  val Inheritance: String = """{
                              |  "$schema": "https://json-schema.org/draft/2019-09/schema",
                              |  "$id": "http://schema.shape.Rectangle",
                              |  "type": "object",
                              |  "allOf": [
                              |    {
                              |      "$ref": "#/definitions/schema.shape.Shape"
                              |    }
                              |  ],
                              |  "properties": {
                              |    "height": {
                              |      "type": "integer",
                              |      "format": "int32"
                              |    },
                              |    "width": {
                              |      "type": "integer",
                              |      "format": "int32"
                              |    }
                              |  },
                              |  "definitions": {
                              |    "schema.shape.Shape": {
                              |      "type": "object",
                              |      "properties": {
                              |        "name": {
                              |          "type": [
                              |            "string",
                              |            "null"
                              |          ]
                              |        }
                              |      }
                              |    }
                              |  }
                              |}""".stripMargin

  val Polymorphism: String = """{
                               |  "$schema": "https://json-schema.org/draft/2019-09/schema",
                               |  "$id": "http://schema.animal.Animal",
                               |  "type": "object",
                               |  "allOf": [
                               |    {
                               |      "if": {
                               |        "properties": {
                               |          "what": {
                               |            "type": "string",
                               |            "const": "schema.animal.Lion"
                               |          }
                               |        }
                               |      },
                               |      "then": {
                               |        "$ref": "#/definitions/schema.animal.Lion"
                               |      }
                               |    },
                               |    {
                               |      "if": {
                               |        "properties": {
                               |          "what": {
                               |            "type": "string",
                               |            "const": "schema.animal.Elephant"
                               |          }
                               |        }
                               |      },
                               |      "then": {
                               |        "$ref": "#/definitions/schema.animal.Elephant"
                               |      }
                               |    }
                               |  ],
                               |  "definitions": {
                               |    "schema.animal.Animal": {
                               |      "properties": {
                               |        "name": {
                               |          "type": [
                               |            "string",
                               |            "null"
                               |          ]
                               |        },
                               |        "sound": {
                               |          "type": [
                               |            "string",
                               |            "null"
                               |          ]
                               |        },
                               |        "type": {
                               |          "type": [
                               |            "string",
                               |            "null"
                               |          ]
                               |        },
                               |        "endangered": {
                               |          "type": "boolean"
                               |        }
                               |      }
                               |    },
                               |    "schema.animal.Lion": {
                               |      "allOf": [
                               |        {
                               |          "$ref": "#/definitions/schema.animal.Animal"
                               |        }
                               |      ],
                               |      "properties": {
                               |        "mane": {
                               |          "type": "boolean"
                               |        }
                               |      }
                               |    },
                               |    "schema.animal.Elephant": {
                               |      "allOf": [
                               |        {
                               |          "$ref": "#/definitions/schema.animal.Animal"
                               |        }
                               |      ],
                               |      "properties": {
                               |        "trunkLength": {
                               |          "type": "number",
                               |          "format": "double"
                               |        },
                               |        "tusk": {
                               |          "type": "boolean"
                               |        }
                               |      }
                               |    }
                               |  }
                               |}""".stripMargin

  val Cycle: String = """{
                        |  "$schema": "https://json-schema.org/draft/2019-09/schema",
                        |  "$id": "http://schema.TestCyclic$Loop",
                        |  "type": "object",
                        |  "properties": {
                        |    "next": {
                        |      "$ref": "http://schema.TestCyclic$Loop"
                        |    },
                        |    "name": {
                        |      "type": [
                        |        "string",
                        |        "null"
                        |      ]
                        |    }
                        |  }
                        |}""".stripMargin

  val Unwrapping: String = """{
                             |  "$schema": "https://json-schema.org/draft/2019-09/schema",
                             |  "$id": "http://schema.TestUnwrapping$UnwrappingRoot",
                             |  "type": "object",
                             |  "properties": {
                             |    "age": {
                             |      "type": "integer",
                             |      "format": "int32"
                             |    },
                             |    "name.first": {
                             |      "type": [
                             |        "string",
                             |        "null"
                             |      ]
                             |    },
                             |    "name.last": {
                             |      "type": [
                             |        "string",
                             |        "null"
                             |      ]
                             |    }
                             |  }
                             |}""".stripMargin

  val ShapeSchema: String =
    """{
      |  "oneOf": [
      |    {
      |      "$ref": "#/$defs/rectangle"
      |    },
      |    {
      |      "$ref": "#/$defs/circle"
      |    }
      |  ],
      |  "$defs": {
      |    "shape": {
      |      "properties": {
      |        "area": {
      |          "type": "number"
      |        }
      |      }
      |    },
      |    "rectangle": {
      |      "allOf": [
      |        {
      |          "$ref": "#/$defs/shape"
      |        },
      |        {
      |          "properties": {
      |            "height": {
      |              "type": "number"
      |            },
      |            "width": {
      |              "type": "number"
      |            }
      |          },
      |          "required": [ "height", "width" ]
      |        }
      |      ]
      |    },
      |    "circle": {
      |      "allOf": [
      |        {
      |          "$ref": "#/$defs/shape"
      |        },
      |        {
      |          "properties": {
      |            "radius": {
      |              "type": "number"
      |            }
      |          }
      |          "required": [ "radius" ]
      |        }
      |      ]
      |    }
      |  }
      |}""".stripMargin

}
