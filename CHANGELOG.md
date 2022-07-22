# Revision history for Kriti-Lang

## Upcoming

  - Adds `Kriti.CustomFunctions.basicFuncMap` functions to the kriti executable.
  - Adds `KritiError` type to exports from `Kriti`.
  
## 0.3.2 -- 2022-06-30

  - Adds new functions: `toPairs`, `fromPairs`, `removeNulls`, `concat`, `parserToFunc`
  - Supports optional variable lookup and operators in string interpolation

## 0.3.1 -- 2022-02-23
  - adds support for custom functions.

## 0.3.0 -- 2022-02-07

  - adds defaulting operator `??`
  - Adds optional path lookup operator.
  - Fixes unicode support.
  - Adds a Pretty Printer.
  - Adds `!=`, `>=`, `<=`, `in` operators and `not` function.
  - Adds support for Aeson >=2.0.0
  - Parser rewrite with Happy and Alex with improved source span generation.

## 0.2.2 -- 2021-11-16

  - Fixes a bug in String Templates when interpolating Numbers and Booleans.

## 0.2.1 -- 2021-10-26

  - Field lookup by string literal with brackets
  - escapeUri function for escaping strings when interpolating URIs

## 0.1.0.0 -- 2021-09-28

* First release.

  - A basic CLI tool
  - String Interpolation
  - Loops
  - If statements with `>`, `<`, `==`, `||`, `&&` operations.
