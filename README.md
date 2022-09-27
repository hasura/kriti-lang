# Kriti Lang

[![kriti-lang::CI](https://github.com/hasura/kriti-lang/actions/workflows/haskell.yml/badge.svg)](https://github.com/hasura/kriti-lang/actions/workflows/haskell.yml)

A minimal json templating language inspired by Go's template language.

Kriti templates are a superset of JSON with path lookups, if/then/else expressions, loops, and some basic predicate and conditional operators.

Kriti expressions are wrapped in double curly brackets such as `"http://wwww.google.com/{{$body.path}}"`. The `Kriti` evaluator takes `Kriti` template and a set of source json expressions paired with binders then uses them to construct a new json expression.

### Kriti Expressions

#### Path Accessors

Values can be looked up in bound json expressions using the standard path lookup syntax:
```
{{ $body.foo.bar[0]['my key'] }}
```

* `.` is used to look up object fields
* `[x]` is used to lookup array indices
* `['a b c']` is used to lookup string literal object fields

If a variable is unbound, the kriti template fail and throw an exception. To prevent such failures, we provide an "optional" lookup operator:

```
{{ $body?.foo }}
```
This example will return a `null` if `foo` is not bound in `$body`. Optional lookups will immediately shortcircuit with `null` so that the following will not attempt any lookups past the unbound `foo`:
```
{{ $body?.foo.bar.baz }}
```

* `foo?` is used to optionally look up a variable.
* `?.` is used to optionally look up object fields
* `?[x]` is used to optionally lookup array indices
* `?['a b c']` is used to optionally lookup string literal object fields

#### Defaulting Operator

The defaulting operator `??` can be used to replace a `null` value with any other value. The expression `null ?? true` will evaluate to `true`. This is especially useful when used with path lookups:

```
$foo?.bar ?? true
```

#### Loops

The `range` identifier is used to declare for loops:
```
{{ range i, x := $.event.author.articles }}
  {{ x.title }}
{{ end }}
```
`i` and `x` above are binders for the index and value of the array element from `$.event.author.articles`. The index can be omitted by using an underscore in its place.

#### If Statements
Kriti supports if statements and `>` `<` `==` `||` and `&&` operators.
```
{{ if x.published && (x.post_id > 100) }}
    {
      "id": {{x.id}},
      "title": {{x.title}}
    }
{{ else }}
    null
{{ end }}
```

#### String Interpolation
Bound variables, booleans, integers, object/array lookups, and functions can be interpolated:
```
"http://www.{{$.domain}}.com/{{$.path}}"
```

```
"http://www.{{$.domain}}.com/{{ $?[1000] }}"
```

### Library
The library exposes two function `runKriti` and `runKritiWith`, the type definitions of the function are:
``` haskell
runKriti :: Text -> [(Text, Value)] -> Either KritiErr Value
```
The first argument of the function is the template JSON, for example, we can use `myTemplate` as the first argument:
``` haskell
myTemplate :: Text
myTemplate =
    "{\
    \   'name': {{$.name.english}},\
    \   'id': {{$.id}},\
    \   'hp': {{$.base.HP}}\
    \}"
```

``` haskell
runKritiWith :: T.Text -> [(T.Text, J.Value)] -> Map.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value) -> Either KritiError J.Value
```
`runKritiWith` has an additional argument, which takes a hashmap from name of the custon function to it's haskell definition

#### Library Usage Sample Program
To run the example, first clone this repository using the following command:
``` sh
git clone git@github.com:hasura/kriti-lang.git
```
Now, run the following command:
``` sh
cd kriti-lang
cabal new-run example
```

The second argument is a `list` of `tuple` of `(Text, Value)`. The first element of the tuple is the binding to be used for the JSON object, i.e. for the above template we are using `x` as the JSON binding, so, `x` will bind to the JSON object. The second element of the tuple is of type `Data.Aeson.Value` (can be obtained by `Data.Aeson.decode` method).

The function `runKriti` will return `Either KritiErr Value`. If the parser is successful, then it will return `Right Value`, else it will return `Left KritiErr` which can be used for debugging.

#### Basic Functions Collection

The `Kriti.CustomFunctions` module defines functions that can be enabled by including in the `Map` given to `runKritiWith`.

There is also a collection of all these functions defined as `basicFuncMap` that can act as a Kriti stdlib or prelude.

For reference, these functions are listed here:

| Function Name | Description&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; | Example Template | Output |
| --- | --- | --- | --- |
| empty | Returns `true` if an `object`, `array`, or `string` is empty, if a number is `0`, and true for `null`. Raises an error for `boolean`s. | `{"object": {{ empty({"a": 1}) }}, "string": {{ empty("") }}, "array": {{ empty([1]) }} }` | `{"array":false,"object":false,"string":true}` |
| size | Returns the length of an `array` or `string`, the number of keys of an `object`, the value of a `number`, `1` for `true` and `0` for `false`, and `0` for `null`. | `{"object": {{ size({"a": 1}) }}, "string": {{ size("asdf") }}, "array": {{ size([1]) }} }` | `{"array":1,"object":1,"string":4}`|
| inverse | Reverses an `array` or `string`, leaves an `object` or `null` as-is, takes the reciprical of a number, and negates a `bool`. | `{"string": {{ inverse("asdf") }}, "array": {{ inverse([1,2,3]) }}, "number": {{ inverse(4) }} }` | `{"array":[3,2,1],"number":0.25,"string":"fdsa"}` |
| head | Takes the first element or character of an `array` or `string`. Throws an error if they are empty, and throws an error for all other types. | `{"string": {{ head("asdf") }}, "array": {{ head([1,2,3]) }} }` | `{"array":1,"string":"a"}` |
| tail | Drops the first element of an `array` or `string`. Throws an error for all other types. | `{"string": {{ tail("asdf") }}, "array": {{ tail([1,2,3]) }} }` | `{"array":[2,3],"string":"sdf"}` |
| toCaseFold | Converts a `string` to a normalized casing (useful for case-insensitive string comparison). Throws an error for non-strings. | `{"string": {{toCaseFold("AbCd")}} }` | `{"string":"abcd"}` |
| toLower | Converts a `string` to lower-case. Throws an error for non-strings. | `{"string": {{toLower("AbCd")}} }` | `{"string":"abcd"}` |
| toUpper | Converts a `string` to upper-case. Throws an error for non-strings. | `{"string": {{toUpper("AbCd")}} }` | `{"string":"ABCD"}` |
| toTitle | Converts a `string` to title-case. Throws an error for non-strings. | `{"string": {{toTitle("AbCd")}} }` | `{"string":"Abcd"}` |
| fromPairs | Convert an `array` like `[ [a,b], [c,d] ... ]` to an `object` like `{ a:b, c:d ... }` | `{"array": {{ fromPairs([["a",1],["b",2]]) }} }` | `{"array":{"a":1,"b":2}}` |
| toPairs | Convert an `object` like `{ a:b, c:d ... }` to an `array` like `[ [a,b], [c,d] ... ]`. | `{"object": {{ toPairs({"a": 1, "b": 2}) }} }` | `{"object":[["a",1],["b",2]]}` |
| removeNulls | Removes `null` items from an array. | `{"array": {{ removeNulls([1,null,3,null,5]) }} }` | `{"array":[1,3,5]}` |
| concat | Concatenates a `string`, `array`, or `object` - for objects keys from right-most objects are preferred in a collision. | `{"arrays": {{ concat([[1,2],[3,4]]) }}, "strings": {{ concat(["abc", "def", "g"]) }}, "objects": {{ concat([{"a":1, "b":2},{"b":3, "c":4} ] ) }} }` | `{"arrays":[1,2,3,4],"objects":{"a":1,"b":3,"c":4},"strings":"abcdefg"}` |

### CLI Tool
The executable is a CLI tool which applies a transformation to a single json file:
``` bash
➜ cabal run kriti -- --json test/data/eval/success/source.json --template test/data/eval/success/examples/example1.kriti
{"guid":"43a922da-9665-4099-8dfc-f9af369695a4"}
```
The binder for the source file can be changed wit the `--bind` flag.

## Transformation Examples

JSON Input:
```
{
  "event": {
    "name": "Freddie Jones",
    "age": 27,
    "author": {
      "articles": [
        { "id": 0, "title": "The Elements", "length": 150, "published": true},
        { "id": 1, "title": "ARRL Handbook", "length": 1000, "published": true},
        { "id": 2, "title": "The Mars Trilogy", "length": 500, "published": false}
      ]
    }
  }
}
```

Template Example:
```
{
  "author": {
    "name": {{$.event.name}},
    "age": {{$.event.age}},
    "articles": [
{{ range _, x := $.event.author.articles }}
      {
        "id": {{x.id}},
        "title": {{x.title}}
      }
{{ end }}
    ]
  }
}
```
JSON Output:
```
{
  "author": {
    "name": "Freddie Jones",
    "age": 27,
    "articles": [
      {"id": 0, "title": "The Elements"},
      {"id": 1, "title": "ARRL Handbook"},
      {"id": 2, "title": "The Mars Trilogy"}
    ]
  }
}
```

Template Example 2:
```
{
  "author": {
    "name": {{$.event.name}},
    "age": {{$.event.age}},
    "articles": [
{{ range _, x := $.event.author.articles }}
  {{ if x.published }}
      {
        "id": {{x.id}},
        "title": {{x.title}}
      }
  {{ else }}
      null
  {{ end }}
{{ end }}
    ]
  }
}
```

JSON Output 2:
```
{
  "author": {
    "name": "Freddie Jones",
    "age": 27,
    "articles": [
      {"id": 0, "title": "The Elements"},
      {"id": 1, "title": "ARRL Handbook"},
      null
    ]
  }
}
```

## Contributing

Thank you for considering to contribute to `kriti-lang`!

- We use `ormolu` for formatting. The minimum version of ormolu required is `0.3.0.0`.
- Use `GHC` version `8.10.7` or above.
- Use `cabal` version `3.2.0.0` or above.

