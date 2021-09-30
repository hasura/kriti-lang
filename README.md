# Kriti Lang

[![kriti-lang::CI](https://github.com/hasura/kriti-lang/actions/workflows/haskell.yml/badge.svg)](https://github.com/hasura/kriti-lang/actions/workflows/haskell.yml)

A minimal json templating language inspired by Go's template language.

Kriti templates are a superset if JSON with path lookups, if/the/else expressions, loops, and some basic predicate and conditional operators.

Kriti expressions are wrapped in double curly brackets such as `"http://wwww.google.com/{{$body.path}}"`. The `Kriti` evaluator takes `Kriti` template and a set of source json expressions paired with binders then uses them to construct a new json expression.

### Kriti Expressions

#### Path Accessors

Values can be looked up in bound json expressions using the standard path lookup syntax:
```
{{ $.foo.bar[0]['my key'] }}
```
`.` is used to look up object fields and `[x]` is used to lookup array indices and string literal object fields.

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
String Interpolation is currently limited to path lookups:
```
"http://www.{{$.domain}}.com/{{$.path}}"
```

### Library
The library exposes the function `runKriti`, the type definition of the function is:
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

### CLI Tool
The executable is a CLI tool which applices a transformation to a single json file:
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
- Use `GHC` version `8.10.4` or above.
- Use `cabal` version `3.2.0.0` or above.
