# Kriti Lang

A minimal json templating language based on Go' template language.

## TODO
- Pretty Printer
- String Interpolation
- Array Membership Parsing
- CLI Interface

## Uses
This library exposes the function `runKriti`, the type definition of the function is:
``` haskell
runKriti :: Text -> [(Text, Value)] -> Either KritiErr Value
```
The first argument of the function is the template JSON, for example, we can use `myTemplate` as the first argument:
``` haskell
myTemplate :: Text
myTemplate =
    "{\
    \   'name': {{x.name.english}},\
    \   'id': {{x.id}},\
    \   'hp': {{x.base.HP}}\
    \}"
```

The second argument is a `list` of `tuple` of `(Text, Value)`. The first element of the tuple is the binding to be used for the JSON object, i.e. for the above template we are using `x` as the JSON binding, so, `x` will bind to the JSON object. The second element of the tuple is of type `Data.Aeson.Value` (can be obtained by `Data.Aeson.decode` method).

The function `runKriti` will return `Either KritiErr Value`. If the parser is successful, then it will return `Right Value`, else it will return `Left KritiErr` which can be used for debugging.

## Run example
To run the example, first clone this repository using the following command:
``` sh
git clone git@github.com:hasura/kriti-lang.git
```
Now, run the following command:
``` sh
cd kriti-lang
cabal new-run example
```
## Examples

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
