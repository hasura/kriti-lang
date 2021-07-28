# GoBasic Lang

A minimal json templating language based on Go's template language.

## TODO
- Template pretty printer
- Better Error Messaging
- What, if any, static typechecking is possible?
- Write this readme
- Port over unit tests from old project
- Many additional unit tests

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
