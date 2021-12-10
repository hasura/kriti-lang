# Javascript Style Optional Chaining

## Metadata

```
---
authors: Solomon Bothwell <solomon@hasura.io>
discussion: [Github Issue](https://github.com/hasura/kriti-lang/issues/30)
---
```

## Description

Path lookups to missing object fields and out of bound array indices
result in a runtime error. 

```
> runKriti "{{$foo}}" []
Left (RenderedError {_code = InvalidPathCode, _message = "Path Lookup Error: \"$foo\"", _span = Span {start = AlexSourcePos {line = 0, col = 3}, end = AlexSourcePos {line = 0, col = 7}}})
```

This is good behavior to prevent silent failures at runtime. However,
there are specific cases where users want a nullable path lookup such
as if the field truly is optional and you want to construct certain
data conditionally based on that optionality.

One specific example of this is when adding query parameters to a URL
in a Hasura webhook request transformation. A desired use case is to
conditionally set query params based on fields in the request body.

This RFC proposes to add [javascript style optional
chaining](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining)
for object and array lookups.

## How

I propse a simplfied version of the Javascript Optional Chaining syntax:

```
runKriti "{{ $body?.bar }}" [($body, {"foo": 1})]
Right Null
```

Optional Lookups will shortcircuit at the first error:

```
runKriti "{{ $body?.bar.baz }}" [($body, {"foo": 1})]
Right Null
```


### Effects and Interactions

This will allow use to do a verbose form of defaulting values using if statements:

```
runKriti "{{ if $body?.bar.baz }} {{ $body.bar.baz}} {{ else }} null {{ end }}" [($body, {"foo": 1})]
Right Null
```

Which could be used to solve the query param request transformation issue mentioned in the intro. At a later date we can develop more terse defaulting operator, perhaps based on the Javascript [Nullish Operator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator).

### Alternatives

As an alternative to introducing a new operator, we could change the
before of the standard path lookup operator or even allow the
semantics to be set via a configuration option. I am opposed to both
of these options as there are common scenarios where you may want a
runtime error and where you may want an optional value.


### Unresolved Questions

How do we handle lookup attempts on non object terms? Should they throw an error or return `null`?

```
runKriti "{{ $body?.foo }}" [($body, 1)]
Left (RenderedError {_code = InvalidPathCode, _message = "Path Lookup Error: \"$foo\"", _span = Span {start = AlexSourcePos {line = 0, col = 3}, end = AlexSourcePos {line = 0, col = 7}}})
```

### Future Work / Out of Scope

As mentioned earlier, a Nullish style operator would be a great further addition.
