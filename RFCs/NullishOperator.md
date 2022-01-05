# Nullish Operator

## Metadata

```
---
authors: Solomon Bothwell <solomon@hasura.io>
discussion: https://github.com/hasura/kriti-lang/issues/34
---
```

## Description

Currently, if a user wishes to substitue a value for `null` they have
to use an `if` statement:

```
{{ if $foo == null }} "hello" {{ else }} $foo {{ end }}
```

This is incredibly verbose. Terser syntax would be really helpful,
especially if we want to improve the capabilities of our string
templates.

If a kriti template references an unbound variable then an exception is thrown:

```
> runKriti "{{ $foo }}" []
Left (RenderedError {_code = InvalidPathCode, _message = "Path Lookup Error: \"$foo\"", _span = Span {start = AlexSourcePos {line = 0, col = 4}, end = AlexSourcePos {line = 0, col = 8}}})
```

JSON schemas are inconsistent. This leads to a reasonable likelyhood
of either a `null` or unbound variable cropping up. Is demonstrated
above, `if` statements can be used to handle `null` values, but there
is no exception catching ability to handle unbound variables.

This RFC proposes a [Nullish
Operator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator)
style operator that will provide a terse form of defaulting behavior
when `null` and will catch exceptions from unbound variables and
return a default value.

## What

`??`, aka `nullish`, is a binary associative operator which returns
its right hand operand if the left hand is `null` or unbound,
otherwise it returns the left hand operand.

I propose we use the same syntax from Javascript:

```
runKriti "{{ null ?? "foo" }}" []
Right "foo"
```

`??` should associate to the left and allow for chaining:

```
runKriti "{{ null ?? null ?? "goodbye" }}" []
Right (String "goodbye")
```

Unlike Javascript, we don't have `undefined` as a first class
value. When unbound values are encountered we throw an
exception. Because we have no exception handling semantics, this
always terminates the program.

`??` should catch any unbound value in its left hand operand and
return its right hand.

```
runKriti "{{ $foo ?? "goodbye" }}" []
Right (String "goodbye")
```

### How

In principle, we could handle this entirely in the parser by converting to an `Iff` term:
```
> parser "$x ?? true"
Right (Iff (Span {start = AlexSourcePos {line = 0, col = 1}, end = AlexSourcePos {line = 0, col = 54}}) (Eq (Span {start = AlexSourcePos {line = 0, col = 7}, end = AlexSourcePos {line = 0, col = 17}}) (Path (Span {start = AlexSourcePos {line = 0, col = 7}, end = AlexSourcePos {line = 0, col = 9}}) [Obj (Span {start = AlexSourcePos {line = 0, col = 7}, end = AlexSourcePos {line = 0, col = 9}}) "$x"]) (Null (Span {start = AlexSourcePos {line = 0, col = 13}, end = AlexSourcePos {line = 0, col = 17}}))) (Boolean (Span {start = AlexSourcePos {line = 0, col = 21}, end = AlexSourcePos {line = 0, col = 25}}) True) (Path (Span {start = AlexSourcePos {line = 0, col = 37}, end = AlexSourcePos {line = 0, col = 45}}) [Obj (Span {start = AlexSourcePos {line = 0, col = 40}, end = AlexSourcePos {line = 0, col = 42}}) "$x"]))
```

However, we would end up with a garbage span and this doesn't address exception catching. It would be better to create a new AST constructor:
```
data ValueExt
  = ...
  | Nullish Span ValueExt ValueExt
```

And then for interpretation we simply do:
```
eval = \case
  ...
  Nullish _ t1 t2 -> do
    case t1 of
      Null _ -> eval t2
      Path _ path -> do
        ctx <- ask
	    case runExceptT (evalPath (J.Object ctx) path) of
		  Left _ -> eval t2
		  Right v1 -> eval v1
      _ -> eval t1

```
### Effects and Interactions

When combined with the proposed Optional Chaining feature we will be
able to do nice expressions like:

```
{{ $body?.bar.baz ?? $bar }}
```

Where we attempt to look up a nested value in `$body` and default to `$bar` if it is unbound or `null`.

### Alternatives

This proposed operated merges the semantics of defaulting and
exception handling. An alternative approach would be to provide a pure
defaulting operator for `null` and a seperate `catch` operation for
path lookups.

The reason I proposed this dual operation is that it more closes
mimics the behavior of the Javascript `nullish` operator where
`undefined` is a first class value rather then an exception. I believe
that attempting to mirror the behavior in Javascript would make the
interface more familiar for the majority of users.

Another alternative for exception handling is to simply get rid of
them all together. Failed path lookups could return a `Null` value.

### Unresolved Questions


### Future Work / Out of Scope

