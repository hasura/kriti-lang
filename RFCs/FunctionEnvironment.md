# Thread Haskell Functions Into The Kriti AST

```
---
authors: Solomon Bothwell <solomon@hasura.io>
discussion: https://github.com/hasura/kriti-lang/pull/49
---
```

## Description


Kriti supports two primitive functions:

```
{{ escapeUri $foo }}
{{ not $bar }}
```

Every time we add a new function we have to hard code it into the
parser, AST, and evaluator, then we must bump the `Kriti` version. If we
add continue to add primitive functions in this way then before long
the Kriti interpreter will become incredibly complex and burdensome to
maintain.

One of the design goals with `Kriti` is simplicity of design and
implementation. We want to keep the base language as small as
possible. If we start adding functions particular to the needs of the
`Hasura GraphQL Engine` (or any other specific consumer of `Kriti`) then
we will violate this design principle.

`Kriti` is an embedded interpreter. We import the interpreter as a
library into other Haskell applications where it is used to transform
JSON values. Because we compile `Kriti` into our application, we have an
interesting opportunity to thread transformations in the meta language
(Haskell) into our `Kriti` interpreter. This would allow users to create
their own ad-hoc library of functions to extend `Kriti` in whatever ways
their application requires.

## How


The entry point to `Kriti` is:

```haskell
type KritiTemplate = T.Text
type JsonContext = [(T.Text, J.Value)]

runKriti :: KritiTemplate -> JsonContext -> Either KritiError J.Value
```

We can easily pass in our `Haskell` functions with a slight tweak to `runKriti`:

```haskell
type KritiTemplate = T.Text
type JsonContext = [(T.Text, J.Value)]
type FunctionContext = [(T.Text, _)]

runKriti :: KritiTemplate -> JsonContext -> FunctionContext -> Either KritiError J.Value
```

The question then becomes, what type do we need for our functions?

The Kriti evaluator has this signature:
```haskell
eval :: ValueExt -> ExceptT EvalError (Reader JsonContext) J.Value
```

If we run our haskell functions in the same monad then they will have
access to the `JsonContext` and we will be able to easily thread them
into the evaluator:

```haskell
_ :: ValueExt -> ExceptT EvalError (Reader JsonContext) ValueExt
```

The key diffeence being we return a `ValueExt` to be further
interpreted.

To make this work we need to extend the Kriti AST:

```haskell
data ValueExt
  = -- | Core Aeson Terms
    Object Span (Compat.Object ValueExt)
  | Array Span (V.Vector ValueExt)
  ...
  | Function Span T.Text ([(T.Text, J.Value)] -> ValueExt -> Either KritiError ValueExt) ValueExt
```

And the evaluator:

```haskell
eval :: ValueExt -> ExceptT EvalError (Reader Ctxt) J.Value
eval = \case
  ...
  Function sp id f t1 -> fmap f $ eval t1
```

The tricky part will be constructing `Function` terms in the
parser. We will need to add the `[(T.Text, [(T.Text, J.Value)] ->
ValueExt -> Either KritiError ValueExt)]` term from `runKriti` and add it to our `ParserState`:

```haskell
data ParserState = ParserState
  { parseSource :: B.ByteString,
    parseInput :: {-# UNPACK #-} AlexInput,
    parseStartCodes :: {-# UNPACK #-} (NE.NonEmpty Int),
    parseSpan :: Span,
	parseFuncCtx :: [(T.Text, [(T.Text, J.Value)] -> ValueExt -> Either KritiError ValueExt)]
  }
```

Then write a new `Alex` production rule for custom functions that does a lookup in `parseFuncCtx`:

```haskell
function :: { ValueExt }
function
  : 'escapeUri' kritiValue { buildFunc EscapeURI (locate $1) $2 }
  | 'not' kritiValue { buildFunc Not (locate $1) $2 }
  | ident kritiValue { Function (locate $1 <> locate $2) $1 (lookupFuncInContext $1) }
```

### Effects and Interactions

This will add additional complexity to the parser but this is a one time cost.

### Unresolved Questions

How do we handle a failed lookup in the parser?

Will this new production rule introduce ambiguity in the grammar?
