# Extend Operator Availability

## Metadata

```
---
authors: Solomon Bothwell <solomon@hasura.io>
discussion: https://github.com/hasura/kriti-lang/issues/38
---
```

## Description

Kriti supports `&&`, `||`, `==`, `>`, and `<` infix operators, but only in `if` statements:

```
{{ if $x > $y }} true {{ else }} false {{ end }}
```

We limited operators to `if` statements initially because we were
unsure of what syntax we would want to give them when used more
generally.

Currently all non-JSON expressions must be wrapped in `{{ .. }}`. If
we were to continue that trend, then operators outside of `if`
statements should look like this:
```
{{ $x > $y }}
```

This is rather verbose and doesn't especially add any clarity. Kriti
is not JSON, Kriti generates JSON.
## What

I propose that we allow the use of operators as top level expressions
and that we drop the curly bracket wrapping. I propose we also drop
the curly bracket requirements for function application.

The following examples become valid syntax:
```
$x > $y
```
```
escapeUri "foo+!"
```
```
escapeUri ($x > $y)
```
```
{ "foo": $x > $y }
```

I also propose we add `>=`, `<=`, `!`, `in` as additional operators
and `not` as an additional function.

`!` is a unary not operator on booleans and `in` checks list
membership and object key membership.

```
!true
```

```
1 in [1, 2, 3]
```

```
"a" in { "a": 1, "b": 2}
```

### How

Eliminating the double curly brackets would allow us to simply remove
the distinction between `values` and `terms` in our parser.

Adding the new operators would be trivial.

### Alternatives

We can carry on with requiring `{{ .. }}` brackets for all non-JSON
terms but I don't think it only serves to make the language more confusing.

We can also continue to limit operators to `if` statements but I think
that is an unecessary handicap on the language. The same goes for not
including the suggested new operators.

### Unresolved Questions

We need to make a decision about requiring curly brackets.

### Future Work / Out of Scope

