```
Exercise 4.20 In the language of this section, all variables are mutable, as they are in Scheme. Anotehr alternative is to allow both mutable and immutable variable bindings:

  ExpVal = Int + Bool + Proc
  DenVal = Ref(ExpVal) + ExpVal

Variable assignement should work only when the variable to be assigned to has a mutable binding. Dereferencing occurs implicitly when the denoted value is a reference. 

Modify the language of this section so that let introduces immutable variables, as before, but mutable variables are introduced by a letmutable expression, with syntax given by

  Expression ::= letmutable Identifier = Expression in Expression
```

# Extended Mutable/Immutable Variable Language

This project extends the core language from Section 4 to support both mutable and immutable variable bindings. Inspired by Scheme's default mutability, we introduce a more nuanced design in which:

- **Immutable variables** are declared using `let`
- **Mutable variables** are declared using a new construct: `letmutable`

## Motivation

In the base language, all variables are mutable by default. However, some languages distinguish between mutable and immutable bindings for better reasoning, performance, and safety. This extension adds explicit support for both kinds of bindings:

- Immutable variables provide functional purity and predictability.
- Mutable variables allow imperative programming where necessary.

## Syntax

We extend the expression syntax with a new `letmutable` form:

```bnf
Expression ::= ...
             | let Identifier = Expression in Expression        ; immutable binding
             | letmutable Identifier = Expression in Expression ; mutable binding
```

## Semantic Domains

```
ExpVal = Int + Bool + Proc
DenVal = Ref(ExpVal) + ExpVal
```

- `Ref(ExpVal)` represents a mutable reference.
- `ExpVal` without a reference represents an immutable binding.

## Operational Semantics

- `let x = e1 in e2`:
  - `e1` is evaluated to an `ExpVal`
  - The environment maps `x` to that `ExpVal` (immutable).

- `letmutable x = e1 in e2`:
  - `e1` is evaluated to an `ExpVal`.
  - A reference cell is created to store the value.
  - The environment maps `x` to `Ref(Expval)` (mutable).

- Assignment (`set x = e`)  
  - Permitted only when `x` is bound to a `Ref`.
  - The new value overwrites the contents of the reference.
  - If `x` is immutable, an error is raised. 

- Dereferecing is implicit. 
  - When a variable is looked up, and the value if a reference, its contenst are returned.

## Examples

```
let x = 10 in
letmutable y = 20 in
  begin set y = 30;
        -(0,-(-(0,x),y))
  end
```

- `x` is immutable, bound directly to 10.
- `y` is mutable, initially 20, updated to 30.
- `x+y` evaluates to `10+30=40`.

```
let x = 10 in
letmutable y = 20 in
  begin set x = 30;
        -(0,-(-(0,x),y))
  end
```

- `Cannot set immutable variable: x`

```
let x = 10 in
letmutable y = 20 in
let p = proc(z) set z = 30 in
  begin (p y);
        -(0,-(-(0,x),y))
  end
```

- `10+20=30`

```
let x = 10 in
letmutable y = 20 in
let p = proc(z) set z = 30 in
  begin (p x);
        -(0,-(-(0,x),y))
  end
```

- `10+20=30`

## Implementation Notes

- The environment maps identifiers to `DenVal`.
- Evaluating a variable fetches the `DenVal`, dereferencing if needed.
- `set` checks for `Ref` and updates the referenced cell.
- Haskell stack commands
  - `stack ghci ch4:exe:bothref-exe`
  - `stack build ch4:exe:bothref-exe`
  - `stack test ch4:test:bothrefslang-test`
  - `stack run bothref-exe .\app\bothrefslang\examples\both1.bothref`
  - `stack run bothref-exe .\app\bothrefslang\examples\both2_error.bothref`
  - `stack run bothref-exe .\app\bothrefslang\examples\both3_proc.bothref`
  - `stack run bothref-exe .\app\bothrefslang\examples\both4_proc.bothref`


## Future work

- Type-checking (if present) should ensure `set` is only used with mutable variables.
- Add type annotations distinguishing mutable and immutable bindings.
- Introduce `const` or `var`-style syntax for convenience.
- Support for mutable structures beyond variables (e.g., arrays or records).
