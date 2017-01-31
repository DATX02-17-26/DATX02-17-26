# Code Style

This document discusses the code style used in this project. It is mainly focused on the style to use for haskell code as it is the primary language used within the project.

It is partly based on [tibbes haskell-style-guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

GitHub does not always render code correctly, so check the raw file instead.

## American vs. British

Only english is used.

American english should be used, i.e:
+ `color` is good, `colour` is bad.
+ `normalize` is good, `normalise` is bad.

## Line length

Use `<= 80` characters per line.

## Indentation

**`2` spaces should be used for indentation everywhere.**

Tabs and `4` spaces should never be used, except for places where it must be used.

Sometimes other amounts of spaces may be used when it makes sense for readability, as specified in the rest of this document.

## Blank Lines

One blank line between top-level definitions. No blank lines between type signatures and function definitions. Add one blank line between functions in a type class instance declaration if the function bodies are large. Use your judgement.

## Whitespace

Surround binary and unary operators with a single space on both sides. Don't insert a space after a lambda.

This is good:
```haskell
top_func1 :: SomeType
top_func1 = myfunc $ a + b * c * (d + f)

top_func2 :: SomeOtherType
top_func2 = fmap (\a -> a)
```

This is bad:
```haskell
top_func1 :: SomeType
top_func1 = myfunc$a+b*c*(d+f)

top_func2 :: SomeOtherType
top_func2 = fmap ( \ a -> a)
```

## Parentheses and `$`

Prefer using `$` to avoid parentheses wherever possible unless you have to use extra function applications to be able to do so.

## Naming

### Types

We use the standard haskell capital camel case styling where words are capitalised. `3` words should at most be used. Always use conceptually relevant names.

For readability reasons, don't capitalize all letters when using an abbreviation. For example, write `HttpServer` instead of `HTTPServer`. Exception: Two letter abbreviations, e.g. `IO`.

### Functions

Use lower camel case for naming functions.

### local variables

When dealing with destructuring lists, the head and tail should be named `(x:xs)`, i.e: `(something:somethings)`.

If it is obvious (use your better judgement) from the context of the function `1` and `2` character names can be used such as `i, v, l, r, e`. Prefer such characters that are mnemonics of a longer name as in: `i(dentifier/integer)`, `v(ariable)`, `l(left)`, `r(ight)`, `e(xpression)`. Do not pick them arbitrarily.

Otherwise, use local names that make the code as self documenting as possible.

Words in local variables should follow the same style as functions.

## Modules

Use singular when naming modules e.g. use `Data.Map` and `Data.ByteString.Internal` instead of `Data.Maps` and `Data.ByteString.Internals`.

### Export Lists

Only export API functions, never helpers.

Format export lists as follows:

```haskell
module Data.Set (
    -- * The @Set@ type
    Set
  , empty
  , singleton

    -- * Querying
  , member
  ) where
```

## Imports

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports. The imports in each group should be sorted alphabetically, by module name.

Always use explicit import lists or qualified imports for standard and third party libraries. This makes the code more robust against changes in these libraries. Exception: The Prelude.

## Data declarations

Align the constructors in a data type definition. Example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader
  deriving (Eq, Show)
```

Format records as follows:

```haskell
data Person = Person
  { _firstName :: !String  -- ^ First name
  , _lastName  :: !String  -- ^ Last name
  , _age       :: !Int     -- ^ Age
  } deriving (Eq, Show)
```

Or:

```haskell
data Expr
  = ELit {
      _elit :: Integer -- ^ Integer literal
    }
  | EVar {
      _evar :: Ident -- ^ Variable identifier
    }
  | EPlus {
      _eleft  :: Expr -- ^ Left expression
    , _eright :: Expr -- ^ Left expression
    }
  deriving (Eq, Show)
```

Prefer records over not using records, and prefix (function) names with `_`.
This makes the generation of lenses and prisms automatable.

### `data` and `newtype`

Use `newtype` wherever possible to enable optimizations.

## List Declarations

Align the elements in the list. Example:

```haskell
exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

Optionally, you can skip the first newline. Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

## Pragmas

Put pragmas immediately before the function signature of the function they apply to, but after any haddock documentation for that function. Example:

```haskell
-- | The identity function.
{-# INLINE id #-}
id :: a -> a
id x = x
```

In the case of data type definitions you must put the pragma before the type it applies to. Example:

```haskell
data Array e = Array
  {-# UNPACK #-} !Int
  !ByteArray
```

## Type signatures

**Always put type signatures on all top level functions** in a module. It is fine to not do this in code in feature branches, but not in non-feature branches such as: `dev`, `master`, etc.

### Generality

When writing general utility functions, use the **most general type signature** - which can be inferred using `ghci`.

When writing DSLs, it is often better to restrict to the conceptually correct type. For DSL helpers, it might be better to use the most general type for reusability purposes - that decision is up to the individual programmer.

### Indentation

When a type signature fits within the `80` character limit, put it on one line.

`ffmap :: Functor f => f a -> (a -> f b) -> f b`

Otherwise, you should indent to the typing judgement `::` as follows:

```haskell
myfunc :: (Ord a, Monad f, MyOtherConstraint (f a), ...)
       => (Expr -> Expr -> Expr)
       -> Expr -> Expr
       -> Expr
```

Here, the constraints, the resulting type, HOFs have all been put on their own lines.

Functions in higher order functions (HOF) should **not** be broken into several lines, like above, unless it exceeds `80` lines. If it does, indent as follows:

```haskell
myfunc :: (MyLongType -> OtherLongType -> ThirdLongType -> FourthLongType ->
           FifthLongType)
       -> SomeInput
       -> Output
```

This indicates that they belong to a "unit". This also applies to constraints. You should in general not have to do this very often. However, it might be better to create a `type` alias for the function or constraint.

## `where` clauses and `let` bindings

`let` and `where` clauses should in general be **avoided** unless you have a good reason to use them. Instead of using them for helpers, put the helpers as top level functions and export `API` functions via the module system. This helps `unit testing` since `where` clauses and `let` bindings can not be tested at all.

Legitimate uses of `let` and `where` include the following:

+ you repeat an expression many times
  - in many **different patterns** and you do not want to repeat yourself (**DRY**).
  - as a **sub expression** of a larger one you DRY and you also want haskell to **optimize performance** by causing haskell to not evaluate expressions try (call by need vs. call by value).
+ you want to avoid exceeding `80` characters (only applies to let).

When you use `where` clauses, they should be indented as follows:

```haskell
myfunc :: TheSignature
myfunc a b c = some_expr
  where n1    = some_local_expr
        name2 = another_local_expr
```

And `let` bindings should be indented as follows:

```haskell
myfunc :: TheSignature
myfunc a b c = let left  = some_local_expr
                   right = another_local_expr
               in  some_expr
```

## if-then-else clauses

If in monadic code, prefer guard functions if possible.

Prefer `if c then ei else ee` over pattern matching on `Bool` typed expressions.

If the expression can be put on a single line, do that. Otherwise, indent as follows:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, you should be consistent with the 2-spaces indent rule, and the then and the else keyword should be aligned. Examples:

```haskell
foo = do
  someCode
  if condition
    then someMoreCode
    else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
  then doSomethingSilly
  else someOtherCode
```

## Pattern matching

Prefer `case` expressions for pattern matching instead of using pattern matching in the top of functions. This follows the DRY principle.

```haskell
data Expr = Var  { ident :: Ident }
          | Lit  { lit   :: Int }
          | Plus { left  :: Expr, right :: Expr }
```

```haskell
-- good:
eval :: MyType
eval e = case e of
  Var  v   -> exp1
  Lit  i   -> exp2
  Plus l r -> exp3

-- bad:
eval :: MyType
eval (Var v)    = exp1
eval (Lit i)    = exp2
eval (Plus l r) = exp3
```

One exception applies to destructuring lists and tuples:

```haskell
-- good:
listfun (x:xs) = exp1
tuplefun (name1, name2) = exp1

-- bad:
listfun l = case l of
  (x:xs) -> exp1
tuplefun pair = case pair of
  (name1, name2) -> exp1
```

If you can eta-reduce your `case` expression, use `LambdaCase` as follows:

```haskell
eval :: MyType
eval = \case
  Var  v   -> exp1
  Lit  i   -> exp2
  Plus l r -> exp3
```

Align variables and the `->` arrows when it helps readability.

Never use `case` in `case` - split things up to smaller functions instead.

When it comes to pattern matching on `Maybe` and `Either`s, use the functions `maybe` and `either`.

## Hanging Lambdas

You may or may not indent the code following a "hanging" lambda. Use your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
        putStrLn "Here comes a number!"
        print n
```

```haskell
foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

## Monadic code

Do not use monadic operators and do notation if all you do can be restricted to `Functor` and `Applicative` logic.

Prefer not using do-notation over using it, but if this makes you more comfortable, using do-notation is fine.
Never use nested do blocks, instead split things up into smaller functions.

If you can avoid `>>=` and lambdas, do that and instead use operators like: `<$>`, `<*>`, `*>`, `<*`, `>=>`.

Prefer using `<$>` and `<*>` instead of the `liftM2/3/4/...` functions.

## Point-free style

Prefer point-free style, but do it with care.
Always eta-reduce functions and lambdas unless there is an explicit reason not to.

Using `>=>` can help in making monadic code point-free.

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

If you ever have to use `flip`, this is a clear indication that you have over-used point-free style.

## Partial and total functions

All made and used functions must be total. Never use partial functions, this includes infinite loops and stack overflows.

+ Exception: where it makes sense, `main` can live forever.
+ Exception: if you have previously locally verified that the constraints satisfied for the application of a partial function (such as `head`) to a specific argument will be total, then it is fine to use that partial function for that specific argument.

## Extensions

### Forbidden

Never use any of the following extensions: [http://dev.stephendiehl.com/hask/#the-dangerous](http://dev.stephendiehl.com/hask/#the-dangerous).

### Allowed

All others are fine unless they syntactically violate this style guide.

Syntactic extensions such as `LambdaCase` and `TupleSections` can be used even if they are not strictly needed.

## Comments

### Punctuation

Write proper sentences; start with a capital letter and use proper punctuation.

### Top-Level Definitions

Comment every top level function (particularly exported functions), and provide a type signature; use Haddock syntax in the comments. Comment every exported data type. Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
  { age  :: !Int     -- ^ Age
  , name :: !String  -- ^ First name
  }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
  { -- | This is a very very very long comment that is split over
    -- multiple lines.
    field1 :: !Text

    -- | This is a second very very very long comment that is split
    -- over multiple lines.
  , field2 :: !Int
  }
```

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces. Align comments for data type definitions. Some examples:

```haskell
data Parser = Parser
  !Int         -- Current position
  !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where salt = 453645243  -- Magic hash salt.
```

### Links

Use in-line links economically. You are encouraged to add links for API names. It is not necessary to add links for all API names in a Haddock comment. We therefore recommend adding a link to an API name if:

    The user might actually want to click on it for more information (in your judgment), and

    Only for the first occurrence of each API name in the comment (don't bother repeating a link)

## Warnings

Code should be compilable with `-Wall -Werror`. There should be no warnings.