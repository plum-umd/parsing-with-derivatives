For a warm up to understand Parser Derivatives, consider reviewing Derivatives
of Regular Expressions as explained by Brzozowski, as well as my implementation
of them.

This implementation does not go into detail of convincing the reader of the
correctness or completeness of the theory behind parser derivates. This
information can be obtained by reading our paper YACC IS DEAD (Might and
Darais) We expect to publish this paper soon.  This does not hinder the
usefulness of examining my implementation for our parsing technique.

*** Intro ***

The discussion hereafter concerns parsing grammars that describe context free
languages.  The technique allows for the parsing of ANY context free language.
Parsers are built using a natural combinator interface which mirrors how one
might describe a grammar in BNF.  The implementation is also remarkably
compact.  This technique is deceptively simple.  

*** The Parser Data Structure ***

The parser combinator functions will build up a parser data structure, not a
parsing function.  The result is a cyclic graph, where each node in the graph
represents a conceptual node in the BNF grammar.  For example, a grammar that
looks like:

<G> = '(' <G> ')' | <F> | epsilon
<F> = '1' | <F> '+' <F> | <F> '*' <F> | <G> | epsilon

will translate into combinators:

parser = g
  where
    g =     terminal '(' <~> g <~> ')' ==> (\ ...)
        <|> f
        <|> epsilon
    f =     terminal '1' ==> (\ ...)
        <|> f <~> terminal '+' <~> f ==> (\ ...)
        <|> f <~> terminal '*' <~> f ==> (\ ...)
        <|> g
        <|> epsilon

which roughly translates into the structure:

parser = g
  where
    g = (Alt (Reduction (Con (Terminal '(') 
                             (Con g (Terminal ')')))
                        (\ ...))
             (Alt f epsilon))
    f = (Alt (Reduction (Terminal '1')
                        (\ ...))
             (Alt (Reduction (Con f (Con (Terminal '+') f))
                             (\ ...))
                  (Alt (Reduction (Con f (Con (Terminal '*') f))
                                  (\ ...))
                       (Alt g epsilon))))

Note that this grammar is left, right, and self recursive, as well as highly
ambiguous.  The Reduction functions have been omitted, because this is where
one would place computation to assemble the results of the parse.  Reductions
are also necessary if we wish to assign Haskell types to these parser
structures.

*** How Parsing Happens ***

Before describing how this parser will consume input and as a result output
something useful, I will first discuss the higher level idea about derivatives
of formal languages.

Every formal language has a set (possibly infinite in size) of "strings" which
are members of that language.  If we take the derivative of a language L with
respect to some character c, we describe a new language, L_c, which contains
the tails of all the strings that were in L that had c as a first element.  For
example, for the language 

L = { "foo", "foobar", "fizz", "bar" }
L_"f" = { "oo", "oobar", "izz" }
L_"b" = { "ar" }
L_"foo" = { "", "bar" }
L_"z" = {}

What our paper describes, and this implementation exploits, is that it is
possible to compute the derivative of any context free language.

Any input string can be checked to see if it is a member of language L by
taking the input string derivative of L and seeing if the empty string exists
in the resulting language.  But we can go much beyond just testing membership.
In order to perform useful computations along the structure of a parse, we must
accumulate some result inside successfully parsed epsilon values.

It should be clear to the reader at this point that parsing is now a
conceptually simple process.  Build a parser data structure (which represents a
language), take derivatives for every character of input, examine the resulting
parser data structure (which also represents a language) for epsilon values.

*** Parsers Combinators and Derivatives ***

Now to more of the details of the implementation.

A Parser is one of the following:
 * Concatination: sequences two parsers
 * Alternation: allows the parser to simultaneously explore two paths
 * Reduction: reduces the result of a parser to another value (and type. this is important)
 * Terminal: consumes a single token, after which it becomes an epsilon
 * Epsilon: produces a result without parsing any input
 * Empty: represents a failed parser, and fails on all future input

The recusive nature of a parser follows: 
  P ::= Con P P
      | Alt P P
      | Red P
      | Ter
      | Eps
      | Emp

Parsers are directed cyclic graphs.  They are data structures that represent an
intermediate state of a parse.  

*** Knot tying technique using memoization ***

The derivative of a parser combinator is defined recursively, making use of the
derivative of child parsers.  When parsers are cyclic, which they typically
are, standard functional recursion will not terminate.

One solution to this is to use pointers, or unique identifiers, for all parsers
in a parser graph.  The derive function could then cache the references to its
results, so when computing the derivative of a node it has already seen, the
knot can be tied by looking up a reference to the known (and computationally in
progress) result.

An implementation of parser derivatives using pointers and the IO monad in
Haskell was created before this one.  This implementation was a big success!
This new implementation is more a result of my spare time dreaming that a more
functional and pure approach could be found. The only need for all the IO
pointer nonsense is to make up for the lack of a good way to write a recursive
function over a cyclic data structure.  Thus, an attempt to do such a thing in
pure, non-IO haskell is motivated.  This parser demonstrates the solution.

Let's imagine a simple parser that works on Char values as input tokens for
simplicity. It might be defined (using GADTS) as:

data Parser a where
  Con :: Parser a1 -> Parser a2 -> Parser (a1, a2)
  Alt :: Parser a  -> Parser a  -> Parser a
  Red :: Parser a  -> (a -> b)  -> Parser b
  Ter ::              Char      -> Parser Char
  Eps ::              Set a     -> Parser a
  Emp ::                           Parser a

The function for computing the derivative might look something like:

derive :: Parser a -> Char -> Parser a
derive (Con p1 p2) c = conDerive (derive p1 c) (derive p2 c)
derive (Alt p1 p2) c = altDerive (derive p1 c) (derive p2 c)
derive (Red p f)   c = redDerive (derive p c) f
derive (Ter t)     c
         | c == t    = Eps (Set.singleton c)
         | otherwise = Emp 
derive (Eps _)     c = Emp
derive Emp         _ = Emp

Obviously, this will not terminate for a cyclic parser. The first step in
solving this is to add a field for the derivative within the structure of the
parser

data Parser a where
  Con :: Parser a1 -> Parser a2 -> {- the derivative field -} (Char -> Parser (a1, a2)) -> Parser (a1, a2)
  Alt :: Parser a  -> Parser a  -> {- the derivative field -} (Char -> Parser a)        -> Parser a
  Red :: Parser a  -> (a -> b)  -> {- the derivative field -} (Char -> Parser b)        -> Parser b
  Ter ::              Char      -> {- the derivative field -} (Char -> Parser Char)     -> Parser Char
  Eps ::              Set a     -> {- the derivative field -} (Char -> Parser a)        -> Parser a
  Emp ::                           {- the derivative field -} (Char -> Parser a)        -> Parser a

For the purposes of this demonstration, imagine there exists a function `memo'
with signature and pseudo-implementation:

memo :: (Char -> b) -> Char -> b
memo f c = if cached c 
             then cachedValueOf c 
             else cacheAndEvaluate c (f c)

Also note that:

f p1 p2 = result
  where
    result = someUseOf result

is equivalent to:

f p1 p2 = fix $ \ result -> someUseOf result

now we define smart constructors:

con p1 p2 = fix $ \ result -> Con p1 p2 (memo (derive result))
alt p1 p2 = fix $ \ result -> Alt p1 p2 (memo (derive result))
red p  f  = fix $ \ result -> Red p  f  (memo (derive result))
ter t     = fix $ \ result -> Ter t     (memo (derive result))
eps s     = fix $ \ result -> Eps s     (memo (derive result))
emp       = fix $ \ result -> Emp       (memo (derive result))

and accessor

parserDeriveFun :: Parser a -> Char -> Parser a
parserDeriveFun (Con _ _ f) = f
parserDeriveFun (Alt _ _ f) = f
parserDeriveFun (Red _ _ f) = f
parserDeriveFun (Ter _   f) = f
parserDeriveFun (Eps _   f) = f
parserDeriveFun (Emp     f) = f

notice that `derive' and `parserDeriveFun' both have the same type.  Now all we
must do is (1) use the smart constructors when constructing new parser values
and (2) use `parserDeriveFun c childParser' instead of `derive c childParser'
inside the derive function, and (3) call `parserDeriveFun' from code that needs
to take the derivative of a parser, rather than `derive'.  The new derive
function would then look like:

derive :: Parser a -> Char -> Parser a
derive (Con p1 p2) c = conDerive (parserDeriveFun p1 c) (parserDeriveFun p2 c)
derive (Alt p1 p2) c = altDerive (parserDeriveFun p1 c) (parserDeriveFun p2 c)
derive (Red p  f)  c = redDerive (parserDeriveFun p c) f
derive (Ter t)     c
         | c == t    = Eps (Set.singleton c)
         | otherwise = Emp 
derive (Eps _)     c = Emp
derive Emp         _ = Emp

you can now convince yourself that the derivative will end up using a cached
derivative of a parser to prevent non-termination.

for an example of how this might work step by step, consider the parser:

xList = alt (eps (Set.singleton ""))
            (red (con (ter 'x')
                      xList)
                 (:))

asking for the derivative of xList with respect to any char c, by calling
(parserDeriveFun xList c), will:

call (parserDeriveFun (Alt ...) c), not in cache, so call derive
.. call (derive (Alt ...) c), caching the result in parserDeriveFun of (Alt ...)
.. .. call (parserDeriveFun (Red ...) c), not in cache, so call derive
.. .. .. call (derive (Red ...) c), caching the result in parserDeriveFun of (Red ...)
.. .. .. .. call (parserDeriveFun (Con ...) c), not in cache, so call derive
.. .. .. .. .. call (derive (Con ...) c), caching the result in parserDeriveFun of (Con ...)
.. .. .. .. .. .. call (parserDeriveFun (Alt ...) c), value exists in the cache, so it gets used
.. .. .. .. .. .. return
.. .. .. .. .. return
.. .. .. .. return
.. .. .. return
.. .. return
.. return
return

in the complete implementation, all values that are calculated recursively are
contained in the data structure PKnot.  The smart constructors (module
DerParser.Builder) use fix to create the PKnot off of the returned value.

***

The other recursive computations in this implementation are

* a fixed point stepper
* freezing fixed point metadata
* weeding empty branches

These functions do not take an extra argument like derive does (in derive this
argument is the token of type Char), and thus they do not need to use `memo'.

By way of example, in the case of weed, the smart constructors would look
something like:

con p1 p2 = fix $ \ result -> Con p1 p2 (KnotData (memo (derive result)) ... (weed result) ...)
alt p1 p2 = fix $ \ result -> Alt p1 p2 (KnotData (memo (derive result)) ... (weed result) ...)
red p  f  = fix $ \ result -> Red p  f  (KnotData (memo (derive result)) ... (weed result) ...)
ter t     = fix $ \ result -> Ter t     (KnotData (memo (derive result)) ... (weed result) ...)
eps s     = fix $ \ result -> Eps s     (KnotData (memo (derive result)) ... (weed result) ...)
emp       = fix $ \ result -> Emp       (KnotData (memo (derive result)) ... (weed result) ...)

*** Fixed point computation ***

This parser implementation uses a novel approach for calculating fix point
values, that relies on the knot tying technique described above.

Every parser contains metadata that is computed from a fixed point computation.
These metadatas are initialized to some `top' boolean value inside an FPMaybe
data structure.

data FPMaybe a = Decided a | Undecided a -- simplified from version in module DerParser.FPMaybe

a single step will leave decided metadata values how they are, and compute a
new undecided value based on the values of its children.  When this step yields
unchanged undecided values, all the values are "frozen", and set to be decided
values.  This follows pretty directly the defenition of a fixed point: calling
a function (in this case `fixMetaStep') over and over until the output
converges on an unchanged result.

*** Data Reification ***

The reader may have noticed that in order to compute whether or not there is a
parser in a parser graph whose metadata has changed, we must be able to iterate
over each parser.  Luckily, thanks to [name], his recent paper "Data
Reification", and hackage package data-reify, a cyclic graph can easily be
flattened into reified form.  Once the graph has been flattened, we can
traverse the finite list of nodes and fold over their values.  Note that using
DataReify is not an option for computing children dependent values like the
derivative because there is no way to reassemble the type information of the
graph.

See src/DerParser.hs to get a high level view of what each of the DerParser.*
modules contain.
