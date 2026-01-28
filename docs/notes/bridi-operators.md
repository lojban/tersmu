# Bridi operators

*(Notes from when I was first thinking about the July 2014 rewrite; doesn't have much to do with the eventual code.)*

Thinking through handling tailterms led to taking seriously the notion that terms and tags are bridi operators.

## Basic problem

Tailterms are parse-disrespecting monstrosities.

e.g. unclear what to do with

- `{ge ro da zo'u broda da gi ro da zo'u brode da vau da da}`

On the one hand, clearly the final `da` can't be in the scope of either `roda`. On the other hand, we're under fairly clear instructions to equate

- `{ge pu broda gi ca brode vau baku}` with `{ge puba broda gi caba brode}`

## Solution

Parsing a term results in

- `(Prop -> Prop, Maybe Arg, Bindings -> Bindings)`  
  (along with anaphoric bindings, side-assumptions etc).

The Bindings here are purely parse-respecting, and are just for the da-series. Arg is a JboTerm maybe tagged with a numerical FA position (or a modal?). This return value is used as follows:

- **For prenex terms:** the Prop and Bindings maps are applied to remainder; any JboTerm has only its vague "subject" meaning.
- **For pre-briditail terms:** the Prop and Bindings maps are applied to remainder; JboTerms are collected for handing to bridi.
- **For tailterms:** the Prop map and args are applied to the compound bridi, recursing inside; Bindings are applied to remainder of this list of tailterms.

So this gives

- `{ge ro da zo'u broda da gi ro da zo'u brode da vau da da}` ==  
  `{ge ro da su'o de zo'u broda da de de gi ro da su'o de zo'u brode da de de}`

## Way to implement this

```haskell
type BridiOperator = (Prop -> Prop, Maybe Arg)
type CompoundBridi
    = Selbri3 [BridiOperator] Selbri3 [BridiOperator]
    | [BridiOperator] ConBridi Connective CompoundBridi CompoundBridi [BridiOperator]
```

That's what we parse a (sub)sentence to. Once we get to an appropriate level, convert it to a Prop in the obviousish way. Yes, it really does seem to have to be this complicated!

Can we do it more neatly with continuations?

**Further complication:** termsets. But let's not let that worry us unduly.
