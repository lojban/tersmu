# Sides (noi clauses)

Purpose of noi is twofold: to make additional assertions which don't fit neatly into the main text or which have different illocutionary force, and to give hints to the glorker.

## Simple handling

`mi noi broda` adds `mi broda` as a side-assertion.

(Really though, it ought to add `mi goi ko'a broda` as a side-assertion, and replace the `{mi}` in the main text with `{ko'a}`.)

If the subclause references externally bound variables, e.g.  
`ro lo tercange cu darxi mi noi xebni ri`,  
then things get interesting.

In that case, the natural side-assertion is `mi xebni ro lo tercange` (where in reality we'd bind `{lo tercange}` to a ko'a and use that), and in `ci lo tercange cu darxi mi noi xebni ri` we might be tempted to go for something like `mi xebni lo ci tercange poi darxi mi`; but consider `naku ci lo tercange cu darxi mi noi xebni ri`.

So it seems the only sensible rule is that **externally bound variables always get universally quantified in the side-assertion.**

Is the appropriate domain for that quantification always so easily determined?

How about `by a cy vu'o goi ko'a darxi mi noi xebni ko'a`? To parallel the above, we should universally quantify over the finite domain {by,cy}.

Note we're going to introduce a distinction between `ro da poi broda […]` and `ro da zo'u ganai da broda gi […]`, but so be it. We think of poi clauses et al as specifying the *sort* of the variable.

How about: `by a cy vu'o goi ko'a ro tercange pe ko'a darxi mi noi xebni xy`? The universal quantification rule has to be recursive — any externally bound variables in the JboPred which specifies a domain must themselves be universally quantified.

Consider `lo ka darxi mi noi xebni ce'u`. First note that an implicit ce'u needs to be added, the reference within the noi clause not counting. The side-assertion has to be `mi xebni roda`.

Now consider `ro tercange poi mi noi xebni ke'axire se darxi`. Possibilities:

- (i) mi xebni ro da poi ge tercange gi darxi mi
- (ii) mi xebni ro da poi tercange
- (iii) mi xebni ro da

(i) probably isn't appropriate; consider `ro tercange poi mi darxi lo ke'a xasli cu fengu`, where the domain of the function had better be at least ro tercange.

(ii) is probably correct.

noi clauses applied to variables must, I think, be considered donkey errors. It effectively happens all the time with CLL-lo, but doesn't make sense. Alternatively, they could universally quantify over the variable, e.g. `xu do darxi su'o xasli noi xebni do` could give `ro xasli cu xebni do`; but I think making it an error is preferable.

## Actually

I've changed my mind; externally bound variables in a noi clause should be donkey errors. I'm not sure about lo below, though.

## zo'e and lo

Recall that xorxes at some point agreed that lo terms with externally bound variables should refer to functions of those variables.

Given the "lo broda == zo'e noi broda" dogma, we should view this as being a generalisation of the above noi handling, where the focus of the side-sentence is allowed to be a function of the universally quantified variables.

Does this affect only zo'e? Could we want the referent of `{mi}` to be variable iff it has a noi clause with externally bound variables? Is it actually a good idea even with zo'e?

Only way I can see to render it in lojban is with subscripted ko'a — `ci da broda lo brode be da` -> `ro da zo'u zo'e goi ko'a xi da brode da .i ci da broda ko'a xi da`, pretending that `{xi da}` parses, which it doesn't. Not good. Marginally better: `ci da poi brodi cu broda lo brode be da` -> `zo'e goi fy zo'u ro da poi brodi zo'u li ma'o fy mo'e da brode da .i ci da zo'u da broda li ma'o fy mo'e da`

I think for a first implementation we should ignore all this, and just universally quantify as above. Actually, for a first first implementation we could just treat externally bound variables as donkey errors.

## Miscellany

I guess we can't give `ro da broda pa de noi brode da` the "obvious" meaning.

Makkai's FOLDS?

Could we use the same mechanism to give a plausible handling of strong donkeys — universally quantify over the domain of the donkey variable?  
`ro tercange poi ponse su'o xasli cu darxi ri` -> `ro tercange poi ponse su'o xasli cu darxi ro xasli`? Not exactly intuitive…

**voha:** donkey vo'a should behave the same way as other donkeys, e.g. in `ko'a .e ko'a broda lo brode be vo'a`

## Implementation

We have some extra information to go in ParseState:

- a list of side-sentences (with their own illocutions)
- a JboPred for the domain of each variable, to be specified when we request the variable with getFreshVar. Can be `{ga du abu gi du by}` in the finite case.

How to handle the relative vars, I'm not currently sure…

Then we run the incidental in a fresh BridiParseState, and whenever we reference an unbound variable, we note the dependency. We should also do this when we put assign the domain to the variable in the first place, associating deps. So we get a tree of deps which we flatten, giving a chain of universal quantifiers to prepend to the side-sentence. Finally, we add the side-sentence.

For xorlo: new JboTerm constructor, Constant Int; a Description returns a new one of those. So, really, ought each instance of a personal pro-sumti.

le/voi: make it an illocution on the side-sentence?

Should our side-sentences be JboProps or JboPreds? The latter fits with the idea of giving discourse referents with the pred hinting to a referent; but perhaps it's unlojbanic to focus on the subject like this? e.g. `{zo'e zo'e noi ri mamta cu prami}` is entirely reasonable. The ability to use `{noi}` to introduce questions or commands also suggests that they're more props than preds.
