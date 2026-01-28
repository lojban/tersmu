# Tags (tense and modal)

`{gi'a ba bo}` etc.

By 10.17.10 and common sense, the tag takes effect only in the case that both connectands hold.

One solution is to put tagged connectives into our logic as connective annotations. But putting BAI in is surely a bad idea.

So perhaps we should take a more direct approach: render it as a disjunct over the possible boolean combinations, with tags ignored except in the TT case, when we handle them as we would without logical part.

So we reduce to handling `{.i bai ja ba bo}` and its ilk.

For these:

- for BAI: add to the left a tag with an event variable for the right;
- for tense: add a tense operator to the right with an event variable for the left.

Actually, there's a better solution than the "TT-skimming" approach above, which is the one that's now implemented:

> broda i ja ba bo brode  
> EX x1. (x1=. broda( ) ∨ (ba)(x1). brode( ))

this works as desired: since EX x1. (ba)(x1). brode() is just equivalent to brode(), there's only a tense condition in the case that both disjuncts hold.

Ugh, but that doesn't work for other tags; e.g. EX x1. (bai)(x1). brode() isn't really equivalent to brode().

Worse:  
EX x1. (x1=. broda( ) --> (ba)(x1). brode( ))  
can be true when broda and brode both occur but in the wrong order, by taking x1 to be an arbitrary event before the event of brode…

So this is crap.

So "TT-skimming" was the right idea after all?
