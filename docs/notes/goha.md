# GOhA

Trying to make sense of CLL prescription: I can't.

Since we've already dropped CLL's magic selbri NA and selbri tags, things are neater than they might be; in particular we can ignore the "replacement" semantics of 15.8–9.

## Giheks

If GOhA can bind to a compound bridi, we need to decide how a compound bridi can act as a tertau…

**Clue?** `cei` can't get at a compound bridi. So maybe `go'i` shouldn't either? But `cei` does bind terms, which can be connected or quantified, which isn't really different from getting a compound bridi.

## Semantics options

**Simplest:** GOhA binds to a JboRel with some Args. But then any negations and tags are lost, and quantified terms give donkeys.

**Most complicated:** GOhA binds to a Bridi = Args -> JboProp, and we somehow (?) make sense of this acting as tertau.

## Rethinking tanru

Tanru as bridi operators, analogous to other modal operators? Note that xorxes' zasni gerna does allow tertau to be tagged. So we could have `parseTU :: BridiM Bridi`, which would be a relief.

Hmm… but we don't actually want a difference between

- `ro da broda brode` and `broda brode fa ro da`,

so they can't act wholly analogously to tags…

(Consider: `{do xamgu na cladu}`? `{do na(ku) cladu .i xamgu go'i}`? `{do xamgu cladu be naku}`? `{do xamgu cladu naku}`? No, we can't do that.)

So as operators, they **commute** with all other operators. So in fact, this is equivalent to simply having a seltau act on a JboProp by acting on all JboRels in it. So let's do that.
