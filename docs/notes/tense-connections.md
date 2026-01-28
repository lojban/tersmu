# Tenses and modals (tense connections)

Tense tags should clearly scope like quantifiers and negation.

## BAI

For BAI, there are two reasonable possibilities:

- **(i)** behaving identically to tenses (except for the weird difference with connectives)
- **(ii)** behaving like positional arguments to a bridi

(ii) is what CLL seems to suggest.

(i) allows greater expressivity, e.g.

- `ni'i ko'a na broda`
- `na broda .i se ri'a bo brode`
- `pu gau mi ba broda`

I suspect what's really going on is that BAI conflates two grammatical functions; we have properly modal operators, like ni'i and bai, which are propositions about the event/proposition; indeed, the corresponding gismu (often) have places for the event to go in. But we also have BAI like be'i and ka'a, which really do seem to fill the advertised role of adding a missing place to a selbri.

e.g. what could `ve ka'a ko'a na bajra` mean, if not `na ku ve ka'a ko'a bajra`?

For current purposes, I think we should simply go for (i), being the most general option; if the semantics of some BAI end up meaning they commute with other operators, that's not our current business.

The semantics of tense is, in contrast, mostly quite clear, to the extent that we could hope to expand into predications about events; but that's still best done as interpretation of the modal logic.

## ki

ki as a sumtcita is unproblematic.

But e.g. what does `na ku pu ki broda .i brode` mean? Blatant donkey?

If tenses are quantifiers, then ki makes no sense? But some tenses certainly are quantifiers — e.g. `{roroi}`.

Maybe `{ki}` should apply to *all* bridi operators — negation and quantification included? And really function as an afterthought `{zo'u tu'e}`? Pretty drastic.

One alternative is the purely syntactic one — `na ku pu ki broda .i brode` simply means `na ku pu broda .i pu brode`. Is that really the best we can do?

Well… we could give up on the interaction with negation etc, and effectively go for option (ii) above for tenses too (with order of tenses still mattering, natch). Then `{ki}` would just repeat the operator on all future bridi.

I think giving up on `{ki}` would be preferable (it can always be replaced with `{ca ko'a zo'u tu'e}`, after all).

**Cunning alternative:** have `{ki}` with a tense implicitly insert a `{ca zo'e}`; it fixes a constant time, and sets the base time to that. Makes much sense! Similarly with other tags.

More precisely: ki appears at the end of a taglist, indicating what subset of time,space,actuality,jbomodals the ki applies to. Whether sumtcita or bare, it then acts like the underlying taglist, with the additional effect of setting as a reference: if tense, a constant time/space/actuality coincident with the remaining event; if jbomodal sumtcita, the seltcita sumti; if a combination of these two: hrm.

No, that doesn't quite work either. New idea?

## Connectives

For these, we seem forced to add an operator which assigns a variable to an event of a proposition. See this document for details.

(It should be a variable rather than a constant so we can handle things like  
`ro danlu cu jbena gi'e ba bo morsi` ->  
Ax:danlu(_). Ey. (y=. jbena(x) ∧ (ba)(y). morsi(x)))

## Tense grammar

I agree with xorxes that the current complicated grammar of tenses is not reflected in the semantics. So we can reasonably use the zasni gerna tense grammar (which only adds productions, and requires no extra semantics, except for newly legal `{SE [tense]}`).

## Implementation

Add modal operators to the logic:

```haskell
data Prop r t o = ... | Operator o Prop
```

and their lojban instances:

```haskell
data JboOperator
    = Tagged Tag (Maybe JboTerm)
    | WithEventAs JboTerm
data Tag
    = Tag {tagNAI::Bool, tagBase::TagBase}
    | TagJOI Cmavo Tag Tag
    | TagNAhE Cmavo Tag
data tagBase
    = TenseCmavo Cmavo
    | FAhA Bool Cmavo
    | ROI {roiIsSpace::Bool, roiQuantifier::Quantifier}
    | TAhE_ZAhO {taheZoheIsSpace::Bool, taheZahoCmavo::Cmavo}
    | BAI Cmavo
    | FIhO JboPred
```
