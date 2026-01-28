# Design notes

**First goal:** handle the "first order fragment" of lojban.

> *Note to reader: what follows is largely stream-of-consciousness, extended over many months. Don't expect consistency, do expect tangents.*

---

## Handling quantifiers

- Binding happens where a prenex is or could be, i.e. at `statement` and at `subsentence`.
- An unbound `da` gets exported to the innermost such prenex, and binds just within the corresponding statement/subsentence.
- Handling exportation may be a little complicated… for rather similar reasons that handling `ri` et al will be — the semantics of lojban is partly a matter of the partial order of the parse tree, but partly of the (cross-level) total order of occurrence in the text.
- This actually fits the State Monad approach quite well.

## Naive algorithm

- (Using a monad,) build up a prenex as we parse a statement/subsentence. Every time we hit a currently unbound `da`, or a `naku`, or a tense/modal, we add it to the end of the prenex.
- **Problem:** it doesn't correctly handle the interaction between sumti connectives and negation — CLL explicitly states (16.12.10–11) that `{ge xy gi zy na ku}` == `{na ku ga xy gi zy}`. Presumably tenses and unbound `da` should be handled analogously.

## Idea

Treat sumti connectives just like quantifiers:

- `x .e y broda` == ∀z ∈ {x,y}. z broda == z=(x and y). z broda (similarly)
- `x .o y broda` == z=(x iff y). z broda
- Semantics the obvious ones. We could even add such quantifiers to handle termsets. So now all is clear.

We have:

```haskell
data QT = Arglist [Obj]
      | Negation QT
      | Quantifier Number (Obj -> QT)
      | Connective TruthTable (Obj -> Obj -> QT)
      | Tag Tag Object QT
type QuantifiedTerms = QT
data TermCxt = {QT::QT, lastFA::Nat}
Term :: TermCxt -> TermCxt
```

Actually, this is useless: can't even handle `{A .a B .e C .a bo D}`.

**Instead:** have a `ComplexSumti` type, and have `term` initially return that; then make exporting to the prenex a step which comes later. So the initial parsing returns something closely tied to the lojban, and the hard work converting that to logic is done separately.

**Remaining complexities:**

- `{su'o broda be da zo'u}` -> `{da de poi broda da zo'u}`?
- Tenses and modals are of quantificatory nature (sometimes inherently (ta'e, ro roi), and always if JA-connected), so should go in the prenex.

## Handling sumti

CLL seems to imply (ignoring masses):

- A sumti is a constant, and refers to one or more individuals (`constant` can't be taken too literally — e.g. we may have a quantified variable as a linkarg in a description sumti).
- `poi` narrows the referent-set of a sumti.
- Sumti always appear quantified.
- We can consider bound `{da}` as special — it's a singular variable, so quantifiers on it are meaningless.

**xorlo's modification** (ignoring plural predication and Kinds etc):

- `lo` has no default quantifier, and when unquantified is magic — it acts like it has a universal quantifier with tightest scope.
- Moreover, `{lo broda}` refers to only *some* individuals which broda, not all. So it acts much like `{le}` does, but with assertive power.

### Handling this in logic

**Naive approach:** translate to monadic second order:

- `{le broda goi ko'a cu brode ko'a}` -> EX X. (describe(FA x∈X. broda(x)) ∧ FA x∈X. FA y∈X. brode(x,y))
- `{lo broda goi ko'a cu brode ko'a}` -> EX X. (FA x. (broda(x) <-> x∈X) ∧ EX x∈X. FA y∈X. brode(x,y))

But no need for second-order quantifiers really:

- `{le broda goi ko'a cu brode ko'a}` -> FA x:(describe(broda(x)). FA y:(describe(broda(y)). brode(x,y)
- `{lo broda goi ko'a cu brode ko'a}` -> EX x:(broda(x)). FA y:(broda(y)). brode(x,y)

So we analyse e.g. `{ko'a}` as binding to a unary predicate rather than the corresponding set.

**Issues:**

- The predicate might involve a bound variable — in which case, anaphora to the sumti should simply be disallowed, or better, allowed only within the scope of the quantifier.
- The same description might have different referents on different uses, whereas ko'a must bind to a single choice of referents. So maybe second-order quantification is necessary for `{le}` and `{voi}` after all. So probably also for xorlo. Also for `{la}`. Given which… is it actually worth the bother to handle `{lo}` differently? Wouldn't it be better to translate to monadic 2nd order, then have some proof theory which will do the reduction to 1st order if we want it?

**Alternatively:**

- Nothing 2nd order. `{le broda}` and `{la .brod.}` are considered *absolute* constants. `{ro da le broda be da cu brode}` is not allowed, or is equiv to `{ro da le broda be de cu brode}`.
- Getting goi working will be painful and monadic.

*Remark:* if you think that using xorban style plural predication will make any of the above problems go away, think again, recalling that we still need to be able to quantify over atoms.

### Plural semantics

Even sticking to CLL, some things seem to make a logic with plural terms and variables more natural. The closest things we have to basic terms — personal prosumti and le/la-clauses — are plural, and at least many anaphoric sumti are plural.

So maybe we should translate to a plural logic, and from there to FOL by distributivity if we want? So it seems our intermediate logic would just be FOL with plural terms and an among relation? No need even for plural quantifiers? So we can actually just use FOL, with some unusual terms.

## zo'e

- Translating `{lo broda}` to `{zo'e noi broda}`, and `{le}` fa'u `{voi}`, seems better than considering it as a constant. So we should add zo'e to our logic?
- Can we just consider it as introducing an existential plural quantifier? `{ro da zo'e}` == `{zo'e ro da}` but `{da zo'e noi broda da}` != `{zo'e noi broda da ku'o da}`.
- Examples: `{zo'e se fetsi ro mamta}` == `{ro mamta cu fetsi zo'e}` == `{ro mamta cu fetsi da}`. So why not just have it as an *innermost* existential quantifier?
- To handle the 'obvious referents' interpretation of `{zo'e}` we should interpret `{zo'e}` as an existential quantifier with tightest scope over a varying glorked domain. e.g. `{ro da broda zo'e}` -> ∀x. ∃y∈C_x. broda(x,y).
- With this definition of `{zo'e}`, `{lo}` == `{zo'e noi}` seems to explain many uses of `{lo}`. Often `{lo}` gives a Skolem function (C_x being a singleton), but not always.
- With multiple `{zo'e}`s, the domains have to be glorked simultaneously before any existential quantification — i.e. ∃(x,y)∈C, not ∃x∈C. ∃y∈C'_x.
- This leaves only generics and kinds to deal with. Preference: *not* allow `{lo}` to get at either — require `{lo'e}` for generics, and `{lo ka}` in place of kinds. Alternatively, we could allow \Gen in place of \Exists as a possible quantifier for `{zo'e}`.
- Note that unfilled places often do seem to be generics — e.g. adjectival uses of predicates, like `{ko'a sutra}` or `{ko'a jdari}`.

## Handling descriptions and zo'e without really handling them

- Just leave them as unanalysed terms. The terms will have structure which can be used in further analysis, but we consider this beyond the basic logic we deal with here.
- Note that the structure will sometimes mention bound variables, e.g. in `{ro da lo broda be da cu brode}`, but that's fine.
- Quantifiers we can handle by quantifying the "among [the term]" predicate. Relative clauses have I guess to be considered part of the term.

## Two-stage interpretation

If we're willing to accept `{lo}` == `{zo'e noi}`:

**First stage:** everything is reduced to predications, in particular worlds, of (possibly complicated) selbri with sumti being named entities or personal prosumti or bound variables or zo'e expressions (zo'e + relative clauses). I.e. we obtain something of type `Prop Selbri JboTerm`, where JboTerm has just those four constructors. Anaphora are resolved at this level. For zo'e expressions, anaphora bind "in intension" — to the zo'e clause as a whole; they therefore make sense anywhere in the scope of any variables which occur bound in the zo'e expression. (So e.g. `{ro te cange poi ponse lo xasli cu darxi ri}` doesn't really work — it becomes equivalent to `{ro te cange poi ponse lo xasli cu darxi lo xasli}`. But nevermind, at least for now.) The result of this first stage should be quite human-readable.

**Second stage:** zo'e expressions are replaced by existential/generic quantifiers as appropriate, with domains to be glorked, or (perhaps) by property predications. The result may not be so readable. Interpreting the final expression in a model (as part of a discourse) is potentially rather complicated, due to the glorking required. But we can null-glork, letting the domain always be the whole universe.

## Quantified zo'e and relative clauses

Do the clauses apply to the zo'e itself or to atoms below it?

- `ro lo broda poi brode` -> (ro (zo'e noi broda)) zi'e poi brode
- but `ro mi poi broda` -> (ro mi) poi broda

We could have this be a difference between poi and noi: poi takes only singular variables. `{lo broda poi brode}` is a type error.

*xorxes on this issue:* we still have a way to differentiate both possibilities, at least for the "lo broda ku" type of sumti (not for "ko'a"), because there are three different points where a relative clause may be attached: "lo (1) broda (2) ku (3)". When attached at (2) the restriction occurs before singular quantification over referents; when attached at (3) it occurs as a restriction on the singular quantifier. One problem is that (2) is the "normal" place of attachment (elided "ku") and (3) requires an explicit "ku". But in many cases both meanings will agree anyway.

## Kinds/properties

xorxes wants kinds in the universe, distinct from properties, with `{lo broda}` (and hence `{zo'e noi broda}`) able to get a kind. Considered evil for various reasons.

We can get approximately the same effect by making `{lo broda}` be ambiguous between `{zo'e noi broda}` and `{zo'e noi ka broda}`, while keeping zo'e sane. Current favoured solution: `{zo'e noi}` as close-scope plural existential with glorked domain; `{lo}` as ambiguous between `{zo'e noi}`, `{pa ka}` and `{lo'e}` (lo'e about generics, semantics TBD). In reality, favour dropping the second two options so `{lo}` == `{zo'e noi}`, and generics and properties have to be marked as such. We could then go back to using `{lo ka}` rather than `{pa ka}` without ambiguity. But maybe think through the tense system and generics before making firm decisions.

Note: if unfilled places can be generics, this would mean doing away with the "unfilled place == zo'e" rule, which would be painful. Alternative: declare generic unfilled places erroneous — e.g. say it should be `{ko'a selkuclykai}`, or `{ko'a zilselkucli}`, rather than `{ko'a se kucli}`.

## Notes on lambdas

- Currently implementing lambda expressions directly in Haskell, as function types. In FOL we use `(Int -> Prop)` rather than `(Term -> Prop)`; in Lojban.hs we use `(JboTerm -> JboProp)`. The use of Int in the former was to make interpretation possible.
- Problem with using Int for the latter: we actually do want to apply them to JboTerms. We run into problems when interpreting JboProps (e.g. when interpreting tanru).
- **Solution:** introduce a constructor to Prop: `LambdaApp (Int -> Prop r t) t`, interpreted in a model in the obvious way — it just adds a binding. A monad allowing us to neatly bind a new variable would be very helpful.

## The big idea

We reduce the problem of understanding (a fragment of) lojban to that of understanding (i) first-order logic, (ii) brivla and cmene and non-anaphoric pro-sumti, (iii) tanru and NU-selbri, (iv) zo'e-terms. More precisely, we interpret a lojban sentence as a proposition in first-order logic with (ii)–(iv) as the terms and relations. A zo'e-term corresponds to an expression of the form `{zo'e noi broda}` (or just `{zo'e}`); it has the corresponding unary predicate (or nothing) as data.

## Handling 'statement'

Surprisingly painful! We want to hold to the rule that we raise quantification and sumti connectives to the prenex… so we want to process 'linearly', holding on to bindings as we go… so we can't (monadlessly, at least) just recurse on the structure of the afterthought connected sentences… **Eventual realisation:** continuations solve the problem. We could wrap them in a monad, but not convinced that would be clarificatory.

## Rethinking anaphora

The idea that `{lo broda ri}` equivalent to `{lo broda lo broda}` is flawed — consider e.g. `{mi cpacu lo plise gi'e citka ri}`. So anaphora are going to let us distinguish between an approach to `{lo}` using kinds and one which strips them out, unless the latter has some bizarre rules. e.g. what does `{lo remna cu prami ri}` mean?

## loi

Can be the (mundane) iota operator.

## jo'u vs joi

`{jo'u}` could be like `{.e}`, but expanded after everything else. I.e. `{ko'a jo'u ko'e da broda}` == `{da ko'a .e ko'e se broda}`. So formally it would create a new kind of term, a jo'u-set of bunches, predications on which always resolve distributively to predications on the constituent bunches. Presumably `{ko'a jo'u ko'e joi ko'i}` == `{ko'a joi ko'i joi ke ko'e joi ko'i}`.

## New thinking on handling descriptors

Handle `{lo broda}` analogously to unbound `{da}`: export to outermost scope as a glorked Skolem function (which generally is just a constant). So `{ro broda lo brode cu brodi}` -> GL X:brode(_). FA x:broda(_). brodi(x,X); `{ro da lo brode be da cu brodi}` -> GL F:brode(_,\1). FA x:broda(_). brodi(x,F(x)). Then there's no issue with anaphora, except where they break through prenexes — which probably just shouldn't be allowed. `{zo'e}` we can treat as `{lo du}`. Or, perhaps better for now, leave as a term (but not handling `{noi}` in any magic way for it). Implementing this: looks like we need to pass around another continuation… Has the time come for a monad? One which simultaneously handles bindings, glorkings and prenex exportation? It seems so. But handling Skolem functions seems remarkably painful. Note that they can in principle be functions not only of explicitly quantified things, because we also have e.g. `{ti .e ta lo broda be ri cu brode}`… Why not just have a GL quantifier scoping as with `{da}`? Main issue: negations can then scope over the `{lo}`. So we have something like CLL-lo, but with a plural variable. So modulo glorkiness, the same as `{pi za'u}` ought to be. We could consider this to be correct xorlo, and have the fact that it's possible to pull it back out of negations and quantifiers (sometimes involving introducing a Skolem function) as being part of the semantics of GL.

## Semantics of GL

We go through plausible bunches, in decreasing order of plausibility, until we hit one which makes the statement *as a whole* true. Note that this is rather different from a plural existential — we want the whole statement to be true, not just the formula below the GL. We even break out of abstractions this way. This can get unreasonably complicated… So even if we only want to work with finite models, it seems we're forced to pass to skolem functions. Note: it isn't clear whether xorlo itself is meant to involve glorking.

## Giving up on gadri

Just treat them as uninterpreted quantifiers: `{broda lo brode ri}` -> "{lo} x1:(brode(_)). broda(zo'e,x1,x1)"; for now at least, we leave further processing to the pragmatics module. (Yes, this is a complete cop-out.)

## Questions

`{ma}` needs to get outermost scope in some cases; but there is a scope-respecting alternative — work syntactically. The answer to a `{ma}` question is a sumti fragment, evaluated in the scope at which the `{ma}` occurred. The simpler approach is to say that "illocutionary operators" scope over the entire statement (or, in the case of du'u, subsentence).

## Weak dedonkeyisation

A (weak) donkey is a situation of anaphoric reference to something which is within (but not bound by) a quantifier of which the anaphor is out of scope. E.g. `{ro da zo'u da broda le brodi .i ri brode}` is a weak donkey, and `{ro da broda .i ri brode}` is a strong donkey. There's no theoretical problem in interpreting weak donkey anaphora correctly, so we should. Probably we should separate out binding of `{ko'a}`, `{ri}` and letterals from binding of `{da}` and `{ke'a}` etc. Only the latter exhibit current scope behaviour. So we need to complicate our monads. We also need to consider how to handle description sumti; Skolem functions are likely a better fit.

## GOhA

Often donkey. E.g. `{ro da broda .i go'i}` is donkey. `{broda .i go'i}` is *always* weakly donkey. Obvious algorithm: process all terms, leaving some jboterms and a (possibly complicated, possibly tagged) selbri; assign that lot to go'i, with the jboterms effectively becoming linkargs. These jboterms will generally be (weakly) donkey when the go'i is used. Problem: bound terms may appear in the selbri itself, in linkargs or NU. Donkeys everywhere!

## Illocution and scope

As with questions, these could in principle respect scope, but a statement/du'u-subsentence should have a single uniform illocutionary force. Note that illocution scope is not the same as noi's side-statement scope or lo's existential-presupposition scope, because the latter break out even of du'u-subsentences.

## Handling unquantified terms

(Amongst which: `{la foob}`, `{mi}`, and, at least under xorlo, description sumti.) These should be understood *before* we get into the "logical" part of the parsing — they are discourse referents. Main issue: this neat separation isn't really possible, because e.g. whether we interpret `{lo broda be da}` as a function or as a constant depends on whether `{da}` is already bound when the term appears. Suggested simple solution: `{lo broda be da}` with unbound `{da}` is an error. `{lo broda be su'o da}` is fine. So `{lo broda be da}`, or `{lo broda be ri}`, or even `{lo broda be ko'a}`, is interpreted in the first place as a discourse referent *function*, about which we know that every value satisfies broda(f(x),x). Anaphora can never be bound to such functions, only their values.

## Giheks, geks, and tailterms

Xorxes convinced me to change the handling of giheks to parallel eks rather than ijeks, but I'm not sure I'm happy about this. They feel more afterthoughty to me. The only issue with treating them as such is tailterms. Decided to switch back to the original behaviour — we handle tailterms with extendTail, then handle them analogously to ijeks. *FIXME: rethink this in light of my tag-forced change in ijek handling (I expect we now find that xorxes was right).*

## Quantifier scope in tanru units

Can we use `{ko'a mecrai pilji be ro ko'e}` for "ko'a is the least common multiple of (the referents of) ko'e"?

## Tags

We have three kinds of tags: selbri tags, term tags, tagged connectives. Selbri and term tags are to be handled analogously to NA. For tag connectives: `{ba gi broda gi brode}` claims that an event ko'a of broda occurs, and that an event of brode occurs ba ko'a. So we seem to want to complicate the logic in two ways: firstly by adding modal operators, secondly by adding modal connectives. One problem: 10.13.7 mandates `{pu broda baku}` == `{puba broda}`. So we're forced to equate `{pu broda}` with `{pu ku broda}`. So we're forced to export it to the prenex. But then everything falls apart. OK. So xorxes is right and I am wrong. Tags and negation are *bridi* operators, not statement operators.

## Tense, Events and Davidson

Davidson, translated: `{broda}` claims `{da nu broda}`. Then `{pu ko'a broda}` claims `{su'o nu broda gi'e purci ko'a}`, etc. Crucial difference: there's only one du'u broda, but generally many nu broda.

## Eventual interaction with a theorem prover

http://www.eprover.org/ looks good.

## The true horror of CLL lojban

`ro da na broda .i je da brode` seems forced by the rules on the scopes of na and da to come out as `na ku ro da zo'u ge da broda gi da brode`, so the rule has to be that a na gets shifted to the outside of the statement prenex — which is counterintuitive. Meanwhile, CLL rules force tenses to *not* be exported to the statement prenex. Furthermore, sumti connectives parallel variables, and we seem forced to render `mi .e do broda ro da .ije da brode` in a horrible expanded form. **Given all of which, we should GIVE UP on slavishly implementing CLL's bugs, and instead give a sensible semantics.** Clearly, that means xorxes' semantics. So `{na broda}` is interchangeable with `{na ku broda}`; sentences have prenexes (semantically whether or not grammatically); giheks work like geks; Cont isn't needed (!).

## 2014 rewrite

**Aims:** Simplify code (in particular dropping continuations, at cost of slavish CLL-support). Handle weak donkey anaphora. Prepare to handle illocution and side-assumptions.

**Donkeys:** To handle `{ri}` correctly, we can't handle quantification with functions. We need to clearly distinguish "monadic" bindings of sumbasti (ri, ko'a) from parse-respecting bindings of variables (da, ke'a). Use monads *only* for parse-disrespecting structure (e.g. sumbasti, noi clauses, xorlo etc). Pass down variable bindings as an explicit argument; use a monad to obtain a fresh variable each time; allow sumbasti to bind to variables; have use of a sumbasti bound to an unbound variable be an error. We should also handle logically connected sumti as variables.

**Implicit vars:** At the top of the NU/poi: get the Prop, check inside it for an instance of the variable; if find none, replace first ZoheTerm in each Rel with it.

**Connectives, terms, and xorlo:** If we really want to implement xorlo properly, then we do not want to make the mistake of thinking `{ko'a .e ko'e lo broda cu brode}` is equivalent to `{ge ko'a lo broda cu brode gi ko'e lo broda cu brode}`, because under xorlo it isn't. Handling logically connected sumti with variables deals with this.

**Tailterms:** Similar problem with `{broda gi'e broda vau lo brodi}`, complicated by wanting the gihek to scope over quantified tail terms. The correct solution is the same: use a relation variable! Recall that a pro-bridi (e.g. `{go'i}`) comes with an arglist. Basic problem: tailterms are parse-disrespecting monstrosities. Solution: parsing a term results (effectively) in `(Prop -> Prop, Maybe Arg, Bindings -> Bindings)`. The Bindings here are purely parse-respecting (da-series). Arg is a JboTerm maybe tagged with a numerical FA position (or a modal). This return value is used with continuations to ensure that the Prop map and any Arg are applied to the correct bridis. Using Cont really is very natural here.
