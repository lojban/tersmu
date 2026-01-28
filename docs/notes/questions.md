# Questions

Definitely a challenge!

## Recall

- Question words in subclauses affect the top-level statement.
- `{kau}` can appear anywhere, and corresponds to a question as to the identity of the preceding construct (which need not be a question word).
- `{xu kau}` is further magic, and doesn't rise to the top like bare `{xu}`.

`{kau}` in particular is going to be a nightmare, and should perhaps be taken as evidence that we need to handle indicators much more subtly than I'd been hoping to get away with.

But for now we can take the easy way out, and pretend there's a selma'o KAU which is allowed only in those positions required to allow the q-kaus.

The semantics of questions are clear. Representing connective questions will be a pain, though — we effectively want a notation for quantifying over propositional and sumti connectives (noting that there's only one question in `{ko'a .e ko'e broda je'i brode}`).

For indirect questions: it's simply a matter of scope. (We might as well support subscripted kau to allow getting at higher embedded scope, as with ce'u/ke'a.)

## Another thing to consider

What does it mean to answer `{re da ma broda}` with `{ro de}`, or indeed `{da}`? No answer in prescription. I'm currently thinking it's best to consider question quantifiers to have maximal scope, effectively, and parallel GOhA, so e.g. answering with `{da}` means claiming `{de zo'u re da de broda}`.

Connective questions then should scope-jump analogously:

- `{re da ko'a ji ko'e broda}` == `{de po'u ko'a ji ko'e zo'u re da de broda}` (or just as well, `{ko'a ji ko'e vu'o goi fo'a zo'u re da fo'a broda}`) — "for which of ko'a and ko'e do two things broda it?"

although we have to have

- `{mi .a do ge'i broda gi brode}` == … (logical expansion with da po'u no ji pa etc.)

Ah, but these logical tricks are no good, since we want to allow non-logical connectives as answers… How about using a formulation with `li jy du li pa ji li re` and `ju'e xi jy`, where we just declare that this works for both logical and non-logical?

## So on the semantic side we need

- New quantifier ("for what x", "?x1." / `{da po'u ma}`).
- Another quantifier "?x1:(ko'a,ko'e).", corresponding to `{da po'u ko'a ji ko'e}`.
- Yet another quantifier "?<c1>." and a new connective <c1> (and yes, we really do want multiple variable types).
- New bridi quantifier for `{mo}`.

---

`{xo}` is painful. It's in selma'o PA, so it can appear in the middle of a number or lerfu string. I suppose it actually should just be handled textually? Similarly for cu'e and ji?
