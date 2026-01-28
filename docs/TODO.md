# TODO

## Next main job

- Redo parsing so as to properly handle full grammar and morphology, and ensure frees are correctly associated with their positions.
- Currently unsure whether this should be done by using a camxes implementation as an external program, or by implementing camxes ourselves.
- Once this is done, we can think about handling UI and so on.

## Open questions

- **fai after tagless jai** is ignored; do we want that?

## Multiple jai places

Currently we just swap with a single `JaiPos`, yielding things like:

```
> jai ca ke jai ri'a broda be fai do ke'e fai mi
(ca)(). (ri'a)(mi). broda( )
> ko'a jai ca jai bau jai ri'a broda fai ko'e
(ca)({ko'a}). (bau)({ko'e}). (ri'a)(). broda( )
```

Alternative would be to have a jai push a stack, and use subscripted fai to get at inner places.

## Ideas / suggestions

- **xorxes suggests:** `pe [tag] ko'a = poi ke'a co'e [tag] ko'a` (and that using xo'i is too specific). Tempted to agree.
- Think again about zasni gerna tenses.
- Support `{sei sa'a}` and `{to'i}`?
- Use to…toi in lojban for sides?
- **Alternative tags mode:** doing away with modals and non-logical sentence connectives in favour of extra WithEventAs and TagRels? Nice idea, but unclear how to handle complicated joik connected tenses; see `docs/notes/reductions.md`.
- Option to render sumti (and tag, abstractor) connectives as quantifiers?
- **Pretty printing**
- Optional unicode output
- **Illocution:** ko etc.
- **Irrealis UI:** allow a free as a selbri tcita, then current free handling should get the scope right, and we can consider them as modal operators?
