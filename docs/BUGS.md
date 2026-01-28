# Bugs, limitations, and questionable design choices

All known divergence from baseline (CLL + xorlo), or from plausible interpretations thereof, is noted here.

## Bugs

- **Lojban output of questions** is wrong (not a fixed-point; probably just needs to use `{kau}`).
- **Question words** other than `{xu}`, `{ma}` and `{mo}` are not handled properly.
- **Parser approximation of camxes** results in some bugs:
  - Free clauses (vocatives, `{to}` brackets, `{sei}` etc.) sometimes lead to parse errors if they're not at the start of a (sub)sentence
  - `{zoi}` quotes don't work properly
  - Magic words aren't handled accurately, and erasure cmavo not at all

## Limitations

- Most indicators and frees are ignored, even those such as "irrealis" attitudinals which arguably have logical meaning.
- `{ko}` is not handled specially.
- `{ra'o}` is ignored.
- The distinction between `{lV'e}` and `{lV}` (whatever that might be) is not handled.
- We parse the name in "LA sumtiTail", whereas arguably we should just use the text. Similarly, we don't keep hold of the raw text of a lu-quote.
- We don't handle `{nei}` or `{no'a}`, which have murky and self-referential semantics.
- We don't handle `{soi}`.
- **Seltcita termsets**, as described in CLL 10.25 (e.g. `{la frank. sanli zu'a nu'i la'u lo mitre be li mu}`), are not supported.
- We don't handle some donkey sentences which arguably we could, e.g.  
  `{ro tercange poi ponse lo ke'a xasli cu darxi xy}`, or more canonically  
  `{ro da poi broda ke'a goi xy cu brodi xy}`.

## Deliberate deviations from baseline (not considered bugs)

- `{na broda}` is equivalent to `{naku broda}`; consequently, `{ja'a}` has no logical effect. This is in line with the BPFK section "brivla negators".
- Quantifiers don't scope over (even connected) sentence boundaries, unless prenexed.
- We consider `{PA da}` to introduce a quantifier in the usual logical sense, so ignore CLL's rules on rebinding bound variables (CLL:16.14) and simultaneous quantification in termsets (CLL:16.7) which conflict with this.
- Some constructs are accepted which the official grammar rejects; e.g. the tag grammar we use is essentially that of xorxes' zasni gerna.

## Other possibly contentious points

- **Sumti in already filled places** simply replace what was there before; this is well-established for GOhA pro-bridi, and we extend it to the general case. So e.g. `{broda ko'a fe ko'e}` == `{broda ko'e}`. Gleki points out some counter-evidence: CLL:9.3.9 and CLL:12.2.3.
- **Linkargs on the main selbri** are interpreted along with the arguments of the main bridi, rather than giving the selbri its own scope; this fits with the behaviour of bare tags on selbri, and with CLL:5.12.11.
