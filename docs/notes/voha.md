# vo'a

Should be able to refer to as-yet-unspecified sumti, which causes problems with e.g.

- `vo'e broda ko'a gi'e brode ko'e`
- or `lo brodi be vo'e broda ko'a gi'e brode ko'e`

Actually, not too bad: we just need an extra JboTerm constructor, `MainBridiSumti Int`, and every time we add a positional argument to a main bridi we do a `mapProp` which does a substitution. That will fork appropriately in the above examples. Most annoying bit then is just having to know when we're in a "main bridi".
