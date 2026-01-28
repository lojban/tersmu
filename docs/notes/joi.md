# JOI

From the discussion in CLL, it seems we can consider JOI to have precisely these functions:

1. **As propositional connectives**, entirely analogous to the logical ones but with murkier semantics acting on the events/predications (e.g. it refers to `{joi gi broda gi brode .iseri'abo brodi}` as meaning that broda and brode combined cause brodi).
2. **Tanru connections**
3. **Sumti connections**

All joi productions, including term connections, except tanru and sumti connections, should be expanded to (1) analogously with logical connections.

It would be nice to reduce (2) to (1), e.g. to say that the tanru-unit `{broda joi brode}` refers to the (say unary) relation `{joi gi ce'u broda gi ce'u brode}`. But 5.6.20 suggests otherwise.

We can however try to reduce (1) to (3). We can interpret  
`{joi gi broda gi brode .iseri'abo brodi}`  
as implying  
`{lo nu broda ku joi lo nu brode cu rinka lo nu brodi}`.

c.f. [BPFK Section: sumtcita Formants](http://www.lojban.org/tiki/BPFK+Section%3A+sumtcita+Formants)

## Tags as binary relations

`(ba)(x). broda()` is shorthand for  
EX e. (e=. broda() ∧ (ba)(x,e))  
where in fact `(ba)(x,e) == selbalvi(x,e)` (but bare tenses still obey special rules, which we shouldn't try to implement, as to what should go in for x).

However, I think we should go with adding **non-logical connectives** to the logic, and merely having it as the intended semantics that

- EX e. EX e1. EX e2. (e=. (e1=. broda() {joi} e2=. brode()) ∧ e=(e1{joi}e2))
