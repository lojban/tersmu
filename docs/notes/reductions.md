# Reductions

## Modals

- `JboTagged jbotag (Just t) p` |-> EX e. (TagRel [jbotag,t] ∧ e=. p)
- `pu ba broda` |-> EX e. (`{pu ba}`(e) ∧ e=. broda()) |-> pu(). EX e. (`{ba}`(e) ∧ e=. broda()) |-> EX e2. (`{pu}`(e2) ∧ e2=. EX e. (`{ba}`(e) ∧ e=. broda())
- `pu ba joi bai broda` |-> EX e. (`{pu ba joi bai}`(e) ∧ e=. broda()) |-> EX e. ((`{pu ba}` {joi} `{bai}`)(e) ∧ e=. broda()) |-> ???  
  Maybe irreducible… note interval-connected tenses even have obvious meanings…

## Joiked props

- `broda() {joi} brode()` |-> EX e1. EX e2. EX e3. (e1=. broda() ∧ e2=. brode() ∧ e3 = e1 {joi} e2)  
  (probably this adds nothing to `{broda ije brode}`)
- `joi gi broda gi brode .i se ri'a bo brodi` |-> EX e1. EX e2. EX e3. (e1=. broda() ∧ e2=. brode() ∧ e3 = e1 {joi} e2 ∧ EX e4. (e4=. brodi() ∧ ri'a(e3,e4)))
