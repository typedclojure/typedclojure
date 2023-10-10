- introduce the kind of Types
  - `t/Type` is the kind of all types
  - can constrain by wrapping parens:
    - `(t/Type :< upper :> lower)`
- combine type and dotted variable environments
- Breaking: nesting dotted variable expansions are now disallowed
  - since the dotted variable is scoped as a regular var before
    the dots and shadows the dotted variable
  - e.g., `(t/All [b :..] ['[b :.. b] :.. b -> t/Any])`
- scope dotted variables as kind `(t/* t/Type)`
