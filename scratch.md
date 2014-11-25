Unification vs different syntaxes to enforce seperation of things.

State vectors as a thing.

Syntax for cyclic references.

Equality as a thing.

Functors applied to values passed through to scopes instead of modules.

Expressing invariants. Whole program invariants - ie an invariant that says that all state is in a particular state vector structure.

Declarative -> logical -> combinatorial -> relational (+ laws) -> pure functional -> functional -> imperative -- as part of the type system - so eg some things (like reader macros) can be defined to only take up to eg combinatorial fns.

Reader macros return the same kind of data structure as `read`, and `read` handles the splicing. That means that it can return quasiquoted values which will decay into regular values while filling any holes from the surrounding scope a la +{ } and -{ } from MetaLua.

'Views' a la Wadler.

Described types. Mirrors.

Quotient types. Mirrors, described types, equality. A Quotient type should be able to be defined through defining the relevant group, and then defining equivalence classes. Intensional vs extensional equality.

Predicates. Sets. Intensional vs extensional.

Totality. Divergence. Programs and co-programs.

Edges.
