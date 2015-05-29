Atomic
Lambda
Row: Product or Sum - represent tagged constructors as nested rows ie a sum of products
Label/selector/etc -> pinned or polymorphic or literal
Predicates: Lacks, Row equality
Operations: Row extension, Row restriction
Binding
Defining form

---

Note:

###Locatives

- Variables, through quantification. Available both inside the scope of a statement that is typed by a signature, as well as in the signature itself
- capabilites/unforgeable names
- selection/projection things, made available through labels
- by-nameish-things, like procs, fns, methods, predicates etc

&foo = literal foo label

∀ gives polymorphism as such.
∃ gives parametrisation ie functors/modules.
λ gives free variables as such.
ı gives a fresh variable
μ gives a fixpoint; this is useful for mutual recursion ie 'μ x y.' for x and y as mutually recursive.
Everything that is not quantified is assumed to be not a variable as such - ie another kind of locative..

{} are products, and can be used to declare type or data
<> are sums, and can be used to declare type or data
⦇⦈ are rows which could be products or sums, and can be used only to declare type

case :: ∀ x y z a. <x::y |z> → {x::(y→a) |z} → a.
case it pattern = (pattern x) (it x)

quantifier-block == ('∀'|'∃'|'λ') identifier+ '.'
type == quantifier-block\* (row-type|product-type|sum-type|lambda-type|atomic-type|identifier) '.'
top-level-type-constraint == identifier '::' type '.'
assignment == identifier '=' (type|value) '.'

use:

List = ∃ t. <&nil::&nil, &cons::{&car::t, &cdr::List t}>.

listtostr :: ∀ t. List t → String
listtostr lst = "(" +
  case lst {
    &nil   ⇒ \_ → "",
    &cons  ⇒ \x → (show $ x &car) + (listtostr $ x &cdr)
  } + ")".

processnode :: ∃ a. ∀ x y z. {x::(y→a) |z} → <x::y |z> → a.
processnode pattern it = (pattern∘x) (it∘x)

Initial Kinds:

Atomic, Row, Product, Sum, Label, Predicate?, Statement, Expression, Lambda, Type k

Type :: Nat → Nat
Type (k::Nat) :: Type (k + 1)
Type k = 

Integer, 

---

Atomic Type
Dependent Row: Product or Sum
Dependent Lambda

---

Atomic -> Thing with an Edge backing onto... something. The Edge defines the primops. Import the Edge to get access to the values that hold those primops.

---

levitation -> type -> described type
sinking -> described type -> type

---

; = ., - for ending a statement that would otherwise be ambiguous, then continuing in a list.

---

· like 'flip' in haskell.

a · b == b a.
This has symmetry with '.' in the case of labels; .&foo gets foo from the environment, a·&foo gets foo from a.

---

a `\w` macro which you can use to create implicit variable/dynamic binding like things.

