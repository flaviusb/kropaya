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

&foo = literal foo label

∀ gives polymorphism as such.
∃ gives parametrisation ie functors/modules.
λ gives free variables as such.
Everything that is not quantified is assumed to be a bound variable as such.

{} are products, and can be used to declare type or data
<> are sums, and can be used to declare type or data
⦇⦈ are rows which could be products or sums, and can be used only to declare type

case :: ∀ x y z a. <x::y |z> → {x::(y→a) |z} → a.
case it pattern = (pattern∘x) (it∘x)

use:

List = ∃ t. <&nil::&nil, &cons::{&car::t, &cdr::List t}>

listtostr :: ∀ t. t∈Showable. List t → String
listtostr lst = "(" +
  case lst {
    &nil   ⇒ "",
    &cons  ⇒ \x → (show $ x∘car) + (listtostr $ x∘cdr)
  } + ")".

processnode :: ∃ a. ∀ x y z. {x::(y→a) |z} → <x::y |z> → a.
processnode pattern it = (pattern∘x) (it∘x)

---

Atomic Type
Dependent Row: Product or Sum
Dependent Lambda

---

Atomic -> Thing with an Edge backing onto... something. The Edge defines the primops. Import the Edge to get access to the values that hold those primops.

---

levitation -> type -> described type
sinking -> described type -> type
