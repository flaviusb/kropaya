The reified things must satisfy a signature.

Reader ∃input ast partial-ast-container.
  read: U input → F ast.
  read-partial: U input → F {&partial-ast: partial-ast-container ast, &residual-input: input}.

Printer ∃output ast.
  print: U ast → F output.

Evaller ∃ast partial-ast-container.
  eval: U Universe → U ast → F Universe.
  eval-partial: U Universe → U (partial-ast-container ast) → F Universe.

Universe
  sprout
  join
  get
  set
  exists

Mirror

Looper
  next: ∀ast input partial-ast-container (ev: Evaller ast partial-ast-container) (re: Reader input ast partial-ast-container). U {&universe: Universe, &partial-ast: partial-ast-container ast, &residual-input: input} → F {&universe: Universe, &partial-ast: partial-ast-container ast, &residual-input: input}.
  fork
  join

---

Positive types: values. Negative types: computations.
All values must therefore be positive, all computations negative.
'F x' makes x a computation - return a val, or force a thunk/computation. 'U x' makes x a value - thunk.
'→' goes from +ve to -ve - ie it has a default. 
U (x → F y)

foo^-: ⦇foo^+: z|\_⦈ → F z.
That is,
∀z (l:label).⌜⦇\`l^+:\`z|\_⦈ ⊨ \`l^-: ⦇\`l^+: \`z|\_⦈ → F \`z.⌝
z, l:label, a:rows, ⦇l^+:z|a⦈ ⊢ l^-: ⦇l^+: z|a⦈ → F z

---

Atomic
Lambda
Row: Product or Sum - represent tagged constructors as nested rows ie a sum of products
Label/selector/etc -> pinned or polymorphic or literal
Predicates: Lacks, Row equality
Operations: Row extension, Row restriction
Binding
Defining form

---

type/variable:

%let name = quantifier\* type-stuff/value-stuff.

*or*

%let quantifier\*
  (name = quantifier\* type-stuff/value-stuff.)\*
%end

module:

%let name = quantifier\*
  (name = quantifier\* type-stuff/value-stuff.)\*
%end

*or*

%let quantifier\*
  (%let name = quantifier\*
    (name = quantifier\* type-stuff/value-stuff.)\*
  %end)\*
%end


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

