Keywords are sigillised with %.

The keywords are %let, %end, and %edge.

Labels are first class, and are punned as functions; literal labels are sigillised with &.

Variables are introduced in two ways:

- Through quantification
- Through %let statements

The quantifiers are ∀ ∃ μ λ ı (forall, there exists, mu, lambda, fresh), which work 'as usual'. Note: ∃ has no ontological commitment.

The evaluation of %let blocks happens sequentially. Evaluation of = statements inside a %let block happens with all rhs first, then when the corresponding %end is reached, all the bindings are made. So, this means that μ must be used for = statements inside a %let to refer to each other, but succesive %let statements may refer to previous %let statements.

Typing judgements are signalled with ':'.

Sum types are \<\>. Product types are {}. Rows are separated by commas. As types, each row is '&label: type'. As values, each row is '&label ⇒ value' or '&label ⇒ (value: type)'. Sums and products can be polymorphically extended, restricted, trimmed, and stated with type variables - '|' is extension, '/' is restriction.

Juxtaposition is function application. Brackets work 'as usual'. Numeric and string literals work 'as usual'.

%edge is used to introduce hooks from the rts, and (eventually) for ffi.

The syntax of a %let block is:

%let ((target =)|) quantifier\*
  ((binding = value)|(%let...)) \*
%end

Without a target, the bindings are evaluated into the current context. Otherwise, a module is created.

Bindings inside a module are accessed with #.

