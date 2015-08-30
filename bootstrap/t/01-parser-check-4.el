(print "Test quantifiers.")
(mapcar 'print (mapcar (lambda (txt) (parse-quantifier txt 0 '())) '(
  "∀." "∃." "μ." "ı." "λ." ; Test empty quantifier blocks - these should fail to match
  "∀x." "∃ x." "μx ." "ı x ." "λx           ." ; Test one var
  "∀x y." "∃x y z s." "μ  dqwfwefx  wef rgearbgse    ." "ı x4." "λ§." ; Test several vars
  "∀5." "∃x" "μa→b." "ı∀." "λ" ; Test some failures
  )))
