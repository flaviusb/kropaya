(print "Test parsing some tiny programs.")

(print (funcall (list-of-at-least-two-x (alt #'parse-literal)) "6 5." 0 '()))
(print (funcall (seq #'parse-juxtaposition) "6 5." 0 '()))

(mapcar (lambda (txt)
    (progn 
        (print (parse-tree-branch txt 0 '()))
        (print (parse-tree txt 0 '())))) '(
  "7." "8.4." "\"a wef ref regbrev \"." "1 2." "1 2 3." "7 (8 9.)." "(7 8.) 9." "7 (4 5.) 7." "1 ((3 4.) 5.) 6."  "1 (2 (3 4.).) 6."  "1 (2 (3 4.) 5.) 6." "(3 (4 5.) 7.) ((4 5.) 7.)."
  "\\ → 3." "\\x y z → 3." "\\x y z→ y." "\\x y z→(z y x.)."
  "&f" "&car x." "&car (&cdr y.)." "(&car x.) y." "\\x→(&foo x.)."))
