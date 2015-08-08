(print "test lit")
(print (mapcar (lambda (txt) (funcall (lit "3234") txt 0 '())) (list "a" "32" "3234" "32345" "323456")))

(print "test seq")
