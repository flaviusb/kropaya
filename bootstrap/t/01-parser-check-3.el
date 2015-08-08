(print "test comments")
(mapcar 'print (mapcar (lambda (txt) (parse-comment txt 0 '())) '(
  "foo" ; No comments
  "#.fofikfnf\nwdfwrg." "#.." "#.  ." ; hash comments
  "#. wfe e2sdv" "# dsfwe" ; broken hash comments
  "※ weflkj 2ekjbf 2ekbfkweb keikb" "※\n" "※" ; Line comments
  "※lwdkf weofkn\n woeifhwodknfoweif" ; Line comments with stuff after
  )))

(print "test whitespace")
(mapcar 'print (mapcar (lambda (txt) (parse-ws txt 0 '())) '(
  "" " " "   " "   \n  "
  "  g   " "   \n g" "foo")))

(print "test identifier")
(mapcar 'print (mapcar (lambda (txt) (parse-identifier txt 0 '())) '(
  "a" "+a" "_b" "asd" "_:wdfwdf" "_+_b" "b33"
  "" ":a" " wefwef" "&rgb" "45")))

