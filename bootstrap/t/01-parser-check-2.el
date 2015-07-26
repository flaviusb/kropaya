(print "test int")
(mapcar 'print (mapcar (lambda (txt) (parse-int txt 0 '())) '("1" "123124" "4574682468" "wergwrg" "v34f3qr" "325423rf3ffedcv")))
(print "test number")
(mapcar 'print (mapcar (lambda (txt) (parse-number txt 0 '())) '("1" "123124" "4574882468" "wergwrg" "v34f3qr" "325423rf3ffedcv" "+7" "-6" "3.0" "3.3" "0.000001" "-3.4" "-0.5423535413" "+-45.4" "+w2.4")))
