(print "test int")
(mapcar 'print (mapcar (lambda (txt) (parse-int txt 0 '())) '("1" "123124" "4574682468" "wergwrg" "v34f3qr" "325423rf3ffedcv")))
