;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'cl-lib)

;; Parsing

(cl-defstruct parser-result pos data (match? t) (decoration nil))

(defun alt (&rest parsers)
  (lambda (text pos data)
    (let* ((base-result (make-parser-result :pos pos :data data :match? nil))
           (result base-result))
      (cl-loop 
        for parser in parsers
        if
          (parser-result-match? result)
          return result
        unless
          (parser-result-match? result)
          do (setq result base-result)
        do (let ((intermediate-result (funcall parser text pos data)))
             (setq result intermediate-result))
        finally (return result)
      ))))


;;(setq foo (lambda (text pos struct) (princ "Foo: <") (princ text) (princ pos) (princ struct) (princ ">\n") (make-parser-result :pos pos :data struct :decoration "ffffooooo")))
;;(setq goo (lambda (text pos struct) (princ "Goo: <") (princ text) (princ pos) (princ struct) (princ ">\n") (make-parser-result :pos pos :data struct :decoration "ggggooooo" :match? nil)))
;;
;;(print "About to call alt(foo).")
;;
;;(princ (funcall (alt foo) "abcde" 2 '()))
;;
;;(print "About to call alt(foo, goo).")
;;
;;(princ (funcall (alt foo goo) "abcde" 2 '()))
;;
;;(print "About to call alt(goo, foo).")
;;
;;(princ (funcall (alt goo foo) "abcde" 2 '()))
;;
;;(print "About to call alt(goo).")
;;
;;(princ (funcall (alt goo) "abcde" 2 '()))

;;(defun pos-to-line-number (text pos)
;;  ())


;; Pattern matching + destructuring

;; Builder

(defun make-primitive (tag val)
    (list tag val))

;; Sum and product pairs are an alist of labe
(defun make-type-row (kind pairs)
  (make-primitive kind pairs))

(defun make-sum (pair)
  (make-primitive 'sum pair))

(defun make-product (pairs)
  (make-primitive 'product pairs))

(defun make-module (name quantifiers forest)
  (make-primitive 'module `((name . ,name) (quantifiers . ,quantifiers) (forest . ,forest))))

;(setq prelude (make-module 'prelude '() '()))
;
;(print prelude)
;(print (cadr prelude))
;
;(print (assq 'name (cadr prelude)))

;; AST

;; Read
;; Eval
;; Print
;; Runloop
;; Universe
;; Mirror

;; Prelude

; Load prelude + add edges as needed
