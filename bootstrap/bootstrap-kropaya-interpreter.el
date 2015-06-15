;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'cl-lib)

;; Parsing

(defun alt (&rest parsers)
  (lambda (text pos struct)
    (let ((result-pos pos)
          (result-struct struct))
      (cl-loop 
        for parser in parsers
        unless 
          (eq pos result-pos)
          return (list result-pos result-struct)
        do (let ((intermediate-result (funcall parser text pos struct)))
             (setq result-pos    (car intermediate-result))
             (setq result-struct (cdr intermediate-result)))
        finally (return (list result-pos result-struct))
        ))))


;;(let ((foo (lambda (text pos struct) (princ "Foo: <") (princ text) (princ pos) (princ struct) (princ ">\n") (list pos struct))))
;;  (print "About to call alt.")
;;  (princ (funcall (alt foo) "abcde" 2 '())))

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
