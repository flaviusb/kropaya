;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'cl-lib)

;; Parsing

; A parser takes (text pos data) and returns a parser-result.
(cl-defstruct parser-result pos data (match? t) (decoration nil))

;Combinators take one or more parsers and return a parser
(defun alt (&rest parsers)
  (lambda (text pos data)
    (let* ((base-result (make-parser-result :pos pos :data data :match? nil))
           (result base-result))
      (cl-loop 
        for parser in parsers
        do (setq result (funcall parser text pos data))
        if
          (parser-result-match? result)
          return result
        finally (return base-result)
      ))))

(defun opt (parser)
  (lambda (text pos data)
    (let ((result (funcall parser text pos data)))
      (if (parser-result-match? result)
        (result)
        (make-parser-result :pos pos :data data)))))

(defun seq (&rest parsers)
  (lambda (text pos data)
    (let* ((result (make-parser-result :pos pos :data data)))
      (cl-loop 
        for parser in parsers
        unless
          (parser-result-match? result)
          return result
        do (setq result (funcall parser text (parser-result-pos result) (parser-result-data result)))
        finally (return result)
      ))))

(defun star (parser)
  (lambda (text pos data)
    (let* ((result (make-parser-result :pos pos :data data))
           (previous-result result))
      (cl-loop 
        unless
          (parser-result-match? result)
          return previous-result
        do (setq previous-result result)
        do (setq result (funcall parser text (parser-result-pos previous-result) (parser-result-data previous-result)))
      ))))

; Some basic parser creators

(defun safe-substring (text start end)
  (if (>= (length text) end)
    (substring text start end)
    nil))

(defun lit (val)
  (lambda (text pos data)
    (if (string= (safe-substring text pos (+ pos (length val))) val)
      (make-parser-result :pos (+ pos (length val)) :data data)
      (make-parser-result :pos pos :data data :match? nil))))

(defun regexp-match (val)
  (lambda (text pos data)
    (let ((match-point (string-match val text pos)))
      (if (eq match-point pos)
        (make-parser-result :pos (match-end 0) :data data)
        (make-parser-result :pos pos :data data :match? nil)))))

(defun wrapped (parser action)
  (lambda (text pos data)
    (let ((result (funcall parser text pos data)))
      (if (parser-result-match? result)
        (make-parser-result :pos (parser-result-pos result) :data (funcall action (parser-result-data result) pos (parser-result-pos result) text) :decoration (parser-result-decoration result))
        (make-parser-result :pos pos :data data :decoration (parser-result-decoration result) :match? nil)))))

(defun none-of-lit (scan-by &rest vals)
    (lambda (text pos data)
      (cl-loop
        for val in vals
        if (string= (safe-substring text pos (+ pos (length val))) val)
          return (make-parser-result :pos pos :data data :match? nil)
        finally (return (if (< (length text) (+ pos scan-by))
                  (make-parser-result :pos pos :data data :match? nil)
                  (make-parser-result :pos (+ pos scan-by) :data data)))
      ))
    )

;; Pattern matching + destructuring

;; Builder

(defun make-primitive (tag val)
    (list tag val))

(defun make-identifier (val)
  (make-primitive 'identifier val))

(defun make-quantifier (kind variables)
  (make-primitive 'quantifier (make-primitive kind variables)))

;; Sum and product pairs are an alist of labe
(defun make-type-row (kind pairs)
  (make-primitive kind pairs))

(defun make-sum (pair)
  (make-primitive 'sum pair))

(defun make-product (pairs)
  (make-primitive 'product pairs))

(defun make-module (name quantifiers forest)
  (make-primitive 'module `((name . ,name) (quantifiers . ,quantifiers) (forest . ,forest))))

(defun make-variable (name)
  (make-primitive 'variable name))

;(setq prelude (make-module 'prelude '() '()))
;
;(print prelude)
;(print (cadr prelude))
;
;(print (assq 'name (cadr prelude)))

;; AST

;; Read

(defun clear-data (text pos data)
  (make-parser-result :pos pos :data ""))

(defun return-text-under-match (parser)
  (wrapped
    parser
    (lambda (data start end text)
      (concat data (substring text start end)))))

(defun return-lit-on-match (parser lit)
  (wrapped
    parser
    (lambda (data start end text)
      (concat data lit))))

(defun new-context-then-merge (parser new-context merge-function)
  (lambda (text pos data)
    (let* ((result (funcall parser text pos new-context))
           (new-data (parser-result-data result)))
      (make-parser-result :pos (parser-result-pos result) :data (funcall merge-function data new-data) :match? (parser-result-match? result)))))

(defun parse-string (text pos data)
                     (funcall (seq (lit "\"")
                      (new-context-then-merge
                        (wrapped
                            (star (alt 
                               (return-text-under-match (none-of-lit 1 "\"" "\\"))
                               (return-lit-on-match (lit "\\\"") "\"")
                               (return-lit-on-match (lit "\\\\") "\\")))
                          (lambda (data start end text)
                            (list 'text data))) nil (lambda (x y) (cons y x)))
                        (lit "\"")) text pos data))


(defun parse-d-int (text pos data)
  (funcall (wrapped
             (regexp-match "[+-]?[0-9]+")
             (lambda (data start end text)
               (cons (list 'int (string-to-number (substring text start end))) data))) text pos data))

(defun parse-int (text pos data)
  (funcall (alt #'parse-d-int) text pos data))

(defun parse-real (text pos data)
  (funcall (wrapped
             (regexp-match "[+-]?[0-9]+\\.[0-9]+")
             (lambda (data start end text)
               (cons (list 'real (string-to-number (substring text start end))) data))) text pos data))

(defun parse-number (text pos data)
  (funcall (alt #'parse-real #'parse-int) text pos data))

(defun parse-ws (text pos data)
  (funcall (regexp-match "[ ]+") text pos data))

(defun parse-comment (text pos data)
  (funcall (regexp-match "\\(※.*$\\)\\|\\(#\\.[^.]*\\.\\)") text pos data))

(setq identifier-string "\\(\\([_+]+[_+:]*\\)?[a-zA-Z][a-zA-Z0-9_:$!?%=<>-]*\\)\\|\\([~!@$%^*_=\'`/?×÷≠⧺⧻§∘≢∨∪∩□⊃∈+-]+[:~!@$%^*_=\'`/?×÷≠⧺⧻§∘≢∨∪∩□⊃∈+-]*\\)\\|\\(\\[\\]\\)\\|…")

(defun parse-variable (text pos data)
  (funcall (new-context-then-merge
             (return-text-under-match (regexp-match identifier-string))
             ""
             (lambda (old new) (cons old (make-variable new))))
           text pos data))

(defun list-of-vars (text pos data)
  (funcall (new-context-then-merge
             (seq #'parse-variable (star (seq #'parse-ws #'parse-variable)))
             nil
             (lambda (old new) (list new old)))
           text pos data))

(defun quantifier (name parser)
  (lambda (text pos data)
    (funcall (wrapped
               parser
               (lambda (data start end text) (make-quantifier name data)))
             text pos data)))

(defun parse-quantifier (text pos data)
  (funcall (new-context-then-merge
             (alt
               (quantifier 'forall (seq (lit "∀") (star #'parse-ws) #'list-of-vars (star #'parse-ws) (lit ".")))
               (quantifier 'exists (seq (lit "∃") (star #'parse-ws) #'list-of-vars (star #'parse-ws) (lit ".")))
               (quantifier 'mu     (seq (lit "μ") (star #'parse-ws) #'list-of-vars (star #'parse-ws) (lit ".")))
               (quantifier 'fresh  (seq (lit "ı") (star #'parse-ws) #'list-of-vars (star #'parse-ws) (lit ".")))
               (quantifier 'lambda (seq (lit "λ") (star #'parse-ws) #'list-of-vars (star #'parse-ws) (lit ".")))
              )
              nil
              (lambda (old new) (cons old new)))
           text pos data))

(defun parse-identifier (text pos data)
  (funcall (new-context-then-merge 
             (return-text-under-match (regexp-match identifier-string))
             ""
             (lambda (old new) (cons old (make-identifier new))))
           text pos data))

(defun parse-name (text pos data)
  (parse-identifier text pos data))

;; Eval
;; Print
;; Runloop
;; Universe

; We use the kropaya representation of these things as much as possible
;(defun make-universe ()
;  )

;(setq universe)

;; Mirror

;; Prelude

; Load prelude + add edges as needed
