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
        result
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

(defun plus (parser)
  (lambda (text pos data)
    (funcall (seq parser (star parser)) text pos data)))

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

(defun make-juxtaposition (tree)
  (make-primitive 'juxtaposition tree))

(defun make-lambda (tree)
  (make-primitive 'lambda tree))

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

(defun make-label-literal (name)
  (make-primitive 'label-literal name))

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

(defun parse-literal (text pos data)
  (funcall (new-context-then-merge
             (alt #'parse-number #'parse-string #'parse-label-literal)
             nil
             (lambda (old new) (if (eq old nil) new (append old new))))
             text pos data)) ; Extend this as we add more literal types

(defun parse-juxtaposition (text pos data)
  (funcall (new-context-then-merge
              ;(seq (alt #'parse-literal (bracketed-x #'parse-tree)) #'parse-ws (list-of-x (alt #'parse-tree-branch (bracketed-x #'parse-tree))) #'parse-nl-or-dot)
              (seq #'juxtaposition-chain-thing #'parse-nl-or-dot)
              nil
              (lambda (old new) (if (eq old nil) (list (make-juxtaposition new)) (append old (list (make-juxtaposition new))))))
           text pos data))

(defun bracketed-x (parser)
  (lambda (text pos data)
    (funcall (seq (lit "(") (opt #'parse-ws) (alt
                                               ;(bracketed-x parser)
                                               parser) (opt #'parse-ws) (lit ")")) text pos data)))

(setq parse-sub-expression (alt #'parse-tree-branch (bracketed-x #'parse-tree)))

(defun parse-lambda (text pos data)
  (funcall (new-context-then-merge
             (seq (lit "\\") (opt #'parse-ws) 
                  (wrapped (opt #'list-of-vars) (lambda (data start end text) (list 'arguments data))) (opt #'parse-ws) (lit "→") (opt #'parse-ws)
                  (new-context-then-merge
                    (wrapped parse-sub-expression (lambda (data start end text) (list 'body data)))
                    nil
                    (lambda (old new) (if (eq old nil) new (append old new))))
                  #'parse-nl-or-dot)
             nil
             (lambda (old new) (if (eq old nil) (list (make-lambda new)) (append old (list (make-lambda new))))))
           text pos data))

(defun parse-nl-or-dot (text pos data)
  (funcall (regexp-match "[.\n\r]+") text pos data))

(defun blocks-of-x (parser)
  (lambda (text pos data)
    (funcall (new-context-then-merge
               (seq parser (star (seq #'parse-nl-or-dot parser)))
               nil
               (lambda (old new) (if (eq old nil) new (append old new))))
             text pos data)))

(setq non-juxt-basis (alt #'parse-lambda #'parse-literal #'parse-variable))

(defun parse-tree-branch (text pos data)
  (funcall (new-context-then-merge
             (alt #'parse-juxtaposition non-juxt-basis)
             nil
             (lambda (old new) (if (eq old nil) new (append old (list new))))
             ;(lambda (old new) new)
             )
           text pos data))

;; parse-tree is the key parsing thing
(defun parse-tree (text pos data)
  (funcall (blocks-of-x #'parse-tree-branch) text pos data))

(defun parse-ws (text pos data)
  (funcall (regexp-match "[ ]+") text pos data))

(defun parse-comment (text pos data)
  (funcall (regexp-match "\\(※.*$\\)\\|\\(#\\.[^.]*\\.\\)") text pos data))

(setq identifier-string "\\(\\([_+]+[_+:]*\\)?[a-zA-Z][a-zA-Z0-9_:$!?%=-]*\\)\\|\\([~!@$%^*_=\'`/?×÷≠⧺⧻§∘≢∨∪∩□⊃∈+-]+[:~!@$%^*_=\'`/?×÷≠⧺⧻§∘≢∨∪∩□⊃∈+-]*\\)\\|\\(\\[\\]\\)\\|…")

(defun parse-variable (text pos data)
  (funcall (new-context-then-merge
             (return-text-under-match (regexp-match identifier-string))
             nil
             (lambda (old new) (if (eq old nil) (list (make-variable new)) (append old (list (make-variable new))))))
           text pos data))

(defun parse-label-literal (text pos data)
  (funcall (new-context-then-merge
             (seq 
               (lit "&")
               (return-text-under-match (regexp-match identifier-string)))
             nil
             (lambda (old new) (if (eq old nil) (list (make-label-literal new)) (append old (list (make-label-literal new))))))
           text pos data))

(defun parse-type-function-rhs (text pos data)
  (funcall (seq (opt #'parse-ws) (lit "→") (opt #'parse-ws) #'parse-type) text pos data))

(setq start-of-type-fragment (alt #'parse-variable (bracketed-x #'parse-type)))

;TODO: actually flesh out parse-type
(defun parse-type (text pos data)
  (funcall (alt 
             (seq #'start-of-type-fragment #'parse-type-function-rhs)
             #'start-of-type-fragment)
           text pos data))

(defun parse-singular-row (text pos data)
  (funcall (new-context-then-merge
             (seq
               (alt #'parse-label-literal #'parse-variable)
               (opt
                 (alt
                   (seq (opt #'parse-ws) (lit ":") (opt #'parse-ws) #'parse-type)
                   (seq (opt #'parse-ws) (lit "⇒") (opt #'parse-ws) parse-sub-expression))))
             nil
             (lambda (old new) (if (eq old nil) (make-singular-row new) (append old (make-singular-row new)))))
           text pos data))

(defun list-of-x (parser)
  (lambda (text pos data)
    (funcall (new-context-then-merge
               (seq parser (star (seq #'parse-ws parser)))
               nil
               (lambda (old new) new))
             text pos data)))

(defun list-of-at-least-two-x (parser)
  (lambda (text pos data)
    (funcall (new-context-then-merge
               (seq parser #'parse-ws parser (star (seq #'parse-ws parser)))
               nil
               (lambda (old new) (if (eq old nil) new (append old (list new)))))
             text pos data)))

(defun juxtaposition-chain-thing (text pos data)
  (funcall (list-of-at-least-two-x (alt non-juxt-basis (bracketed-x #'parse-tree)))
           text pos data))

(defun list-of-vars (text pos data)
  (funcall (list-of-x #'parse-variable)
           text pos data))

(defun list-of-quantifiers (text pos data)
  (funcall (list-of-x #'parse-quantifier)
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
              (lambda (old new) (if (eq old nil) (list new) (append old (list new)))))
           text pos data))

(defun parse-quantifiers (text pos data)
  (list-of-quantifiers text pos data))

(defun parse-identifier (text pos data)
  (funcall (new-context-then-merge 
             (return-text-under-match (regexp-match identifier-string))
             ""
             (lambda (old new) (cons old (make-identifier new))))
           text pos data))

(defun parse-name (text pos data)
  (parse-identifier text pos data))

(defun parse-let (text pos data)
  (funcall (alt parse-short-let parse-long-let) text pos data))

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
