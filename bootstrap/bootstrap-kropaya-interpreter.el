;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'cl-lib)

;; Monads

;; Parsing


;; Pattern matching + destructuring

;; Builder

(defun make-primitive (tag val)
    (list tag val))

(defun make-sum (&rest pairs)
  (make-primitive 'sum pairs))

(defun make-product (&rest pairs)
  (make-primitive 'product pairs))

;; AST

;; Read
;; Eval
;; Print
;; Runloop
;; Universe
;; Mirror
