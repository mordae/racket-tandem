#lang racket/base
;
; Common Utilities
;

(require unstable/error)

(provide (all-defined-out))


;; To make exception raising bearable.
(define-syntax-rule (throw constructor name message arg ...)
  (raise-misc-error #:constructor constructor
                    name message arg ...))


; vim:set ts=2 sw=2 et:
