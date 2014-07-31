#lang racket/base
;
; Cooperative Communication Framework
;

(require racket/contract
         racket/set)

(require misc1/fast-channel
         misc1/syntax
         misc1/evt)

(provide
  (contract-out
    (tandem? predicate/c)
    (rename make-tandem tandem
            (-> (evt/c any/c any/c) (-> any/c any/c void?) tandem?))

    (tandem-transmit (-> tandem? any/c any/c void?))
    (tandem-receive-evt (->* (tandem? any/c) (boolean?) (evt/c any/c)))
    (tandem-call-evt (-> tandem? any/c any/c any/c))))


(struct tandem
  (receive-any-evt transmitter waiters))

(define (make-tandem receive-any-evt transmitter)
  (tandem receive-any-evt transmitter (weak-seteq)))

(define (tandem-transmit tandem tag value)
  (let ((transmitter (tandem-transmitter tandem)))
    (transmitter tag value)))

(define (tandem-receive-evt tandem tag (once? #f))
  (let ((my-channel (make-fast-channel))
        (any-result (tandem-receive-any-evt tandem))
        (waiters (tandem-waiters tandem)))
    (producing new-evt
      (set-add! waiters my-channel)
      (replace-evt (choice-evt any-result my-channel)
                   (λ (r-tag r-value)
                     (cond
                       ((equal? tag r-tag)
                        (when once?
                          (set-remove! waiters my-channel))
                        (values (constant-evt r-value)))

                       (else
                        (for ((waiter waiters)
                              #:unless (eq? waiter my-channel))
                          (fast-channel-put waiter r-tag r-value))
                        (values new-evt))))))))

(define (tandem-call-evt tandem tag value)
  (define new-evt
    (cache-evt (tandem-receive-evt tandem tag #t)))
  (tandem-transmit tandem tag value)
  (values new-evt))


; vim:set ts=2 sw=2 et:
