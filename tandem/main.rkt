#lang racket/base
;
; Cooperative Communication Framework
;

(require racket/contract
         racket/function
         throw)

(provide (rename-out (new-tandem tandem))
         tandem?
         tandem-send
         tandem-wait
         tandem-call
         tandem-listen
         tandem-communicate)


;; Private communication data.
(define-struct/contract tandem
  ((transmit      (-> any/c any/c void?))
   (receive       (-> (values any/c any/c)))
   (waiting       (hash/c any/c (-> any/c void?)))
   (transmit-sema semaphore?)
   (receive-sema  semaphore?)
   (waiting-sema  semaphore?)))


;; Create the structure above with much less info from user.
(define/contract (new-tandem transmit-proc receive-proc)
                 (-> (-> any/c any/c void?)
                     (-> (values any/c any/c))
                     tandem?)
  (tandem transmit-proc
          receive-proc
          (make-hash)
          (make-semaphore 1)
          (make-semaphore 1)
          (make-semaphore 1)))


;; Execute body protected by given semaphore.
(define-syntax-rule (with-semaphore sema body ...)
  (call-with-semaphore/enable-break sema
    (thunk body ...)))


;; Send tagged value without remembering it.
(define/contract (tandem-send tandem tag value)
                 (-> tandem? any/c any/c void?)
  (with-semaphore (tandem-transmit-sema tandem)
    ((tandem-transmit tandem) tag value)))


;; Create and return channel that will receive result of a call.
(define (register-receive-channel tandem tag)
  (let ((waiting (tandem-waiting tandem))
        (channel (make-channel)))
    ;; Inform other threads about the fact the we are waiting for the tag.
    (with-semaphore (tandem-waiting-sema tandem)
      ;; We must not override another waiter, it would block indefinitely.
      (when (hash-has-key? waiting tag)
        (throw exn:fail 'register-receive-channel
               "someone is already waiting for that tag" "tag" tag))

      ;; Pass the boxed value through the channel back to this thread.
      (hash-set! waiting tag (lambda (value)
                               (channel-put channel (box value)))))

    ;; Return the channel for caller to wait on.
    channel))


;; Remove receiving channel registration with proper locking.
(define (unregister-receive-channel tandem tag)
  (with-semaphore (tandem-waiting-sema tandem)
    (hash-remove! (tandem-waiting tandem) tag)))


;; Receive value directly.
(define (receive-value tandem tag)
  (let ((waiting (tandem-waiting tandem)))
    (let loop ()
      (let-values (((value-tag value)
                    (call-with-continuation-barrier
                      (thunk ((tandem-receive tandem))))))
        (cond
          ;; We have found value with our tag, this is it.
          ((equal? tag value-tag)
           value)

          ;; The tag is not ours, look up who is waiting for it
          ;; and forward it to them.  Then look again.
          (else
           (let ((target (with-semaphore (tandem-waiting-sema tandem)
                           (hash-ref waiting value-tag (thunk void)))))
             (target value))
           (loop)))))))


;; Read result either directly or from the channel.
(define (poll-tandem-or-channel tandem tag channel)
  (let ((sync-result (sync channel (tandem-receive-sema tandem))))
    (cond
      ;; We have received the result from someone else.
      ((box? sync-result)
       (unbox sync-result))

      ;; We have been selected to wait for new values.
      ((semaphore? sync-result)
       (dynamic-wind void
         (thunk (receive-value tandem tag))
         (thunk (semaphore-post sync-result)))))))


;; Wait for tagged value to arrive.
(define/contract (tandem-wait tandem tag)
                 (-> tandem? any/c any/c)
  (tandem-communicate tandem tag (lambda (transmit receive)
                                   (receive))))


;; Send tagged value and wait for a reply with the same tag to arrive.
(define/contract (tandem-call tandem tag value)
                 (-> tandem? any/c any/c any/c)
  (tandem-communicate tandem tag (lambda (transmit receive)
                                   (transmit value)
                                   (receive))))


;; Indefinitely listen for tagged values to arrive.
;; As they arrive, the values are passed to the handler procedure.
(define/contract (tandem-listen tandem tag handler-proc)
                 (-> tandem? any/c (-> any/c void?) void?)
  (tandem-communicate tandem tag (lambda (transmit receive)
                                   (for ((value (in-producer receive)))
                                     (handler-proc value)))))


;; Runs handler-proc with a callback for sending tagged values and a callback
;; for receiving them in order for it to perform a multi-step communication.
(define/contract (tandem-communicate tandem tag handler-proc)
                 (-> tandem? any/c (-> (-> any/c void?) (-> any/c) any/c) any/c)
  (let ((channel (register-receive-channel tandem tag)))
    (dynamic-wind void
      (thunk
        (handler-proc (lambda (value)
                        (tandem-send tandem tag value))
                      (lambda ()
                        (poll-tandem-or-channel tandem tag channel))))

      (thunk
        ;; Make sure we unregister the receive channel.
        (unregister-receive-channel tandem tag)))))


; vim:set ts=2 sw=2 et:
