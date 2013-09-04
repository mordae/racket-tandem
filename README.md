# Tandem

Cooperative Communication Framework for Racket.
See the scribblings for more information.

## Example

```racket
(require racket/async-channel
         tandem)

;; Just a silly echo server.
;; Imagine using a TCP socket or something.
(define echo-server
  (let ((echoes (make-async-channel)))
    (tandem (lambda (msg-tag msg-value)
              (async-channel-put echoes (list msg-tag msg-value)))
            (lambda ()
              (apply values (async-channel-get echoes))))))

(tandem-call echo-server 'hello "Hello World!")
;; -> "Hello World!"
```
