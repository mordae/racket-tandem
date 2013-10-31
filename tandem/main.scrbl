#lang scribble/manual

@require[(for-label tandem)
         (for-label racket)
         (for-label racket/async-channel)]

@require[scribble/eval]

@(define tandem-eval (make-base-eval))
@interaction-eval[#:eval tandem-eval (require racket/async-channel tandem)]

@title{Tandem}
@author+email["Jan Dvorak" "mordae@anilinux.org"]

Cooperative Communication Framework for Racket.

When communicating with a remote party, one often needs to run multiple
parallel queries from different threads while monitoring the communication
for asynchronous notifications.  The simple way to implement this pattern is
to spawn a dedicated background thread that takes care of the communication
and acts as a request-response server for other local threads.

Tandem realizes this pattern using cpu time of threads doing the calls or
listening for replies, thus eliminating the background thread.


@defmodule[tandem]

Tandem works as a middleware of sorts.  You need to supply two backend
functions - one that transmits a tagged message and one that receives it.

@defproc[(tandem (transmit-proc (-> any/c any/c void?))
                 (receive-proc (-> (values any/c any/c)))) tandem?]{
  Create structure that tracks state of the cooperative work.

  As an example, we create a simple FIFO echo server that does not really
  communicate with anything external, but imagine that we used a TCP socket
  instead of the asynchronous channel.

  @examples[#:eval tandem-eval
    (define echo-server
      (let ((echoes (make-async-channel)))
        (tandem (lambda (msg-tag msg-value)
                  (async-channel-put echoes (list msg-tag msg-value)))
                (lambda ()
                  (apply values (async-channel-get echoes))))))
  ]
}

@defproc[(tandem? (v any/c)) boolean?]{
  Check that value is a tandem structure.

  @examples[#:eval tandem-eval
    (tandem? echo-server)
  ]
}

@defproc[(tandem-send (tandem tandem?) (tag any/c) (value any/c)) void?]{
  Send a message you don't expect a reply to.

  @examples[#:eval tandem-eval
    (tandem-send echo-server 'a-tag "something")
  ]
}

@defproc[(tandem-wait (tandem tandem?) (tag any/c)) any/c]{
  Wait for a single value with specified tag.

  Since we are using an asynchronous queue and nobody else does try to
  outrun us, we can retrieve the value from @racket[tandem-send] above.

  @examples[#:eval tandem-eval
    (tandem-wait echo-server 'a-tag)
  ]
}

@defproc[(tandem-call (tandem tandem?) (tag any/c) (value any/c)) any/c]{
  Perform a full remote call by sending specified tagged value and
  waiting for a reply with the same tag.

  @examples[#:eval tandem-eval
    (tandem-call echo-server 'hello "Hello World!")
    (tandem-call echo-server 'bye "It's time to wrap this up.")
  ]
}

@defproc[(tandem-listen (tandem tandem?)
                        (tag any/c)
                        (handle-proc (-> any/c void?))) void?]{
  Wait indefinitely for all values matching given tag and apply the handler
  procedure to them.  The only way to escape is to raise an exception.

  @examples[#:eval tandem-eval
    (tandem-send echo-server 'something "nice")
    (tandem-listen echo-server 'something
      (lambda (value)
        (error 'something-handler "received ~s\n" value)))
  ]
}

@defproc[(tandem-communicate (tandem tandem?)
                             (tag any/c)
                             (handler-proc (-> (-> any/c void?)
                                               (-> any/c)
                                               any/c)))
         any/c]{
 Run @racket[handler-proc] with one callback for sending tagged values and
 another one for receiving them in order for it to perform a multi-step
 communication.

 @examples[#:eval tandem-eval
    (tandem-communicate echo-server 'tag
                        (lambda (transmit receive)
                          (transmit "Hello!")
                          (let ((first-reply (receive)))
                            (transmit "Thank you!")
                            (list first-reply (receive)))))
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
