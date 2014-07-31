#lang scribble/manual

@require[(for-label tandem)
         (for-label racket)
         (for-label misc1/fast-channel)]

@require[scribble/eval]

@(define tandem-eval (make-base-eval))
@interaction-eval[#:eval tandem-eval (require misc1/fast-channel tandem)]

@title{Tandem}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

Cooperative Communication Framework

When communicating with a remote party, one often needs to run multiple
parallel queries from different threads while monitoring the communication
for asynchronous notifications. The simple way to implement this pattern is
to spawn a dedicated background thread that takes care of the communication
and acts as a request-response server for other local threads.

Tandem realizes this pattern using events and channels within the threads
participating in the communication, thus eliminating the background thread.


@defmodule[tandem]

Tandem works as a middleware of sorts. You need to supply a backend event
that produces tagged messages from the remote party and a procedure that
sends our tagged messages.

Note that even though tandem itself is thread-safe and in fact designed
to be shared among multiple threads, it does not ensure that the actual
transmitting and receiving code is called from one thread only. It's up
to you to ensure that it will operate correctly under those conditions.

@defproc[(tandem (receive-any-evt (evt/c any/c any/c))
                 (transmit (-> any/c any/c void?)))
         tandem?]{
  Create structure that tracks state of the cooperative work.

  The first argument of the @racket[transmit] procedure is a message tag,
  that uniquely represents a call/return pair or non-uniquely identifies
  an out-of-band notification. The other argument is the payload.

  The same scheme applies to @racket[receive-any-evt]: it's first result
  value represents a tag and the second the payload.

  As an example, we create a simple FIFO echo server that does not really
  communicate with anything external, but imagine that we used a TCP socket
  instead of the asynchronous channel.

  @examples[#:eval tandem-eval
    (define echo-server
      (let ((echoes (make-fast-channel)))
        (tandem echoes
                (λ (tag value)
                  (fast-channel-put echoes tag value)))))
  ]
}

@defproc[(tandem? (v any/c)) boolean?]{
  Check that value is a tandem structure.

  @examples[#:eval tandem-eval
    (tandem? echo-server)
  ]
}

@defproc[(tandem-transmit (tandem tandem?) (tag any/c) (value any/c)) void?]{
  Send a tagged message to the other party.

  @examples[#:eval tandem-eval
    (tandem-transmit echo-server 'a-tag "something")
  ]
}

@defproc[(tandem-receive-evt (tandem tandem?) (tag any/c)) (evt/c any/c)]{
  An event that can be used to wait for a tagged value to arrive.

  Please note that creating such event registers a new channel that
  gets removed only after a garbage collection cycle. Thus creating
  many such events in a tight loop will be incredibly inefficient.

  Since we are using an asynchronous queue and nobody else tries to
  outrun us, we can retrieve the value from @racket[tandem-transmit] above.

  @examples[#:eval tandem-eval
    (sync (tandem-receive-evt echo-server 'a-tag))
  ]
}

@defproc[(tandem-call-evt (tandem tandem?) (tag any/c) (value any/c))
         (evt/c any/c)]{
  Perform a full remote call by sending specified tagged value and
  create an event that waits for a reply with the same tag.

  @examples[#:eval tandem-eval
    (sync (tandem-call-evt echo-server 'hello "Hello World!"))
    (sync (tandem-call-evt echo-server 'bye "It's time to wrap this up."))
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
