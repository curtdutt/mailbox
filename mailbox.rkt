#lang racket/base

(provide mailbox-select
         mailbox->list
         mailbox-clear
         receive)

(require racket/list
         racket/match
         (for-syntax racket/base)
         (for-syntax racket/list))

(define (mailbox-clear)
  (when (thread-try-receive)
    (mailbox-clear)))


(define (mailbox->list)
  (let loop ([lst empty])
    (let ([msg (thread-try-receive)])
      (if msg
          (loop (cons msg lst))
          (begin
            (thread-rewind-receive lst)
            (reverse lst))))))


(define (mailbox-select match . events)
  ;we guard any operations that may execute code that
  ;might fail.
  ;so we catch any exceptions that occur when executing "untrusted" code
  ;and restore the mailbox to the state it was in prior to the call 
  (let loop ([unmatched-messages empty])
    (let ([ready-event (call-with-exception-handler
                        (λ (exn)
                          (thread-rewind-receive unmatched-messages))
                        (λ () (apply sync (thread-receive-evt) events)))])
      (cond [(equal? ready-event (thread-receive-evt))
             (let ([next (thread-receive)])
               ;when an exception occurs in a predicate
               ;we must rewind all messages back into the mailbox
               ;and then raise the exception
               (cond [(call-with-exception-handler 
                       (λ (exn)
                         (thread-rewind-receive (cons next unmatched-messages))
                         exn)
                       (λ ()
                         (match next))) =>
                                        (λ (result)
                                          (thread-rewind-receive unmatched-messages)
                                          result)]
                     [else
                      (loop (cons next unmatched-messages))]))]
            [else
             (thread-rewind-receive unmatched-messages)
             ready-event]))))


(define-syntax (receive stx)
  (syntax-case stx ()
    [(_)
     #`(thread-receive)]
    [(_ pat ...)
     ;parse the list of syntaxes and separate them into events and match clauses
     ;using the loop
     ;then transform the syntax into the actual call to matcher
     ;we need to remain tail recursive, so eatch event and match clause actually returns
     ;the continuation to invoke once the event or match is selected
     ;and we invoke that in tail position
     (let loop ([stx (syntax-e #`(pat ...))]
                [events empty]
                [match-clauses empty])
       (if (empty? stx)
           #`((mailbox-select (λ (msg)
                               ;if the match fails we return #f, and ignore the exception
                               (with-handlers ([exn:misc:match? (λ (exn) #f)])
                                 (match msg #,@(reverse match-clauses))))
                              #,@(reverse events)))
               
           (syntax-case (first stx) (event timeout when)
             [((event evt) code ...)
              (loop (rest stx)
                    (cons #`(wrap-evt evt
                                      (λ (evt) (λ () code ...)))
                          events)
                    match-clauses)]
             
             [((timeout time) code ...)
              (loop (rest stx)
                    (cons #`(wrap-evt (if time (alarm-evt (+ (current-inexact-milliseconds) (* time 1000))) never-evt)
                                      (λ (evt) (λ () code ...)))
                          events)
                    match-clauses)]
             
             [((when condition ...) code ...)
              (loop (rest stx)
                    (cons #`(wrap-evt (guard-evt (λ () (if ((λ () condition ...)) always-evt never-evt)))
                                        (λ (evt) (λ () code ...)))
                          events)
                    match-clauses)]
             
             [(match-clause match-code ...)
              (loop (rest stx)
                    events
                    ;here the match clause returns a closure over the code that gets invoked
                    ;after the match succeeds so it can be called from tail position
                    ;after the mailbox-select is performed
                    (cons #`(match-clause (λ () match-code ...))
                          match-clauses))])))]))