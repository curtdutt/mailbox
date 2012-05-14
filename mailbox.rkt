#lang racket/base

(provide mailbox-select
         mailbox->list
         mailbox-clear
         receive
         try-receive)

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
    (let ([ready-event (apply sync (thread-receive-evt) events)])
      (cond [(equal? ready-event (thread-receive-evt))
             (let ([next (thread-receive)])
               ;when an exception occurs in a predicate
               ;we must rewind all messages back into the mailbox
               ;and then raise the exception
               (cond [(match next) =>
                                   (λ (result)
                                     (thread-rewind-receive unmatched-messages)
                                     result)]
                     [else
                      (loop (cons next unmatched-messages))]))]
            [else
             (thread-rewind-receive unmatched-messages)
             ready-event]))))

#|
Attempts to match and return the first message that does so.

If no matches occur, then the mailbox is left unchanged and mailbox-try-select returns #f


Match is function that takes a single value and returns either #f when it does not match or a value when it does match.

Mailbox-try-select returns what match returns 
|#
(define (mailbox-try-select match)
   (let loop ([unmatched-messages empty])                              
     (let ([next (thread-try-receive)])
       (if next
           (cond [(match next) =>
                               (λ (result)
                                 (thread-rewind-receive unmatched-messages)
                                 result)]
                 [else
                  (loop (cons next unmatched-messages))])
           (begin
             (thread-rewind-receive unmatched-messages)
             #f)))))
                               
       



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
     (let ([code (let loop ([stx (syntax-e #`(pat ...))]
                            [events empty]
                            [match-clauses empty])
                   (if (empty? stx)
                       #`((mailbox-select (λ (msg)
                                            ;if the match fails we return #f, and ignore the exception
                                            (match msg #,@(reverse (cons #`(_ #f) match-clauses))))
                                          #,@(reverse events)))
                       
                       (syntax-case (first stx) (event timeout when)
                         [((event evt) code ...)
                          (loop (rest stx)
                                (cons #`(wrap-evt evt
                                                  (λ (evt) (λ () code ...)))
                                      events)
                                match-clauses)]
                         
                         [((event evt id) code ...)
                          (loop (rest stx)
                                (cons #`(wrap-evt evt
                                                  (λ (res) (λ () 
                                                             (let ([id res])
                                                               code ...))))
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
                                      match-clauses))])))])
       code)]))


#|
Non blocking, returns the first matching message or #f if no matches exist within the mailbox.
|#
(define-syntax (try-receive stx)
  (syntax-case stx ()
    [(_)
     #`(thread-try-receive)]
    [(_ pat ...)
     #`(mailbox-try-select (λ (msg)
                             (match msg pat ... (_ #f))))]))