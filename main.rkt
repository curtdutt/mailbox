#lang racket/base

(provide mailbox-select
         mailbox->list
         mailbox-clear
         receive
         try-receive)

(require racket/list
         racket/match
         (for-syntax racket/base)
         (for-syntax syntax/parse))

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

(define (mailbox-select #:match-fail-value (fail-value #f) proc . events)
  (parameterize-break 
   #f
   (let loop ([unmatched-messages empty])
     (let ([ready-event (call-with-exception-handler 
                         (λ (exn)
                           (thread-rewind-receive unmatched-messages)
                           exn)
                         (λ ()
                           (parameterize-break 
                            #t
                            (apply sync (thread-receive-evt) events))))])
       (if (equal? ready-event (thread-receive-evt))
           (let* ([next (thread-receive)]
                  [result
                   (call-with-exception-handler 
                    (λ (exn)
                      (thread-rewind-receive (cons next unmatched-messages))
                      exn)
                    (λ ()
                      (parameterize-break 
                       #t
                       (proc next))))])
             (if (equal? result fail-value)
                 (loop (cons next unmatched-messages))
                 (begin
                   (thread-rewind-receive unmatched-messages)
                   result)))
           (begin
             (thread-rewind-receive unmatched-messages)
             ready-event))))))
                                       

#|
Attempts to match and return the first message that does so.
If no matches occur, then the mailbox is left unchanged and mailbox-try-select returns #f
Match is function that takes a single value and returns either #:match-fail-value when it does not match or a value when it does match.
Mailbox-try-select returns the value returned by match
|#
(define (mailbox-try-select #:match-fail-value (fail-value #f) match)
  (parameterize-break 
   #f
   (let loop ([unmatched-messages empty])                              
     (let ([next (thread-try-receive)])
       (if next
           (let ([result (call-with-exception-handler 
                          (λ (exn)
                            (thread-rewind-receive (cons next unmatched-messages))
                            exn)
                          (λ ()
                            (parameterize-break #t (match next))))])
             (if (equal? result fail-value)
                 (loop (cons next unmatched-messages))
                 (begin
                   (thread-rewind-receive unmatched-messages)
                   result)))
           (begin
             (thread-rewind-receive unmatched-messages)
             #f))))))
                   

(begin-for-syntax
  (define-syntax-class match-clause
    #:description "match-clause"
    (pattern (pat body ...+)
             #:with match-code #'(pat (λ () body ...))))
  
  (define-syntax-class when-clause
    #:description "when-clause"
    (pattern (((~literal when) condition) code ...+)
             #:with event-code #'(handle-evt (guard-evt 
                                              (λ () 
                                                (if condition always-evt never-evt)))
                                             (λ (evt) 
                                               (λ () code ...)))))
  
  (define-syntax-class timeout-clause
    #:description "timeout-clause"
    (pattern (((~literal timeout) time) code ...+)
             #:with event-code #'(handle-evt (guard-evt 
                                              (λ () 
                                                (if time (alarm-evt (+ (current-inexact-milliseconds) (* time 1000))) never-evt)))
                                             (λ (evt) 
                                               (λ () code ...)))))
  
  (define-syntax-class event-clause
    #:description "event-claues"
    (pattern (((~literal event) evt) code ...+)
             #:with event-code #'(handle-evt evt           
                                             (λ (e)
                                               (λ () code ...))))
    (pattern (((~literal event) evt id:id) code ...+)
             #:with event-code #'(handle-evt evt           
                                             (λ (e)
                                               (λ () ((λ (id) code ...) e)))))))
  
(define no-match (gensym 'no-match))


    
(define-syntax (receive stx)
  (syntax-parse stx
    [(_)
     #'(thread-receive)]
    [(_ (~or E:event-clause T:timeout-clause W:when-clause M:match-clause) ...)
     #`((mailbox-select #:match-fail-value no-match
                       (λ (msg) 
                         (match msg
                           #,@#'(M.match-code ...)
                           [_
                            no-match]))
                       #,@(syntax->list #'(E.event-code ... T.event-code ... W.event-code ...))))]))


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