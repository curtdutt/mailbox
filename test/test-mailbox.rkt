#lang racket/base



(require rackunit
         racket/function
         racket/list)

(require "../mailbox.rkt")




(test-case 
 "mailbox-clear"
 (for-each (curry thread-send (current-thread)) (list 'A 'B 'C 'D))
 
 (mailbox-clear)
 
 (check-false (thread-try-receive)))


(test-case
 "mailbox->list"
 (mailbox-clear)
 (for-each (curry thread-send (current-thread)) '(A B C D))
 
 (check-equal? (mailbox->list) '(A B C D))
 
 (check-equal? (thread-try-receive) 'A)
 
 (check-equal? (mailbox->list) '(B C D))
 
 (check-equal? (thread-try-receive) 'B)
 
 (check-equal? (mailbox->list) '(C D))
 
 (check-equal? (thread-try-receive) 'C)
 
 (check-equal? (mailbox->list) '(D))
 
 (check-equal? (thread-try-receive) 'D)
 
 (check-equal? (mailbox->list) empty)
 
 (check-false (thread-try-receive)))


   

(test-case 
 "mailbox-select"
 (mailbox-clear)
 (for-each (curry thread-send (current-thread)) '(A B C D))

 (check-equal? (mailbox-select (λ (x) (and (equal? x 'D) x))) 'D)

 (check-equal? (mailbox-select (λ (x) (and (equal? x 'B) x))) 'B)
 
 (check-equal? (mailbox->list) '(A C))
 
 (mailbox-clear)
 
 (for-each (curry thread-send (current-thread)) '(A B C D))
 
 #|
 (check-exn exn? (λ ()
                   (mailbox-select (λ (x)
                                     (if (equal? x 'A)
                                         (error 'oops)
                                         #f)))))
|#
 
 (check-equal? (mailbox->list) '(A B C D))

 (mailbox-clear)
 
 (check-equal? (mailbox-select (λ (x) #t) always-evt) always-evt))



(test-case
 "receive"
 (mailbox-clear)
 (for-each (curry thread-send (current-thread)) '(A B C D))
 
 (check-equal? (receive) 'A)
 
 (check-equal? (receive ['C 'C]) 'C)
 
 (check-equal? (receive ['D 'D]) 'D)
 
 #|
 (check-exn (λ (exn) (equal? exn 'foo))
            (λ ()
              (receive [(? (λ (x) (raise 'foo)) 'B)
                        'B])))
|#
 
 (check-equal? (receive) 'B)
 
 (mailbox-clear)
 
 (check-equal? (receive ((event (alarm-evt (+ (current-inexact-milliseconds) 100)))
                         'alarm))
               'alarm)
 
 (check-equal? (receive ['A 'A]
          
                        [(timeout 0)
                         'hello]
                        
                        ['C 'C])
               
               'hello))


(test-case
 "receive ? bug"
 (mailbox-clear)
 (thread-send (current-thread) 'a)
 
 (receive [(? (λ (x) #f) 'a)
           #f]
          [(timeout 0)
           #f])
 
 (check-equal? (mailbox->list) '(a)))


(test-case
 "receive when"
(mailbox-clear)

(check-true (receive [(when #t)
                      #t])))



(test-case
 "receive event value"
 
 (let ([evt (alarm-evt (+ (current-inexact-milliseconds) 100))])
   (check-equal? (receive ((event evt X)
                           X))
               evt)))
           
