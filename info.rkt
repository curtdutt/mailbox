#lang setup/infotab

(define name "okcomps/mailbox: Thread mailbox utilities, including receive primitive.")

(define blurb
  '((p "okcomps/mailbox provides utilities for manipulating a thread's mailbox. It includes a receive primitive "
       "which is similar to erlang's receive.")))

(define release-notes
  '((p "1.0 (1 0) - first release")))

(define categories
  '(devtools misc))

(define homepage "https://github.com/curtdutt/mailbox")

(define required-core-version "4.0")

(define version "1.1")

(define repositories '("4.x"))

(define primary-file "main.rkt")
