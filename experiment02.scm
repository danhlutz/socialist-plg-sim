;; EXPERIMENT 02
;; does the stride of a plan target increase with more producers?
(load "simtools")

(define (make-4-prod)
  (make-economy
    (list
      (make-producer 'a
                     1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'c 0 0))
                     '((b 0.9) (c 0.7)))
      (make-producer 'b 1 
                     (list (make-inventory-entry 'c 0 0)
                           (make-inventory-entry 'd 0 0))
                     '((c 0.1) (d 0.2)))
      (make-producer 'c 1
                     (list (make-inventory-entry 'b 2 0)
                           (make-inventory-entry 'd 1 0))
                     '((b 0.1) (d 0.2)))
      (make-producer 'd 1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'c 0 0))
                     '((b 0.03) (c 0.02))))
    (list (make-plan-driver 'a-driver 'a 1000000))))

(define (make-6-prod)
  (make-economy
    (list
      (make-producer 'a
                     1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'c 0 0))
                     '((b 0.9) (c 0.7)))
      (make-producer 'b 1 
                     (list (make-inventory-entry 'c 0 0)
                           (make-inventory-entry 'd 0 0))
                     '((c 0.1) (d 0.2)))
      (make-producer 'c 1
                     (list (make-inventory-entry 'e 2 0)
                           (make-inventory-entry 'd 1 0))
                     '((e 0.1) (d 0.2)))
      (make-producer 'd 1
                     (list (make-inventory-entry 'e 0 0)
                           (make-inventory-entry 'f 0 0))
                     '((e 0.1) (f 0.2)))
      (make-producer 'e 1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'f 0 0))
                     '((b 0.1) (f 0.2)))
      (make-producer 'f 1
                     (list (make-inventory-entry 'c 0 0)
                           (make-inventory-entry 'e 0 0))
                     '((c 0.03) (e 0.02))))
    (list (make-plan-driver 'a-driver 'a 1000000))))

(define (make-8-prod)
  (make-economy
    (list
      (make-producer 'a
                     1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'c 0 0))
                     '((b 0.9) (c 0.7)))
      (make-producer 'b 1 
                     (list (make-inventory-entry 'c 0 0)
                           (make-inventory-entry 'd 0 0))
                     '((c 0.1) (d 0.2)))
      (make-producer 'c 1
                     (list (make-inventory-entry 'e 2 0)
                           (make-inventory-entry 'd 1 0))
                     '((e 0.1) (d 0.2)))
      (make-producer 'd 1
                     (list (make-inventory-entry 'e 0 0)
                           (make-inventory-entry 'f 0 0))
                     '((e 0.1) (f 0.2)))
      (make-producer 'e 1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'f 0 0))
                     '((b 0.1) (f 0.2)))
      (make-producer 'f 1
                     (list (make-inventory-entry 'g 0 0)
                           (make-inventory-entry 'h 0 0))
                     '((g 0.1) (h 0.2)))
      (make-producer 'g 1
                     (list (make-inventory-entry 'f 0 0)
                           (make-inventory-entry 'h 0 0))
                     '((f 0.1) (h 0.2)))
      (make-producer 'h 1
                     (list (make-inventory-entry 'c 0 0)
                           (make-inventory-entry 'f 0 0))
                     '((c 0.03) (f 0.02))))
    (list (make-plan-driver 'a-driver 'a 1000000))))

(define (run-exp2)
  (let ((test-4-econ (make-4-prod))
        (test-6-econ (make-6-prod))
        (test-8-econ (make-8-prod)))
    (begin
      (newline)
      (display "Running 4-chain economy 50,000 steps")
      (run-n-times test-4-econ 50000 1)
      (newline)
      (display "Running 6-chain economy 50,000 steps")
      (run-n-times test-6-econ 50000 1)
      (newline)
      (display "Running 8-chain economy 50,000 steps")
      (run-n-times test-8-econ 50000 1))))

;; RESULTS
;; increasing the number of producers does reduce the plan target
;; and increase the stride or production 
;; For the very limited example I set, the production appears to decline
;; fairly linearly, althouth I suspect the rate of decline slows.
;; I need to experiment with larger production chains
