;; EXPERIMENT 01
;; does the amount of time between plan targets (the "stride")
;; increase over time? 
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
                     '((b 0.1) (d 0.1)))
      (make-producer 'd 1
                     (list (make-inventory-entry 'b 0 0)
                           (make-inventory-entry 'c 0 0))
                     '((b 0.03) (c 0.02))))
    (list (make-plan-driver 'a-driver 'a 1000000))))

(define (run-exp1)
  (let ((test-econ (make-4-prod)))
    (begin
      (newline)
      (display "Running 4-chain economy 10,000 steps 25 times")
      (run-n-times test-econ 10000 25))))

;; RESULT
;; running this test shows too things:
;;
;; 1) the plan target attained rises ALMOST linearly over time.
;;    the plan target is approximately equal to
;;
;;        target ~ 0.883 * t / ln(t)
;;
;; 2) the stride increases very slowly.
;;    this implies that even though the plan target keeps rising, over
;;    time it slows down, almost imperceptibly
