(load "sim.scm")

;; BASIC TEST PROCEDURES

(define (run-test test)
  (let ((result ((test-function test))))
    (begin 
      (newline)
      (display "Testing: ")
      (display (test-name test))
      (display " - result - ")
      (display result)
      result)))

(define (run-tests tests)
  (define (iter tests pass fail)
    (if (null? tests)
        (list 'passed pass 'failed fail)
        (let ((first-result (run-test (car tests))))
          (if first-result
              (iter (cdr tests) (+ 1 pass) fail)
              (iter (cdr tests) pass (+ 1 fail))))))
  (iter tests 0 0))

(define (make-test name function)
  (list name function))

(define (test-name test) (car test))
(define (test-function test) (cadr test))

(define smoke-1
  (make-test
    'smoke-1
    (lambda () (= 1 1))))



;; PUT YOUR TESTS HERE!

(define tests-to-run
  (list smoke-1
        
        ))

(run-tests tests-to-run)
