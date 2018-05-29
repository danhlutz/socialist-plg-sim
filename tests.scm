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

;; INVENTORY TESTS

(define inventory-test-1
  (make-test
    'inventory-test-1
    (lambda ()
      (let ((inv (make-inventory-entry 'steel 13 15)))
        (= (in-stock inv) 13)))))

(define test-ordered-amount
  (make-test 
    'test-ordered-amount
    (lambda ()
      (let ((inv (make-inventory-entry 'coal 35 14)))
        (= (ordered inv) 14)))))

(define test-inventory-search
  (make-test
    'test-inventory-search
    (lambda ()
      (let ((inventory
              (make-inventory
                (list 
                  (make-inventory-entry 'coal 36 10)
                  (make-inventory-entry 'steel 100 23)
                  (make-inventory-entry 'workers 35 0)))))
        (= (check-stock inventory 'steel) 100)))))

(define test-inventory-error
  (make-test
    'test-inventory-error
    (lambda ()
      (let ((inventory
              (make-inventory
                (list (make-inventory-entry 'socks 13 3)
                      (make-inventory-entry 'undies 3 0)))))
        (eq? 'item-not-in-inventory
             (check-stock inventory 'steel))))))

;; PRODUCER TESTS

(define producer-has-name
  (make-test
    'test-producer-has-name
    (lambda ()
      (let ((hospital
              (make-producer 'healthcare 
                             23
                             (list 'workers 0.1
                                   'scrubs 0.1
                                   'buildings 1.0
                                   'drugs 0.5))))
        (eq? (product hospital) 'healthcare)))))


;; PUT YOUR TESTS HERE!

(define tests-to-run
  (list inventory-test-1
        test-ordered-amount
        test-inventory-search
        test-inventory-error
        producer-has-name
        ))

(run-tests tests-to-run)
