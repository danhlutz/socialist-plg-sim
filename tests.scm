(load "simtools.scm")

;; BASIC TEST PROCEDURES

(define (run-test test)
  (let ((result ((test-function test))))
    (if result
        (display ".")
        (begin (newline)
               (display "-------------------------------------------")
               (newline)
               (display "TEST FAILED: ")
               (display (test-name test))
               (newline)
               (display "-------------------------------------------")
               (newline)
               result))))

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

(define (fail-test msg)
  (lambda ()
    (newline)
    (newline)
    (display "[FAIL-TEST]--> ")
    (display msg)
    (newline)
    #f))

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

(define inventory-needed-test
  (make-test
    'inventory-needed-test
    (lambda ()
      (let ((test-hosp (make-test-hospital)))
        (let ((requirements (test-hosp 'requirements)))
          (let ((needed (inventory-needed requirements 10.0)))
            (and (= (needed-amount needed 'workers) 1.0)
                 (= (needed-amount needed 'scrubs) 1.0)
                 (= (needed-amount needed 'buildings) 10.0)
                 (= (needed-amount needed 'drugs) 5.0))))))))

(define subtract-inventory-test
  (make-test
    'subtract-inventory-test
    (lambda ()
      (let ((test-hosp (make-test-hospital)))
        (let ((requirements (test-hosp 'requirements))
              (inventory (test-hosp 'inventory)))
          (let ((needed (inventory-needed requirements 10.0)))
            (begin (subtract-inventory! inventory needed)
                   (and (= (check-stock inventory 'workers) 9.0)
                        (= (check-stock inventory 'drugs) 20.0)))))))))

;; PRODUCER TESTS

(define (make-test-hospital)
  (make-producer 'healthcare 
                 23
                 (make-inventory
                   (list
                     (make-inventory-entry 'workers 10 3)
                     (make-inventory-entry 'scrubs 32 3)
                     (make-inventory-entry 'buildings 50 3)
                     (make-inventory-entry 'drugs 25 100.0)))
                '((workers 0.1)
                  (scrubs 0.1)
                  (buildings 1.0)
                  (drugs 0.5))))

(define producer-has-name
  (make-test
    'test-producer-has-name
    (lambda () (eq? (product (make-test-hospital)) 'healthcare))))

(define test-producer-produces
  (make-test
    'test-producer-produces
    (lambda ()
      (let ((test-hosp (make-test-hospital)))
        (let ((starting-stock (producer-stock test-hosp)))
          (begin (test-hosp 'produce)
                 (= (- (producer-stock test-hosp) starting-stock)
                       50)))))))

(define production-exhausts-inventory
  (make-test
    'production-exhausts-inventory
    (lambda ()
      (let ((test-hosp (make-test-hospital))
            (first-round '*unassigned*))
        (begin (test-hosp 'produce)
               (set! first-round (producer-stock test-hosp))
               (test-hosp 'produce)
               (= (producer-stock test-hosp) first-round))))))

(define producer-saves-history
  (make-test
    'producer-saves-history
    (lambda ()
      (let ((test-hosp (make-test-hospital)))
        (let ((starting-stock (producer-stock test-hosp)))
          (begin (test-hosp 'produce)
                 (test-hosp 'produce)
                 (and
                   (= (cadr (test-hosp 'history))
                      (- (producer-stock test-hosp) starting-stock))
                   (= (car (test-hosp 'history)) 0))))))))

(define producer-receives-order
  (make-test
    'producer-receives-order
    (lambda ()
      (let ((test-hosp (make-test-hospital))
            (test-order1 (make-order 'healthcare 23 'workforce))
            (test-order2 (make-order 'healthcare 15 'steelmill)))
        (begin (take-order! test-hosp test-order1)
               (take-order! test-hosp test-order2)
               (test-hosp 'add-new-orders!)
               (= (amount-on-order test-hosp) 38))))))

(define amount-on-order-is-zero-at-start
  (make-test
    'amount-on-order-is-zero-at-start
    (lambda ()
      (let ((test-hosp (make-test-hospital)))
        (= (amount-on-order test-hosp) 0)))))

;; SHIPMENT TESTS

(define (make-test-shipment)
  (make-shipment 'drugs 15.3 'healthcare))

(define make-shipment-test
  (make-test
    'make-shipment-test
    (lambda ()
      (let ((shipment1 (make-shipment 'coal 34.2 'steel)))
        (and (eq? (shipment-product shipment1) 'coal)
             (eq? (shipment-to shipment1) 'steel)
             (= (shipment-amount shipment1) 34.2))))))

(define receive-shipment-test
  (make-test
    'receive-shipment-test
    (lambda ()
      (let ((shipment1 (make-test-shipment))
            (test-hosp (make-test-hospital)))
        (begin (receive-shipment! test-hosp shipment1)
               (= (check-producer-stock test-hosp 'drugs) 40.3))))))

(define shipment-reduces-ordered-amount
  (make-test
    'shipment-reduces-ordered-amount
    (lambda ()
      (let ((shipment1 (make-test-shipment))
            (test-hosp (make-test-hospital)))
        (begin (receive-shipment! test-hosp shipment1)
               (= (check-producer-ordered-amount test-hosp 'drugs)
                  (- 100.0 15.3)))))))

;; ORDER TESTS

(define (make-test-steelmill)
  (make-producer 'steel
                 23
                 (make-inventory
                   (list
                     (make-inventory-entry 'workers 20.0 7)
                     (make-inventory-entry 'buildings 100 3)
                     (make-inventory-entry 'machinery 20 5)
                     (make-inventory-entry 'electricity 32 100.0)
                     (make-inventory-entry 'ore 302 50)))
                '((workers 0.1)
                  (buildings 0.2)
                  (machinery 0.05)
                  (electricity 10.0)
                  (ore 1.2))))

(define (make-test-power-grid)
  (make-producer 'electricity 
                 300
                 (make-inventory
                   (list
                     (make-inventory-entry 'workers 0 0)
                     (make-inventory-entry 'buildings 100 10)
                     (make-inventory-entry 'coal 1.0 0) ;; for still days
                     (make-inventory-entry 'windmills 0 1)))
                '((workers 0.1)
                  (buildings 0.2)
                  (coal 0.01)
                  (windmills 0.1))))

(define (make-test-workforce)
  (make-producer 'workers
                 200
                 (list (make-inventory-entry 'healthcare 100 0)
                       (make-inventory-entry 'childcare 100 0)
                       (make-inventory-entry 'food 2500 0)
                       (make-inventory-entry 'buildings 1000 0))
                 '((healthcare 0.05)
                   (childcare 0.25)
                   (food 10.0)
                   (buildings 1.0))))

(define (make-test-buildings)
  (make-producer 'buildings
                 200
                 (list (make-inventory-entry 'workers 200 0)
                       (make-inventory-entry 'lumber 300 0))
                 '((workers 0.1) (lumber 2.0))))

(define (make-test-coalmine)
  (make-producer 'coal
                 200
                 (list (make-inventory-entry 'workers 200 0)
                       (make-inventory-entry 'tools 100 0))
                '((workers 0.2) (tools 0.3))))

(define (make-test-windmill-factory)
  (make-producer 'windmills
                 200
                 (list (make-inventory-entry 'workers 100 30)
                       (make-inventory-entry 'tools 50 0)
                       (make-inventory-entry 'steel 30 0))
                 '((workers 0.2) (tools 0.1) (steel 0.8))))

(define test-order-fulfilment
  (make-test
    'test-order-fulfilment
    (lambda ()
      (let ((dynamo (make-test-power-grid))
            (steelmill (make-test-steelmill))
            (test-order (make-order 'electricity 200 'steel)))
        (let ((test-economy (make-economy (list dynamo steelmill)
                                          '())))
          (begin (take-order! dynamo test-order)
                 (dynamo 'add-new-orders!)
                 (fulfil-orders! dynamo test-economy)
                 (and (= (producer-stock dynamo) 100)
                      (= (check-producer-stock steelmill 'electricity)
                         232))))))))

(define test-plan-creates-orders
  (make-test
    'test-plan-creates-orders
    (lambda ()
      (let ((dynamo (make-test-power-grid))
            (steelmill (make-test-steelmill))
            (workforce (make-test-workforce))
            (buildings (make-test-buildings))
            (coalmine (make-test-coalmine))
            (windmills (make-test-windmill-factory))
            (test-order (make-order 'electricity 600 'steel)))
        (let ((test-economy 
                (make-economy (list dynamo steelmill workforce
                                    buildings coalmine windmills) '())))
          (begin (take-order! dynamo test-order)
                 (plan! dynamo test-economy)
                 (test-economy 'append-new-orders!)
                 (and (= (amount-on-order workforce) 30.0)
                      (= (amount-on-order coalmine) 2.0))))))))

(define test-plan-adjusts-amount-ordered
  (make-test
    'test-plan-adjusts-amount-ordered
    (lambda ()
      (let ((dynamo (make-test-power-grid))
            (steelmill (make-test-steelmill))
            (workforce (make-test-workforce))
            (buildings (make-test-buildings))
            (coalmine (make-test-coalmine))
            (windmills (make-test-windmill-factory))
            (test-order (make-order 'electricity 600 'steel)))
        (let ((test-economy 
                (make-economy (list dynamo steelmill workforce
                                    buildings coalmine windmills) '())))
          (begin (take-order! dynamo test-order)
                 (plan! dynamo test-economy)
                 (and (= (check-producer-ordered-amount
                           dynamo 'workers) 
                         30.0)
                      (= (check-producer-ordered-amount
                           dynamo 'coal) 2.0))))))))

(define test-plan-doesnt-order-whats-been-ordered
  (make-test
    'test-plan-doesnt-order-whats-been-ordered
    (lambda ()
      (let ((dynamo (make-test-power-grid))
            (steelmill (make-test-steelmill))
            (workforce (make-test-workforce))
            (buildings (make-test-buildings))
            (coalmine (make-test-coalmine))
            (windmills (make-test-windmill-factory))
            (test-order (make-order 'electricity 600 'steel)))
        (let ((test-economy 
                (make-economy (list dynamo steelmill workforce
                                    buildings coalmine windmills) '())))
          (begin (take-order! dynamo test-order)
                 (plan! dynamo test-economy)
                 (test-economy 'append-new-orders!)
                 (and (= (check-producer-ordered-amount dynamo 'windmills)
                         30.0)
                      (= (amount-on-order windmills) 29.0))))))))

;; PUT YOUR TESTS HERE!

(define tests-to-run
  (list inventory-test-1
        test-ordered-amount
        test-inventory-search
        test-inventory-error
        producer-has-name
        inventory-needed-test
        subtract-inventory-test
        test-producer-produces
        production-exhausts-inventory
        producer-saves-history
        producer-receives-order
        amount-on-order-is-zero-at-start
        make-shipment-test
        receive-shipment-test
        shipment-reduces-ordered-amount
        test-order-fulfilment
        test-plan-creates-orders
        test-plan-adjusts-amount-ordered
        test-plan-doesnt-order-whats-been-ordered
        ))

(run-tests tests-to-run)