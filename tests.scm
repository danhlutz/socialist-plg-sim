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

(define negative-order-sends-signal
  (make-test
    'negative-order-sends-signal
    (lambda ()
      (let ((order1 (make-order 'manure -30 'farm)))
        (= (order-amount order1) 0)))))

;; PLANNING TESTS

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
                 (test-economy 'append-new-orders!)
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
                 (test-economy 'append-new-orders!)
                 (plan! dynamo test-economy)
                 (test-economy 'append-new-orders!)
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
                 (test-economy 'append-new-orders!)
                 (plan! dynamo test-economy)
                 (test-economy 'append-new-orders!)
                 (and (= (check-producer-ordered-amount dynamo 'windmills)
                         30.0)
                      (= (amount-on-order windmills) 29.0))))))))

(define zero-orders-not-delivered
  (make-test
    'zero-orders-not-delivered
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
                 (= (length (buildings 'order-list)) 0)))))))

(define zero-orders-dont-affect-inventory-amount-ordered
  (make-test
    'zero-orders-dont-affect-inventory
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
                 (let ((on-order-bldgs
                         (check-producer-ordered-amount
                           dynamo
                           'buildings)))
                   (plan! dynamo test-economy)
                   (test-economy 'append-new-orders!)
                   (= on-order-bldgs
                      (check-producer-ordered-amount
                        dynamo
                        'buildings)))))))))

;; PLAN DRIVERS

(define (make-test-driver)
  (make-plan-driver 'healthcare-driver 'healthcare 20))

(define plan-driver-takes-product-message
  (make-test
    'plan-driver-takes-product-message
    (lambda ()
      (let ((test-driver 
              (make-plan-driver 'healthcare-driver 'healthcare 20)))
        (eq? (test-driver 'product) 'healthcare-driver)))))

(define plan-driver-can-make-order
  (make-test
    'plan-driver-can-make-order
    (lambda ()
      (let ((test-driver (make-test-driver)))
        (let ((test-order (test-driver 'make-next-order)))
          (and (eq? (order-product test-order) 'healthcare)
               (= (order-amount test-order) 1)
               (eq? (order-deliver-to test-order)
                    'healthcare-driver)))))))

(define plan-driver-emits-orders-on-plan!
  (make-test
    'plan-driver-emits-orders-on-plan!
    (lambda ()
      (let ((test-driver (make-test-driver))
            (test-hospital (make-test-hospital)))
        (let ((test-economy 
                (make-economy (list test-hospital)
                              (list test-driver))))
          (begin (plan! test-driver test-economy)
                 (test-economy 'append-new-orders!)
                 (= (amount-on-order test-hospital) 1)))))))

(define plan-driver-receives-shipments
  (make-test
    'plan-driver-receives-shipments
    (lambda ()
      (let ((test-driver (make-test-driver))
            (test-hospital (make-test-hospital)))
        (let ((test-economy
                (make-economy (list test-hospital) (list test-driver))))
          (begin (plan! test-driver test-economy)
                 (test-economy 'append-new-orders!)
                 (eq? (test-driver 'send-orders?) #f)
                 (fulfil-orders! test-hospital test-economy)
                 (and (eq? (test-driver 'send-orders?) #t)
                      (= (test-driver 'current-target) 2))))))))

;; SIMULATION TESTS

(define (make-abc-producers)
  (list
    (make-producer 'a
                   20
                   (list (make-inventory-entry 'b 0 0)
                         (make-inventory-entry 'c 0 0))
                   '((b 0.7) (c 0.9)))
    (make-producer 'b 10
                   (list (make-inventory-entry 'a 0 0)
                         (make-inventory-entry 'c 0 0))
                   '((a 0.3) (c 0.5)))
    (make-producer 'c 0
                   (list (make-inventory-entry 'a 2 0)
                         (make-inventory-entry 'b 1 0))
                   '((a 0.1) (b 0.1)))))

(define (make-c-driver)
  (make-plan-driver 'a-driver 'a 20))

(define (make-abc-economy) 
  (make-economy (make-abc-producers) (list (make-c-driver))))

(define sim-step-produces
  (make-test
    'sim-step-produces
    (lambda ()
      (let ((test-economy (make-abc-economy)))
        (let ((c (caddr (test-economy 'producers))))
          (begin (sim-step! test-economy 0)
                 (and (= (producer-stock c) 10)
                      (= (check-producer-stock c 'a) 1)
                      (= (check-producer-stock c 'b) 0))))))))

(define sim-step-fulfils-orders
  (make-test
    'sim-step-fulfils-orders
    (fail-test "Write fulfilment")))

(define sim-step-plans-and-orders
  (make-test
    'sim-step-plans-and-orders
    (fail-test "Make sure plans are made, orders ordered")))

(define sim-step-plan-driver-creates-orders
  (make-test
    'sim-step-plan-driver-creates-orders
    (lambda ()
      (let ((test-economy (make-abc-economy)))
        (let ((a (car (test-economy 'producers)))
              (b (cadr (test-economy 'producers)))
              (c (caddr (test-economy 'producers))))
          (begin (sim-step! test-economy 0)
                 (= (amount-on-order a) 1)))))))

(define sim-step-reports
  (make-test
    'sim-step-reports
    (fail-test "Save the report of each producer and print if desired")))

;; PUT YOUR TESTS HERE!

(define tests-to-run
  (list inventory-test-1
        test-ordered-amount
        test-inventory-search
        test-inventory-error
        ;; producer tests
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
        negative-order-sends-signal
        test-order-fulfilment
        test-plan-creates-orders
        test-plan-adjusts-amount-ordered
        test-plan-doesnt-order-whats-been-ordered
        zero-orders-not-delivered
        zero-orders-dont-affect-inventory-amount-ordered
        ;; PLAN DRIVER TESTS
        plan-driver-takes-product-message
        plan-driver-can-make-order
        plan-driver-emits-orders-on-plan!
        plan-driver-receives-shipments
        ;; SIMULATION STEPS
        sim-step-produces
        sim-step-fulfils-orders
        sim-step-plans-and-orders
        sim-step-plan-driver-creates-orders
        sim-step-reports
        ))

(run-tests tests-to-run)
