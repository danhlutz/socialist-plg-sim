;; INVENTORY

(define (make-inventory-entry product in-stock ordered)
  (cond ((< in-stock 0)
         (error "Stock cannot be less than 0 -- MAKE-INVENTORY-ENTRY"))
        ((< ordered 0)
         (error
           "Ordered amount cannot be less than 0 -- MAKE-INVENTORY-ENTRY"))
        (else (list product in-stock ordered))))

(define (inventory-product inventory-entry) (car inventory-entry))

(define (in-stock inventory-entry) (cadr inventory-entry))

(define (ordered inventory-entry) (caddr inventory-entry))

(define (make-inventory items) items)

(define (get-inventory-item inventory product)
  (assq product inventory))

(define (set-amount-ordered! item amount-ordered)
  (if (< amount-ordered 0)
      (error "Amount ordered can't be < 0 -- SET-AMOUNT-ORDERED!")
      (set-car! (cddr item) amount-ordered)))

(define (subtract-inventory-item! item amount)
  (let ((new-value (max 0 (- (in-stock item) amount))))
    (if (< new-value 0)
        (error "New value less than 0 -- SUBTRACT-INVENTORY-ITEM!")
        (begin 
          (set-car! (cdr item) (- (in-stock item) amount))
          'done))))

(define (add-inventory-item! item amount)
  (define (calc-new-inv-amount)
    (max 0 (- (ordered item) amount)))
  (set-amount-ordered! item (calc-new-inv-amount))
  (set-car! (cdr item) (+ (in-stock item) amount))
  'done)

(define (check-stock inventory item)
  (let ((item-entry (assq item inventory)))
    (if item-entry
        (in-stock item-entry)
        'item-not-in-inventory)))

(define (check-producer-stock producer item)
  (check-stock (producer 'inventory) item))

(define (check-producer-ordered-amount producer ordered-item)
  (let ((inventory (producer 'inventory)))
    (let ((item (assq ordered-item inventory)))
      (ordered item))))

(define (subtract-inventory! inventory needs)
  (if (null? needs)
      'subtraction-done
      (let ((first-need (car needs))
            (rest-needs (cdr needs)))
        (let ((inventory-record (assq (car first-need) inventory)))
          (subtract-inventory-item! inventory-record (cadr first-need))
          (subtract-inventory! inventory rest-needs)))))

;; REQUIREMENTS

(define (make-requirement item amount)
  (list item amount))

(define (requirement-amount requirement) (cadr requirement))
(define (requirement-item requirement) (car requirement))
(define (make-requirement-list items) items)

(define (requirement-factor requirements item)
  (let ((requirement (assq item requirements)))
    (if requirement
        (requirement-amount requirement)
        (error "Requirement not listed -- REQUIREMENT-FACTOR" item))))

(define (needed-amount needed item)
  (let ((this-need (assq item needed)))
    (if this-need
        (cadr this-need)
        (error "Need not found -- NEEDED-AMOUNT" item))))

(define (make-need item amount)
  (list item amount))

(define (inventory-needed requirements amount)
  (if (null? requirements)
      '()
      (let ((first-requirement (car requirements))
            (rest-requirements (cdr requirements)))
        (cons (make-need (requirement-item first-requirement)
                         (* amount (requirement-amount first-requirement)))
              (inventory-needed rest-requirements amount)))))

;; ORDERS

(define (make-order product amount deliver-to)
  (if (< amount 0)
      (make-order product 0 deliver-to)
      (list product amount deliver-to)))

(define (order-product order) (car order))
(define (order-amount order) (cadr order))
(define (order-deliver-to order) (caddr order))

(define (take-order! producer order)
  ((producer 'take-order!) order))

(define (sort-orders orders)
  (if (< (length orders) 2)
      orders
      (sort-by-pivot orders)))

(define (sort-by-pivot orders)
  (let ((pivot (car orders))
        (rest (cdr orders)))
    (let ((split-orders (split rest pivot '() '())))
      (let ((smaller (car split-orders))
            (greater (cadr split-orders)))
        (append (sort-orders smaller)
                (cons pivot (sort-orders greater)))))))

(define (split orders pivot smaller greater)
  (cond ((null? orders)
         (list smaller greater))
        ((< (order-amount (car orders)) (order-amount pivot))
         (split (cdr orders) pivot (cons (car orders) smaller) greater))
        (else
          (split (cdr orders) pivot smaller (cons (car orders) greater)))))

;; SHIPMENTS

(define (make-shipment product amount to)
  (list product amount to))

(define (shipment-product shipment) (car shipment))
(define (shipment-amount shipment) (cadr shipment))
(define (shipment-to shipment) (caddr shipment))

(define (receive-shipment! producer shipment)
  ((producer 'receive-shipment!) shipment))

;; PRODUCERS

(define (make-producer product stock inventory requirements)
  (let ((history '(*init*))
        (orders '())
        (new-orders '()))
    ;; PRODUCE
    (define (calc-production inventory requirements)
      (let ((first-inventory-item (car inventory))
            (rest-inventory (cdr inventory)))
        (let ((first-estimate
                (/ (in-stock first-inventory-item)
                   (requirement-factor 
                     requirements
                     (inventory-product first-inventory-item)))))
          (cond ((null? rest-inventory) first-estimate)
                (else 
                  (min first-estimate
                       (calc-production rest-inventory requirements)))))))
    (define (produce-internal inventory requirements)
      (let ((amount (calc-production inventory requirements)))
        (begin (set! stock (+ amount stock))
               (subtract-inventory! inventory 
                                    (inventory-needed requirements amount))
               (set! history (cons amount history))
               'production-done)))
    ;; ORDERS
    (define (take-order-internal! order)
      (set! new-orders (cons order new-orders))
      'order-taken)
    (define (add-new-orders!)
      (set! orders (sort-orders (append orders new-orders)))
      (set! new-orders '())
      'orders-appended)
    (define (amount-on-order-internal items)
      (fold-right + 0
                  (map order-amount items)))
    ;; SHIPMENTS GO HERE
    (define (receive-shipment-internal! shipment)
      (let ((product (shipment-product shipment)))
        (let ((inventory-item (get-inventory-item 
                                inventory product)))
          (add-inventory-item! 
            inventory-item (shipment-amount shipment))
          'done)))
    (define (make-shipments-internal2)
      (if (null? orders)
          '()
          (let ((first-order (car orders)))
            (if (<= (order-amount first-order) stock)
                (ship-and-process2 first-order)
                '()))))
    (define (ship-and-process2 order)
      (let ((rest-orders (cdr orders))
            (new-stock (- stock (order-amount order)))
            (shipment (make-shipment product
                                     (order-amount order)
                                     (order-deliver-to order))))
        (begin (set! stock new-stock)
               (set! orders rest-orders)
               (cons shipment (make-shipments-internal2)))))
    ;; PLAN
    (define (calc-shortfall)
      (max 0 (- (amount-on-order-internal orders) stock)))
    (define (plan-production-internal)
      (let ((shortfall (calc-shortfall)))
        (if (= shortfall 0)
            '()
            (generate-orders (+ 1 shortfall) requirements))))
    (define (generate-orders shortfall required-items)
      (if (null? required-items)
          '()
          (let ((first-req (car required-items))
                (rest-reqs (cdr required-items)))
            (let ((first-needed (calculate-order-amount 
                                  first-req shortfall)))
              (if (> first-needed 0)
                  (cons (make-planned-order first-req first-needed)
                        (generate-orders shortfall rest-reqs))
                  (generate-orders shortfall rest-reqs))))))
    (define (calculate-order-amount requirement amount)
      (let ((inventory-item (assq (requirement-item requirement)
                                  inventory)))
        (let ((amount-in-stock (in-stock inventory-item))
              (amount-ordered (ordered inventory-item)))
          (- (* amount (requirement-amount requirement))
             (+ amount-in-stock amount-ordered)))))
    (define (make-planned-order requirement amount-needed)
      (let ((inventory-item (assq (requirement-item requirement)
                                  inventory)))
        (begin
          (set-amount-ordered! inventory-item
                               (+ (ordered inventory-item) amount-needed))
          (make-order (requirement-item requirement)
                      amount-needed
                      product))))
    ;; DISPTACH
    (define (dispatch msg)
      (cond ((< stock 0) 
             (error "Stock cannot be less than zero -- MAKE-PRODUCER"))
            ((eq? msg 'product) product)
            ((eq? msg 'order-list) orders)
            ((eq? msg 'producer-stock) stock)
            ((eq? msg 'requirements) requirements)
            ((eq? msg 'inventory) inventory)
            ((eq? msg 'produce) 
             (produce-internal inventory requirements))
            ((eq? msg 'history) history)
            ((eq? msg 'take-order!) take-order-internal!)
            ((eq? msg 'add-new-orders!) (add-new-orders!))
            ((eq? msg 'amount-on-order)
             (amount-on-order-internal orders))
            ((eq? msg 'receive-shipment!) receive-shipment-internal!)
            ((eq? msg 'make-shipments) (make-shipments-internal2))
            ((eq? msg 'plan-production) (plan-production-internal))
            (else (error "Undefined message -- MAKE-PRODUCER" msg))))
    dispatch))

(define (product producer)
  (producer 'product))

(define (producer-stock producer)
  (producer 'producer-stock))

(define (amount-on-order producer)
  (producer 'amount-on-order))

(define (fulfil-orders! producer economy)
  (let ((shipments (producer 'make-shipments)))
    ((economy 'distribute-shipments) shipments)))

(define (plan! producer economy)
  (let ((orders (producer 'plan-production)))
    ((economy 'distribute-orders!) orders)))

;; PLAN-DRIVERS

(define (make-plan-driver name consumes target)
  (let ((current-target 1)
        (orders-sent 0))
    (define (make-next-order)
      (make-order consumes current-target name))
    ;; PLAN-PRODUCTION!
    (define (plan-production-internal!)
      (if (= orders-sent 0)
          (begin (set! orders-sent (+ 1 orders-sent))
                 (list (make-next-order)))
          '()))
    ;; RECEIVE SHIPMENTS and reset flag adjust current-target
    (define (set-new-target)
      (if (< current-target target)
          (set! current-target (+ 1 current-target))
          (set! current-target target)))
    (define (receive-shipment-internal! shipment)
      (begin (set! orders-sent (- orders-sent 1))
             (set-new-target)))
    ;; DISPATCH
    (define (dispatch msg)
      (cond ((eq? msg 'product) name)
            ((eq? msg 'make-next-order) (make-next-order))
            ((eq? msg 'plan-production) (plan-production-internal!))
            ((eq? msg 'add-new-orders!) 'i-take-orders-from-no-one)
            ((eq? msg 'send-orders?) orders-sent)
            ((eq? msg 'receive-shipment!) receive-shipment-internal!)
            ((eq? msg 'current-target) current-target)
            (else 
              (error "Undefined msg -- MAKE-PLAN-DRIVER" msg))))
    dispatch))

;; define a plan-driver that can make and receive orders
;; make-plan-driver takes a product and a target function

;; THE ECONOMY

(define (make-unit producer)
  (list (producer 'product) producer))

(define (unit-product unit) (car unit))
(define (unit-producer unit) (cadr unit))

(define (make-economy producer-list plan-driver-list)
  (let ((units (map make-unit
                    (append producer-list plan-driver-list))))
    ;; LOOKUP-PRODUCER
    (define (lookup-producer product)
      (unit-producer (assq product units)))
    (define (get-units)
      (map (lambda (x) (unit-producer x)) units))
    ;; APPEND NEW ORDERS
    (define (append-new-orders-internal!)
      (begin (map (lambda (unit) ((unit-producer unit) 'add-new-orders!)) 
                  units)
             'done))
    ;; DISTRIBUTE SHIPMENTS
    (define (distribute-shipments-internal shipments)
      (if (null? shipments)
          'done
          (let ((first-shipment (car shipments))
                (rest-shipments (cdr shipments)))
            (let ((producer (lookup-producer (shipment-to first-shipment))))
              (begin (receive-shipment! producer first-shipment)
                     (distribute-shipments-internal rest-shipments))))))
    ;; DISTRIBUTE ORDERS
    (define (distribute-orders-internal! orders)
      (if (null? orders)
          'done
          (let ((first-order (car orders)) 
                (rest-orders (cdr orders)))
            (let ((producer (lookup-producer (order-product first-order))))
              (begin (take-order! producer first-order)
                     (distribute-orders-internal! rest-orders))))))
    (define (dispatch msg)
      (cond ((eq? msg 'distribute-shipments) distribute-shipments-internal)
            ((eq? msg 'distribute-orders!) distribute-orders-internal!)
            ((eq? msg 'append-new-orders!) (append-new-orders-internal!))
            ((eq? msg 'producers) producer-list)
            ((eq? msg 'units) (get-units))
            ((eq? msg 'units-with-names) units)
            ((eq? msg 'driver) (car plan-driver-list))
            (else 
              (error "Undefined message -- MAKE-ECONOMY" msg))))
    dispatch))

;; THE SIMULATION

(define (sim-step! economy step)
  (let ((producers (economy 'producers))
        (units (economy 'units)))
    ;; GET ALL UNITS LATER -- will need to map out just the units
    (begin
      ;; PRODUCE
      (map (lambda (producer) (producer 'produce)) producers)
      ;; FULFIL ORDERS
      (map (lambda (producer) (fulfil-orders! producer economy))
           producers)
      ;; PLAN!
      (map (lambda (unit) (plan! unit economy)) units)
      ;; APPEND NEW ORDERS
      (economy 'append-new-orders!)
      ;; TODO
      ;; report
      'done)))

(define (simulate-til economy steps)
  (define (iter economy steps counter)
    (if (> counter steps)
        economy
        (begin (sim-step! economy counter)
               (iter economy steps (+ 1 counter)))))
  (iter economy steps 0))

(define (get-producer name economy)
  (let ((units (economy 'units-with-names)))
    (unit-producer (assq name units))))

(define (last-report producer)
  (car (producer 'history)))

(define (target-status economy)
  (let ((driver (economy 'driver)))
    (driver 'current-target)))

(define (plan-stride economy)
  (define (get-first-target history)
    (cond ((null? history) '())
          ((= (car history) 0) (get-first-target (cdr history)))
          (else (cdr history))))
  (define (stride history)
    (cond ((null? history) '*no-stride*)
          ((= (car history) 0) (+ 1 (stride (cdr history))))
          (else 1)))
  (let ((a (get-producer 'a economy)))
    (stride (get-first-target (a 'history)))))

(define (display-result economy counter period)
  (newline)
  (display "[")
  (display (* period (+ 1 counter)))
  (display "] ")
  (display "CURRENT-TARGET: ")
  (display (target-status economy))
  (display " | STRIDE: ")
  (display (plan-stride economy))
  (display " | AVG: ")
  (display (* 1.0 (/ (target-status economy) 
                     (plan-stride economy)))))

(define (run-n-times economy period n)
  (define (iter economy period counter)
    (if (> counter n)
        'done-planning
        (begin
          (simulate-til economy period)
          (display-result economy counter period)
          (iter  economy period (+ counter 1)))))
  (iter economy period 0))
