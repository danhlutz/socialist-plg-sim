;; INVENTORY

(define (make-inventory-entry product in-stock ordered)
  (list product in-stock ordered))

(define (inventory-product inventory-entry) (car inventory-entry))

(define (in-stock inventory-entry) (cadr inventory-entry))

(define (ordered inventory-entry) (caddr inventory-entry))

(define (make-inventory items) items)

(define (get-inventory-item inventory product)
  (assq product inventory))

(define (subtract-inventory-item! item amount)
  (set-car! (cdr item) (- (in-stock item) amount))
  'done)

(define (add-inventory-item! item amount)
  (set-car! (cddr item) (- (ordered item) amount))
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
  (list product amount deliver-to))

(define (order-product order) (car order))
(define (order-amount order) (cadr order))
(define (order-deliver-to order) (caddr order))

(define (take-order! producer order)
  ((producer 'take-order!) order))

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
  (let ((history '())
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
      (set! orders (append orders new-orders))
      (set! new-orders '())
      'orders-appended)
    (define (amount-on-order-internal items)
      (fold-right + 0
                  (map order-amount items)))

    
    ;; STILL TO DO: 
    ;; --> PLAN --> take the amount on ordered and make orders
    ;; --> fulfil orders
    

    ;; SHIPMENTS GO HERE
    (define (receive-shipment-internal! shipment)
      (let ((product (shipment-product shipment)))
        (let ((inventory-item (get-inventory-item 
                                inventory product)))
          (add-inventory-item! 
            inventory-item (shipment-amount shipment))
          'done)))
    (define (make-shipments-internal order-list)
      (if (null? order-list)
          '()
          (let ((first-order (car order-list))
                (rest-orders (cdr order-list)))
            (if (<= (order-amount first-order) stock)
                (ship-and-process first-order rest-orders)
                (make-shipments-internal rest-orders)))))
    (define (ship-and-process first rest)
      (let ((shipment (make-shipment product
                                     (order-amount first)
                                     (order-deliver-to first))))
        (begin (set! stock (- stock (order-amount first)))
               (cons shipment
                     (make-shipments-internal rest)))))

    ;; DISPTACH
    (define (dispatch msg)
      (cond ((eq? msg 'product) product)
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
            ((eq? msg 'make-shipments) (make-shipments-internal orders))
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

;; PLAN-DRIVERS

;; define a plan-driver that can make and receive orders
;; make-plan-driver takes a product and a target function

;; THE ECONOMY

(define (make-economy producer-list plan-driver-list)
  (let ((units (map (lambda (x)
                      (list (x 'product) x))
                    (append producer-list plan-driver-list))))
    ;; LOOKUP-PRODUCER
    (define (lookup-producer shipment)
      (cadr (assq (shipment-to shipment) units)))
    ;; DISTRIBUTE SHIPMENTS
    (define (distribute-shipments-internal shipments)
      (if (null? shipments)
          'done
          (let ((first-shipment (car shipments))
                (rest-shipments (cdr shipments)))
            (let ((producer (lookup-producer first-shipment)))
              (begin (receive-shipment! producer first-shipment)
                     (distribute-shipments-internal rest-shipments))))))
    (define (dispatch msg)
      (cond ((eq? msg 'distribute-shipments) distribute-shipments-internal)
            (else 
              (error "Undefined message -- MAKE-ECONOMY" msg))))
    dispatch))

;; TO DO 
;; --> make an economy method
;; that handles production, orders, and reporting for producers

;; THE SIMULATION
;; simulate steps
;; --> produce 
;; --> fulfil orders
;; --> plan-and-order
;; --> report 
