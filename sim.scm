;; INVENTORY

(define (make-inventory-entry product in-stock ordered)
  (list product in-stock ordered))

(define (inventory-product inventory-entry) (car inventory-entry))

(define (in-stock inventory-entry) (cadr inventory-entry))

(define (ordered inventory-entry) (caddr inventory-entry))

(define (make-inventory items) items)

(define (subtract-inventory-item! item amount)
  (set-car! (cdr item) (- (in-stock item) amount))
  'done)

(define (check-stock inventory item)
  (let ((item-entry (assq item inventory)))
    (if item-entry
        (in-stock item-entry)
        'item-not-in-inventory)))

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

(define (make-order product amount)
  (list product amount))

(define (order-product order) (car order))
(define (order-amount order) (cadr order))

(define (take-order! producer order)
  ((producer 'take-order!) order))

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
    ;; TODOS
    ;; ** need to figure out correct interface between orders and delivery /
    ;; -> deliver-order
    ;;    process an order, decrement stock
    ;; -> make-order
    ;;    estimate what will be needed taking into account what has 
    ;;    already been ordered
    ;;    send an order to another producer
    ;;    update amount ordered
    ;; -> receive-order
    ;;    add-order to new-orders-queue
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
            (else (error "Undefined message -- MAKE-PRODUCER" msg))))
    dispatch))

(define (product producer)
  (producer 'product))

(define (producer-stock producer)
  (producer 'producer-stock))

(define (amount-on-order producer)
  (producer 'amount-on-order))
