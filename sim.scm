;; INVENTORY

(define (make-inventory-entry product in-stock ordered)
  (list product in-stock ordered))

(define (in-stock inventory-entry) (cadr inventory-entry))

(define (ordered inventory-entry) (caddr inventory-entry))

(define (make-inventory items) items)

(define (check-stock inventory item)
  (let ((item-entry (assq item inventory)))
    (if item-entry
        (in-stock item-entry)
        'item-not-in-inventory)))

;; PRODUCERS

(define (make-producer product initial-stock requirements)
  ;; INITS GO HERE

  ;; DISPTACH
  (define (dispatch msg)
    (cond ((eq? msg 'product) product)
          (else (error "Undefined message -- MAKE-PRODUCER"))))
  dispatch)

(define (product producer)
  (producer 'product))
