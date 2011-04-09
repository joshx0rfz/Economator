;;; Economate.el
;;;
;;; A dead simple economy simulator

;; Utility functions

; Generic getter
; Must return a cons cell as assoc does
(defalias get-cons assoc)
(defun get-val (key obj) (cdr (get-cons key obj)))

; Sets something once in a list
; Inputs: pred - a predicate to satisfy, should take one argument, return boolean
;         setter - given the current value, return the new value
;         list-val - the list to destructively update
; Outputs: nil
; Operation: Steps through the list
;            When (pred elem) is satisfied
;               (set elem (setter elem))
;               Return
(defun set-once (pred setter list-val)
  (cond ((and list-val (funcall pred (car list-val)))
	 (setcar list-val (funcall setter (car list-val)))
	 t) ; Indicate success
	(list-val (set-once pred setter (cdr list-val)))
	(t nil))) ; Indicate failure

; Same as set-once, but searches from the tail in.
(defun set-once-r (pred setter list-val)
  (cond ((null list-val) ())
	((set-once-r pred setter (cdr list-val)) 
	 t) ; Pass the true return value up.
	((funcall pred (car list-val)) ; Check the predicate
	 (setcar list-val (funcall setter (car list-val)))
	 t) ; Having set our value, return true to indicate we're done
	(t nil))) ; Otherwise, still haven't found it, return nil.

; This applies the setter to all values in list when pred is true
(defun set-when (pred setter list-val)
  (when list-val 
    (when (funcall pred (car list-val))
      (setcar list-val (funcall setter (car list-val))))
    (set-when (cdr list-val))))

; Unfortunately, can't just do (apply 'or (mapcar pref list-val))
; As (or ... is a special form)
; This function tries to find one element in list-val that is true
(defun any (pred list-val)
  (and list-val ; Only continue if list-val is not nil 
       (or (funcall pred (car list-val)) ; Either this car is true
	   (any pred (cdr list-val))))) ; Or one in the rest of the list

; Predicate to test if a value is greater than 0
(defun >0 (x) (> x 0))
(defun =0 (x) (= x 0))

; A random number between 0 and 1.000
(defun rnd-flt () (* (random 1000) 0.001))

;; Constants
(defconst hunt-max-food  7)
(defconst food-new-age   14)
(defconst skin-num-used  10)
(defconst skin-new-age   28)
(defconst beads-init-max 20)
(defconst price-init-max 10)

;; Objects

; The village is a list of huts. We'll add to it later.
(defvar the-village ())

; The food market is a list of offers.
(defvar the-food-market (make-list food-new-age))

; The skin market is also a list of offers.
(defvar the-skin-market ())

; A hut offers food of some age at a price
(defun offer-skins (price hut num)
  (nconc the-skin-market (list price hut num)))

(defun offer-food (price age hut num)


;;; A hut is a simple list
;;; For now, we're defining a hut to have a type, either
;;; hunter or trapper.
(defun make-hut (type) 
  `((type . ,type)
    (food . ,(make-list food-new-age :initial-element 0))
    (food-sell-price . ,(1+ (random price-init-max)))
    (food-buy-price . ,(1+ (random price-init-max)))
    (skins . ,(make-list skin-num-used :initial-element 0))
    (skins-extra . 0)
    (skins-margin . ,(* (rnd-flt) skin-margin-max)
		  (beads . (1+ (random beads-init-max))))))

;;; At sunset, food is eaten and spoils, and skins become worn out
(defun hut-sundown (hut)
  (let (
	(food (get-cons 'food hut))
	(skins (get-val 'skins hut))
	)
    ; Eat food
    (set-once '>0 '1- (cdr food))
    ; Age food
    ; Add a 0 to the end of our food list
    (nconc (cdr food) '(0))
    ; And throw out the oldest food (front of the list)
    (setcdr food (cddr food))
    ; Apply wear and tear to skins
    (set-when '>0 '1- skins)))

;;; At noon, skins are replaced, and food and skins are put on the market
(defun hut-noon (hut)
     (let (
	(skins-x (get-cons 'skins-x hut))
	(skin-replace (lambda (age)
			(if (>0 skins-x)
			    (prog (setcdr skins-x (1- (cdr skins-x)))
				  skin-new-age)
			  age)))
	)
       ; Attempt to replace worn out skins
       (set-when '=0 'skin-replace skins)
       ; Put food and skins on the market.
       
       ))

(defun hut-hunt (hut)

)

(defun hut-trap (hut)

)

(defun hut-hungry? (hut)

)
; 
(defun hut-choose-action (hut)

)

(defun run-day (villa)
)