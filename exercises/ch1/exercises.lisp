;; 1.1

(defparameter *suffixes*
  '(jr. md))

(defun null-or-suffix (name)
  (or (null (car name))
      (member (car name) *suffixes*)))

(defun last-name (name)
  (if (null-or-suffix (cdr name))
      (car name)
      (last-name (cdr name))))

;; 1.2

(defun power (a b)
  "Raise first param to the power second param"
  (defun iter (accum counter)
    (if (eq counter 0)
	1
	(if (eq counter 1)
	    accum
	    (iter (* accum a) (- counter 1)))))
  (iter a b))

;; Solution from book
(defun power (x n)
  "Power raises x to the nth power. N must be an integer >= 0. This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
	((evenp n) (expt (power x (/ n 2)) 2))
	(t (* x (power x (- n 1))))))

;; 1.3

(defun count-atoms (expression)
  "Returns the number of atoms in an expression"
  (defun iter (exp num remaining)
    (if (eq remaining 0)
	num
	(if (atom (car exp))
	    (iter (cdr exp) (+ 1 num) (- remaining 1))
	    (iter (cdr exp) num (- remaining 1)))))
  (iter expression 0 (length expression)))

;; Solution from book
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
	((atom exp) 1)
	(t (+ (count-atoms (car exp))
	      (count-atoms (cdr exp))))))
(defun count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression counting nil as an atom only in non-tail positions"
  (cond ((null exp) if-null)
	((atom exp) 1)
	(t (+ (count-all-atoms (car exp) 1)
	      (count-all-atoms (cdr exp) 0)))))
  
;; 1.4

(defun count-occurrences (exp1 exp2)
  "Count occurrences of one expression inside another"
  (defun iter (exp1 exp2 num remaining)
    (if (eq remaining 0)
	num
	(if (eq exp1 (car exp2))
	    (iter exp1 (cdr exp2) (+ 1 num) (- remaining 1))
	    (iter exp1 (cdr exp2) num (- remaining 1)))))
  (iter exp1 exp2 0 (length exp2)))

;; 1.5
(defun dot-product (a b)
  "Dot product..."
  (apply #'+ (mapcar #'* a b)))
