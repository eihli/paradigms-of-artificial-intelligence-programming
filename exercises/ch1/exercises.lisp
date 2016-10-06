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
