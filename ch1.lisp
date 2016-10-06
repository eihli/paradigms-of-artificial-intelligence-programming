(defun mappend (fn the-list)
  "Supposed to apply a function to a list and return appended..."
  (apply #'append (mapcar fn the-list)))

(defun number-and-negation (number)
  (list number (- number)))

(defun numbers-and-negations (numbers)
  (mappend #'number-and-negation numbers))
