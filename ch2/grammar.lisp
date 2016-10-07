(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(man ball woman table)))
(defun Verb () (one-of '(hit threw attacks)))

(defun one-of (set)
  "Pick one element of a set and make a list of it."
  (list (rand-elt set)))

(defun rand-elt (choices)
  "Choose a random element from a list"
  (elt choices (random (length choices))))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*, but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (car rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (cddr rule))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((rewritten-phrase (rewrites phrase)))
    (cond ((listp phrase)
	   (mappend #'generate phrase))
	  ((not (null rewritten-phrase))
	   (generate (rand-elt rewritten-phrase)))
	  (t (list phrase)))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase with a complete parse tree."
  (cond ((listp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase)
	 (cons phrase
	       (generate-tree (rand-elt (rewrites phrase)))))
	(t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
	((listp phrase)
	 (combine-all (generate-all (car phrase))
		      (generate-all (cdr phrase))))
	((rewrites phrase)
	 (mappend #'generate-all (rewrites phrase)))
	(t (list (list phrase)))))
(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x."
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (append x y)) xlist))
	   ylist))
	   
