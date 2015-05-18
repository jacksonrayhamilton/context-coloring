(defun a (a)
  (or (eq a 'b)
      (equal a '(a b))
      (equal a `(,(append () `(a b ,(+ 1 free) ,free b) free) b ,free))))
