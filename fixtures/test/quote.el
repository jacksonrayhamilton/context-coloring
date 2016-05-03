(quote (lambda () free))
(let () (backquote (,free)))

(defun a (a)
  (or (eq a 'b)
      (equal a '(a b))
      (equal a `(,(append () `(a b ,(+ 1 free) ,free b) free) b ,free
                 "s" ; c
                 ))))

(append '("a" ; b
          "b" ; a
          ))

(lambda () '((?\" ?\")))
