(lambda (list)
  (dolist (var list result)
    (lambda () (+ var list result))))
