(funcall (lambda (fn a)
           (funcall fn (lambda (fn)
                         (fn fn a) fn)) fn) 0 1)
