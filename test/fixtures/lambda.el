(funcall (lambda (fn)
           (funcall fn (lambda (fn)
                         (fn fn fn) fn)) fn) 0)
