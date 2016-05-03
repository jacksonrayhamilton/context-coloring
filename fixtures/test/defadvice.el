(defadvice a (before advice first (b) activate)
  (let ((c b))
    (+ b c)))
