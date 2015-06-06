(let (a)
  (cond
   (a t)
   (free t)
   ((eq a free) t)
   (t (list a free))
   ;; c
   "s"))
