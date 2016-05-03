(let (a
      (b a)
      (c free)
      (d (let (a
               (b a)
               (c free))
           (and a b c free))))
  (and a b c free))

(let ;; comment
    ("s"))

(let (a '))
