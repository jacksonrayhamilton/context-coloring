(let* (a
       (b a)
       (c free))
  (and a b c d e free)
  (let* (d
         (e a)
         (c free))
    (and a b c d e free))
  (and a b c d e free))
