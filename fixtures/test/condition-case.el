(condition-case err
    (progn err free)
  (error err free)
  ((debug error) err free))

(condition-case-unless-debug nil
    ;; c
    (let () nil)
  (error (let () nil))
  "s")
