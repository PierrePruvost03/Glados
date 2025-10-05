(define fact
  (lambda (x)
    (if (zero? x)
      1
      (* x (fact (- x 1))
        )
      )
    )
  )