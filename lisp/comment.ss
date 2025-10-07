(define fact
  (lambda (x)
    (if (zero? x)
      1
      (* x (fact (- x 1))
        )
      )
    )
  )

(fact 20)
; result = 2432902008176640000
