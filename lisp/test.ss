(define fact
  (lambda (x)
    (if (zero? x)
      1 ; prout pipi
      (* x (fact (- x 1)) ; alexendre et cs
        )
      )
    ) ;j'aime la bite
  )
