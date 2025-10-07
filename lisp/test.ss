(define fact
  (lambda (x)
    (if (zero? x)
      1 ; hummm commentaire qui paie la taxe foncière
      (* x (fact (- x 1)) ; commentaire de tek3
        )
      )
    ) ;;commentaire tres mature
  )
