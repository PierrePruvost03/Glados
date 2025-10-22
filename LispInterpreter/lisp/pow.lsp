(define pow
  (lambda (x n)
    (if (zero? n)
        1
        (* x (pow x (- n 1))))))

(pow 2 8)
;; result = 256
