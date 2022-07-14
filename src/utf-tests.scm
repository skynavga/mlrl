
(define t0 (string #x00))
(define t1 (string #x7f))
(define t2 (string #xdf #xbf))
(define t3 (string #xef #xbf #xbf))
(define t4 (string #xf7 #xbf #xbf #xbf))
(define t5 (string #xfb #xbf #xbf #xbf #xbf))
(define t6 (string #xfd #xbf #xbf #xbf #xbf #xbf))

(define test-utf
  (lambda (start end)
    (let ((ucs (make-vector 1)))
      (letrec ((loop
		 (lambda (n)
		   (if (= n end)
		       (write-line "Test Complete")
		       (begin
			 (vector-set! ucs 0 n)
			 (if (not (= (utf->ucs (ucs->utf ucs))))
			     (error "Mismatch on:" n)
			     (loop (+ n 1))))))))
	(loop start)))))

		   