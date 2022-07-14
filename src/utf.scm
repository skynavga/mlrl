
;
; FSS-UTF (UTF-2) Utilities
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;
; In the following the token "utf" refers unambiguously to the
; FSS-UTF transformation format, and not to any other transformation
; format.
;
; For performance reasons, this should go into microcode.  It could
; also be enhanced considerably in Scheme, but that would require doing
; real work.
;

(declare (usual-integrations))

(define utf/char-length
  (lambda (prefix-byte)
    (cond
      ((zero? (fix:and prefix-byte #x80)) 1)
      ((zero? (fix:and prefix-byte #x20)) 2)
      ((zero? (fix:and prefix-byte #x10)) 3)
      ((zero? (fix:and prefix-byte #x08)) 4)
      ((zero? (fix:and prefix-byte #x04)) 5)
      (else 6))))

(define utf/decode
  (lambda (utf k)
    (letrec ((loop
	       (lambda (bs n i)
		 (if (zero? n)
		     (bit-string->unsigned-integer bs)
		     (loop (bit-string-append
			    (unsigned-integer->bit-string 6
			       (fix:and (vector-8b-ref utf i) #x3F))
			    bs)
			   (- n 1)
			   (+ i 1))))))
      (let ((prefix-byte (vector-8b-ref utf k)))
	(set! k (+ k 1))
	(cond
	 ((zero? (fix:and prefix-byte #x80))
	  (fix:and prefix-byte #x7F))
	 ((zero? (fix:and prefix-byte #x20))
	  (loop (unsigned-integer->bit-string 5 (fix:and prefix-byte #x1F))
	    1 k))
	 ((zero? (fix:and prefix-byte #x10))
	  (loop (unsigned-integer->bit-string 4 (fix:and prefix-byte #x0F))
	    2 k))
	 ((zero? (fix:and prefix-byte #x08))
	  (loop (unsigned-integer->bit-string 3 (fix:and prefix-byte #x07))
	    3 k))
	 ((zero? (fix:and prefix-byte #x04))
	  (loop (unsigned-integer->bit-string 2 (fix:and prefix-byte #x03))
            4 k))
	 (else
	  (loop (unsigned-integer->bit-string 1 (fix:and prefix-byte #x01))
            5 k)))))))

(define utf/prefix-byte?
  (lambda (utf k)
    (not (= (fix:and (vector-8b-ref utf k) #xC0) #x80))))

(define utf/find-next-prefix
  (lambda (utf k at-end)
    (let ((nb (string-length utf)))
      (letrec ((loop
		 (lambda (n)
		   (if (= n nb)
		       (at-end)
		       (if (not (= (fix:and (vector-8b-ref utf n) #xC0) #x80))
			   n
			   (loop (+ n 1)))))))
	(loop k)))))

(define utf/next
  (lambda (utf k at-end)
    (if (>= k (string-length utf))
	(at-end)
	(let ((prefix-byte (vector-8b-ref utf k)))
	  (if (= (fix:and prefix-byte #xC0) #x80)
	      (utf/next utf (utf/find-next-prefix utf k at-end) at-end)
	      (utf/decode utf k))))))

(define utf/find-prev-prefix
  (lambda (utf k at-beginning)
    (letrec ((loop
	      (lambda (n)
		(if (<= n 0)
		    (at-beginning)
		    (if (not (= (fix:and (vector-8b-ref utf n) #xC0) #x80))
			n
			(loop (- n 1)))))))
      (loop (- k 1)))))

(define utf/prev
  (lambda (utf k at-beginning)
    (utf/next utf (utf/find-prev-prefix utf k at-beginning) at-beginning)))

(define utf/ucs-length
  (lambda (utf)
    (let ((nb (string-length utf)))
      (letrec ((loop
		 (lambda (k c)
		   (if (>= k nb)
		       c
		       (loop (+ k (utf/char-length (vector-8b-ref utf k)))
			     (+ c 1))))))
	(loop 0 0)))))
		       
(define utf->ucs
  (lambda (utf)
    (if (not (string? utf))
	(error "Invalid UTF string:" utf))
    (call-with-current-continuation
      (lambda (exit)
	(let ((ucs (make-vector (utf/ucs-length utf))))
	  (letrec ((loop
		     (lambda (k c)
		       (vector-set! ucs c
				    (utf/next utf k (lambda () (exit ucs))))
		       (loop (+ k (utf/char-length (vector-8b-ref utf k)))
			     (+ c 1)))))
	    (loop 0 0)))))))

(define ucs/utf-char-length
  (lambda (ucs k)
    (let ((c (vector-ref ucs k)))
      (cond
       ((< c #x00000080) 1)
       ((< c #x00000800) 2)
       ((< c #x00010000) 3)
       ((< c #x00200000) 4)
       ((< c #x04000000) 5)
       (else 6)))))

(define ucs/encode
  (lambda (ucs k)
    (let ((c (vector-ref ucs k)))
      (if (< c #x80)
	  (string c)
	  (let ((bs (unsigned-integer->bit-string 32 c)))
	    (letrec ((loop
		       (lambda (n s)
			 (if (<= n 0)
			     s
			     (loop (- n 6)
			       (string-append s
				 (string
				   (fix:or #x80 
				     (bit-string->unsigned-integer
				       (bit-substring bs (- n 6) n)))))))))
		     (encode
		       (lambda (m b n)
			 (let* ((lb (* n 6))
				(fbs (unsigned-integer->bit-string 8 m)))
			   (bit-substring-move-right!
			     (bit-substring bs lb (+ lb b)) 0 b fbs 0)
			   (loop lb
			     (string (bit-string->unsigned-integer fbs)))))))
	      (cond
		((< c #x00000800) (encode #xC0 5 1))
		((< c #x00010000) (encode #xE0 4 2))
		((< c #x00200000) (encode #xF0 3 3))
		((< c #x04000000) (encode #xF8 2 4))
		(else (encode #xFC 1 5)))))))))

(define ucs/utf-length
  (lambda (ucs)
    (let ((nc (ucs/length ucs)))
      (letrec ((loop
		 (lambda (k n)
		   (if (>= k nc)
		       n
		       (loop (+ k 1) (+ n (ucs/utf-char-length ucs k)))))))
	(loop 0 0)))))

(define ucs/length
  (lambda (ucs)
    (vector-length ucs)))

(define ucs->utf
  (lambda (ucs)
    (if (not (vector? ucs))
	(error "Invalid UCS string:" ucs))
    (let ((utf (make-string (ucs/utf-length ucs)))
	  (nc (ucs/length ucs)))
      (letrec ((loop
		 (lambda (c k)
		   (if (>= c nc)
		       utf
		       (let* ((s (ucs/encode ucs c))
			      (slen (string-length s)))
			 (substring-move-right! s 0 slen utf k)
			 (loop (+ c 1) (+ k slen)))))))
	(loop 0 0)))))

#|
(define (utf->hex-utf utf)
  (let* ((n     (string-length utf))
	 (hslen (let count ((k 0) (len 0))
		  (if (= k n)
		      len
		      (begin
			(if (< (vector-8b-ref utf k) 128)
			    (count (1+ k) (+ len 1))
			    (count (1+ k) (+ len 4)))))))
	 (hs (make-string hslen #\Nul)))
    (let xlate ((k 0) (j 0))
      (if (= k n)
	  hs
	  (let ((b (vector-8b-ref utf k)))
	    (if (< b 128)
		(begin
		  (vector-8b-set! hs j b)
		  (xlate (1+ k) (1+ j)))
		(begin
		  (substring-move-right!
		    (string #\\ #\'
			    (digit->char (quotient b 16) 16)
			    (digit->char (remainder b 16) 16)) 0 4 hs j)
		  (xlate (1+ k) (+ j 4)))))))))
|#
