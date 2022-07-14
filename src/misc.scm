
;
; Miscellaneous Utilities and Abbreviations
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(declare (usual-integrations))

;
; Random Utilities
;

(define (filename->tail filename)
  (let ((i (string-find-previous-char-in-set filename (char-set #\/))))
    (if (number? i)
	(string-tail filename (1+ i))
	filename)))

(define (round-up n m)
  (if (zero? (modulo n m))
      n
      (+ (* (quotient n m) m) m)))

(define (in->pt in) (* in 72))

(define (pt->in pt) (/ pt 72))

(define (pt->upt pt) (* pt 65536))

(define (upt->pt upt) (/ upt 65536))

;
; Abbreviations
;

(define call/cc call-with-current-continuation)

;
; Extended I/O Port Operations
;

(define file-position
  (package/reference
    (find-package '(runtime primitive-io)) 'file-position))

(define file-set-position
  (package/reference
    (find-package '(runtime primitive-io)) 'file-set-position))

(define file-length
  (package/reference
    (find-package '(runtime primitive-io)) 'file-length))

(define input-buffer/flush
  (package/reference
    (find-package '(runtime primitive-io)) 'input-buffer/flush))

(define output-buffer/position
  (package/reference
    (find-package '(runtime primitive-io)) 'output-buffer/position))

(define input-file-state/buffer
  (package/reference
    (find-package '(runtime file-input)) 'input-file-state/buffer))

(define output-file-state/buffer
  (package/reference
    (find-package '(runtime file-output)) 'output-file-state/buffer))

(define output-port/type
  (let ((file-ps (output-port/custom-operation
		   (package/reference
		     (find-package '(runtime file-output))
		     'output-file-template)
		   'print-self))
	(string-ps (output-port/custom-operation
		    (package/reference
		      (find-package '(runtime string-output))
		      'output-string-template)
		    'print-self)))
    (lambda (port)
      (let ((ps (output-port/custom-operation port 'print-self)))
	(cond ((eq? ps file-ps) 'file)
	      ((eq? ps string-ps) 'string)
	      (else '()))))))

(define (string-output-port/position port)
  (letrec ((loop
	    (lambda (l)
	      (if (null? l)
		  0
		  (+ (string-length (car l)) (loop (cdr l)))))))
    (loop (output-port/state port))))

(define (file-output-port/position port)
  (+ (file-position (output-port/channel port))
     (output-buffer/position
       (output-file-state/buffer (output-port/state port)))))
  
(define (output-port/position port)
  (case (output-port/type port)
    ((string)
     (string-output-port/position port))
    ((file)
     (file-output-port/position port))
    (else
     (error "Can't determine position for output port:" port))))

(define input-port/type
  (let ((file-ps (input-port/custom-operation
		   (package/reference
		     (find-package '(runtime file-input))
		     'input-file-template)
		   'print-self))
	(string-ps (input-port/custom-operation
		    (package/reference
		      (find-package '(runtime string-input))
		      'input-string-template)
		    'print-self)))
    (lambda (port)
      (let ((ps (input-port/custom-operation port 'print-self)))
	(cond ((eq? ps file-ps) 'file)
	      ((eq? ps string-ps) 'string)
	      (else '()))))))

(define (string-input-port/position port)
  (error "Can't determine position of string input port:" port))

(define (file-input-port/position port)
  (let ((n (file-length (input-port/channel port)))
	(r ((input-port/operation port 'chars-remaining) port)))
    (- n r)))
  
(define (input-port/position port)
  (case (input-port/type port)
    ((string)
     (string-input-port/position port))
    ((file)
     (file-input-port/position port))
    (else
     (error "Can't determine position for input port:" port))))

(define (string-input-port/set-position port position)
  position	;ignore
  (error "Can't set position of string input port:" port))

(define (file-input-port/set-position port position)
  (let ((s (input-port/state port)))
    (if (not (null? s))
	(input-buffer/flush (input-file-state/buffer s)))
    (file-set-position (input-port/channel port) position)))
  
(define (input-port/set-position port position)
  (case (input-port/type port)
    ((string)
     (string-input-port/set-position port position))
    ((file)
     (file-input-port/set-position port position))
    (else
     (error "Can't determine position for input port:" port))))

;
; Number Serializers & Deserializers
;

(define (write-16 number #!optional signed?)
  (if (default-object? signed?)
      (set! signed? #f))
  (if (not (or (number? number) (integer? number)))
      (error "Can't write non-integer:" number))
  (if (and (negative? number)
	   (not signed?))
      (error "Can't write negative number as unsigned:" number))
  (let ((bs))
    (if signed?
	(if (or (< number -32678) (> number  32767))
	    (error "Can't write signed number: out of range:" number)
	    (set! bs (signed-integer->bit-string 16 number)))
	(if (> number 65535)
	    (error "Can't write unsigned number: out of range:" number)
	    (set! bs (unsigned-integer->bit-string 16 number))))
    (write-char
      (integer->char (bit-string->unsigned-integer (bit-substring bs 8 16))))
    (write-char
      (integer->char (bit-string->unsigned-integer (bit-substring bs 0 8))))))

(define (read-16 #!optional signed?)
  (if (default-object? signed?)
      (set! signed? #f))
  (let ((bs (make-bit-string 16 #f)))
    (bit-substring-move-right!
      (unsigned-integer->bit-string 8 (char->integer (read-char))) 0 8 bs 8)
    (bit-substring-move-right!
      (unsigned-integer->bit-string 8 (char->integer (read-char))) 0 8 bs 0)
    (if signed?
	(bit-string->signed-integer bs)
	(bit-string->unsigned-integer bs))))

(define number-parts
  (lambda (n)
    (let ((whole (inexact->exact (truncate n))))
      (values whole
	      (inexact->exact (truncate (* (abs (- n whole)) 65536)))))))

(define (write-16.16 number)
  (with-values
      (lambda ()
	(number-parts number))
    (lambda (int frac)
      (write-16 int #t)
      (write-16 frac #f))))

(define (read-16.16)
  (let* ((int (read-16 #t))
	 (frac (exact->inexact (/ (read-16 #f) 65536))))
    (if (negative? int)
	(+ int (- frac))
	(+ int frac))))

(define (write-32 number #!optional signed?)
  (if (default-object? signed?)
      (set! signed? #f))
  (if (not (or (number? number) (integer? number)))
      (error "Can't write non-integer:" number))
  (if (and (negative? number)
	   (not signed?))
      (error "Can't write negative number as unsigned:" number))
  (let ((bs))
    (if signed?
	(if (or (< number -2147483648) (> number 2147483647))
	    (error "Can't write signed number: out of range:" number)
	    (set! bs (signed-integer->bit-string 32 number)))
	(if (> number 4294967295)
	    (error "Can't write unsigned number: out of range:" number)
	    (set! bs (unsigned-integer->bit-string 32 number))))
    (write-char
      (integer->char (bit-string->unsigned-integer (bit-substring bs 24 32))))
    (write-char
      (integer->char (bit-string->unsigned-integer (bit-substring bs 16 24))))
    (write-char
      (integer->char (bit-string->unsigned-integer (bit-substring bs 8 16))))
    (write-char
      (integer->char (bit-string->unsigned-integer (bit-substring bs 0 8))))))

(define (read-32 #!optional signed?)
  (if (default-object? signed?)
      (set! signed? #f))
  (let ((bs (make-bit-string 32 #f)))
    (bit-substring-move-right!
      (unsigned-integer->bit-string 8 (char->integer (read-char))) 0 8 bs 24)
    (bit-substring-move-right!
      (unsigned-integer->bit-string 8 (char->integer (read-char))) 0 8 bs 16)
    (bit-substring-move-right!
      (unsigned-integer->bit-string 8 (char->integer (read-char))) 0 8 bs 8)
    (bit-substring-move-right!
      (unsigned-integer->bit-string 8 (char->integer (read-char))) 0 8 bs 0)
    (if signed?
	(bit-string->signed-integer bs)
	(bit-string->unsigned-integer bs))))

