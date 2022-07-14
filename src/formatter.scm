
;
; Formatter System
;

(define-record formatter (device styles fonts templates))

(define *default-formatter-device-args* '(postscript 400 400))

(define (formatter/open device-args)
  (if (null? device-args)
      (set! device-args *default-formatter-device-args*))
  (make-formatter (apply device/open device-args) '() '() '()))

(define (formatter/font-catalog f)
  (device/font-catalog (formatter/device f)))

(define (formatter/format f doc p)
  f	;ignore!!
  (page/for-each-column p
    (lambda (c)
      (let ((cs (document/content-stream doc (column/content-type c))))
	(content-stream/format cs c)))))
