
;
; Style System
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(define-record style-catalog (styles default-style))

(define-record style (catalog name parent bindings))

(define style-equal? eq?)

(define (style/put! s key value)
  (bindings/extend! (style/bindings s) key value))

(define (style/get s key #!optional default-value)
  (if (default-object? default-value)
      (set! default-value '()))
  (call/cc
   (lambda (exit)
     (letrec ((loop
	       (lambda (s)
		 (if (null? s)
		     default-value
		     (let ((b (style/bindings s)))
		       (if (bindings/bound? b key)
			   (exit (bindings/lookup b key))
			   (loop (style/parent s))))))))
       (loop s)))))

;
; Style Catalog
;

(define make-style-catalog
  (let ((old-make-style-catalog make-style-catalog))
    (lambda (default-style)
      (old-make-style-catalog (make-string-hash-table) default-style))))

(define (style-catalog/lookup sc name)
  (hash-table/get (style-catalog/styles sc) name '()))

(define (style-catalog/register! sc s)
  (hash-table/put! (style-catalog/styles sc) (style/name s) s))

(define (style-catalog/new sc name parent)
  (if (style-catalog/lookup sc name)
      (error "Style already registered:" name))
  (if (null? parent)
      (set! parent (style-catalog/default-style sc)))
  (let ((s (make-style sc name parent (make-bindings))))
    (style-catalog/register! sc s)
    s))

;
; Builtin Style Bindings
;

(define *default-style-font*
  (font-spec/intern
   '("Courier" ((weight . medium) (posture . upright)) 12)))

(define (style/font s)
  (style/get s 'font  *default-style-font*))

(define (style/set-font! s fs)
  (style/put! s 'font fs))
