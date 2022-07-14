
;
; Script System
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

;
; Globals
;

(define *scripts* (make-1d-table))

;
; Data Types
;

(define-record script
  (name properties elements))

(define-record script-properties
  (type classes element-properties other))

(define-record script-class
  (name bound))

(define-record script-element
  (name class properties))

;
; Procedures
;

(define register-script
  (lambda (form)
    (let ((s (parse-script form)))
      (if (find-script (script->name s))
	  (begin
	    (warn "Script already defined:" (script->name s))
	    (set! s #f))
	  (add-script (script->name s) s))
      s)))

(define unregister-script
  (lambda (name)
    (if (not (find-script name))
	(warn "Script not defined:" name)
	(delete-script name))))

(define find-script
  (lambda (name)
    (1d-table/get *scripts* name #f)))

(define add-script
  (lambda (name s)
    (1d-table/put! *scripts* name s)))

(define delete-script
  (lambda (name)
    (1d-table/remove! *scripts* name)))

(define parse-script
  (lambda (form)
    (if (or (not (pair? form))
	    (null? form)
	    (null? (cdr form)))
	(error "Invalid Script Specification:" form)
	(parse-script-1 form))))

(define parse-script-1
  (lambda (form)
    (let ((name (car form))
	  (props (cadr form))
	  (elts (cddr form)))
      (let ((sp (parse-script-properties props)))
	(make-script name sp (parse-script-elements elts sp))))))

(define parse-script-properties
  (lambda (props)
    (if (not (alist? props))
	(error "Invalid Script Property List:" props)
	(let ((type (alist/get props ':type ':secondary)))
	  (make-script-properties
	    (case type
	      ((:primary :secondary) type)
	      (else (error "Invalid Script Type:" type)))
	    (parse-script-classes props)
	    '()
	    '())))))

(define parse-script-classes
  (lambda (props)
    (letrec ((loop
	       (lambda (c loc bound ht)
		 (if (not (symbol? c))
		     (warn "Ignoring invalid script class:" c)
		     (if (hash-table/get ht c #f)
			 (warn "Ignoring duplicate script class:" c)
			 (hash-table/put! ht c (make-script-class c bound))))
		 (if (null? loc)
		     'unspecified
		     (loop (car loc) (cdr loc) bound ht)))))
      (let ((ic (alist/get props ':independent-classes '()))
	    (bc (alist/get props ':bound-classes '())))
	(let ((ht (make-symbol-hash-table (+ (length ic) (length bc)))))
	  (if (pair? ic)
	      (loop (car ic) (cdr ic) #f ht))
	  (if (pair? bc)
	      (loop (car bc) (cdr bc) #t ht))
	  ht)))))

(define parse-script-elements
  (lambda (lel sp)
    (let ((cht (script-properties->classes sp)))
      (letrec ((loop
		 (lambda (el lel ht)
		   (if (or (not (pair? el))
			   (not (pair? (cdr el))))
		       (error "Invalid script elements:" el))
		   (let ((class (car el))
			 (elts (cadr el)))
		     (let ((sc (hash-table/get cht class #f)))
		       (if (not sc)
			   (error "Unknown script class:" class))
		       (map (lambda (elt)
			      (if (hash-table/get ht elt #f)
				(warn "Ignoring uplicate element:" elt)
				(hash-table/put! ht elt
				 (make-script-element elt sc '()))))
			    elts)
		       (if (null? lel)
			   'unspecified
			   (loop (car lel) (cdr lel) ht)))))))
	(let ((ht (make-symbol-hash-table)))
	  (if (pair? lel)
	      (loop (car lel) (cdr lel) ht))
	  ht)))))

(define alist/get
  (lambda (al sym default)
    (let ((a (assq sym al)))
      (if (null? a)
	  default
	  (cdr a)))))

