;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;;  define-record and variant-case macros for MIT Scheme                   ;;;
;;;                                                                         ;;;
;;;     define-record is totally rewritten using the built-in               ;;;
;;;      record facility provided by MIT Scheme.                            ;;;
;;;     variant-case is just a modifcation of the PC Scheme                 ;;;
;;;      and MacScheme macro by Jeff Alexander and Shinn-Der Lee.           ;;;
;;;                                                                         ;;;
;;;            Brent Benson (bwb@cs.unh.edu)                                ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; The first two macros are so that the macro definition has
;;; desired affect both in code and at the REP loop.
;;;
(define-macro (define-macro-both pattern . body)
  `(begin
     (define-macro ,pattern ,@body)
     (syntax-table-define system-global-syntax-table ',(car pattern)
       (macro ,(cdr pattern)
	 ,@body))))

(syntax-table-define system-global-syntax-table 'define-macro-both
  (macro (pattern . body)
    `(begin
       (define-macro ,pattern ,@body)
       (syntax-table-define system-global-syntax-table ',(car pattern)
	 (macro ,(cdr pattern)
	   ,@body)))))

;;;
;;; A couple of name differences
;;;
(define gensym generate-uninterned-symbol)
(define add1 1+)
(define (all-true? pred lst) (for-all? lst pred))
(define null-ended-list? list?)

(define error/define-record-or-variant-case
  (lambda args
    (for-each (lambda (x) (display x) (display " ")) args)
    (newline)
    (error "Error from define-record or variant-case.")))

(define-macro-both (define-record name fields)
  (if (and (symbol? name)
	   (list? fields))
      (let* ((name-str (symbol->string name))
	     (type (gensym))
	     (constructor (string->symbol (string-append "make-" name-str)))
	     (predicate (string->symbol (string-append name-str "?")))
	     (accessors (gen-accessors name-str type fields))
	     (updaters (gen-updaters name-str type fields)))
	`(begin
	   (define ,type (make-record-type ,name-str ',fields))
	   (2d-put! ',name 'record-type ,type)
	   (define ,constructor (record-constructor ,type))
	   (define ,predicate (record-predicate ,type))
	   ,@accessors
	   ,@updaters))
      (error/define-record-or-variant-case
	"define-record syntax error:" name)))

(define-macro-both (record-type name)
  `(2d-get ',name 'record-type))

(define (gen-accessors name-str type fields)
  (if (null? fields)
      '()
      (let* ((field-name (car fields))
	     (field-str (symbol->string field-name))
	     (acc-name 
               (string->symbol (string-append name-str "/" field-str))))
	(cons `(define ,acc-name (record-accessor ,type ',field-name))
	      (gen-accessors name-str type (cdr fields))))))
	     
(define (gen-updaters name-str type fields)
  (if (null? fields)
      '()
      (let* ((field-name (car fields))
	     (field-str (symbol->string field-name))
	     (upd-name 
               (string->symbol
		(string-append name-str "/set-" field-str "!"))))
	(cons `(define ,upd-name (record-updater ,type ',field-name))
	      (gen-updaters name-str type (cdr fields))))))
	     
(define-macro-both (variant-case record-var . clauses)
  (let ((var (gensym)))
    (letrec
      ((loop
	 (lambda (clause)
	   (cond
             ((null? clause)
              `((#t (error/define-record-or-variant-case
                      "no clause matches:" ,var))))
             ((eq? (caar clause) 'else)
              (if (not (null? (cdr clause)))
                  (error/define-record-or-variant-case
                    "variant-case syntax error: clauses after an else."
                    (cdr clause))
                  `((#t ,@(cdar clause)))))
             ((assoc (caar clause) (cdr clause))
              (error/define-record-or-variant-case
                "variant-case syntax error: duplicate clause:"
                (caar clause)))
             (else
	       (let ((name (symbol->string (caar clause))))
		 (cons
		   `((,(string->symbol (string-append name "?")) ,var)
		     (let ,(let-vars name (cadar clause))
		       ,@(cddar clause)))
		   (loop (cdr clause))))))))
       (let-vars
	 (lambda (name fields)
	   (cond
	     ((null? fields) '())
	     ((member (car fields) (cdr fields))
	      (error/define-record-or-variant-case
		"variant-case syntax error: duplicate field. record:"
		(string-append name "," " field:") (car fields)))
	     (#t
               (cons
		 `(,(car fields)
		   (,(string->symbol
		       (string-append
			 name "->" (symbol->string (car fields))))
		    ,var))
		 (let-vars name (cdr fields))))))))
      (if (and (all-true?
		 (lambda (clause)
		   (and (null-ended-list? clause)
			(not (null? clause))
			(symbol? (car clause))
			(if (eq? (car clause) 'else)
			    (not (null? (cdr clause)))
			    (and (> (length clause) 2)
				 (null-ended-list? (cadr clause))
				 (all-true? symbol? (cadr clause))))))
		 clauses))
	  `(let ((,var ,record-var))
	     (cond ,@(loop clauses)))
	  (error/define-record-or-variant-case
	    "variant-case syntax error:" record-var)))))
