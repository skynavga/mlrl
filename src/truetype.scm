
;
; TrueType GX Font Format Support
;

(declare (usual-integrations))

(define-record lookup-table (type table-data))

(define-record segment (start end rator rands))

(define-record state-table
  ( type first-glyph glyph-classes state-array initial-state actions ))

(define-record contextual-substitution-action
  ( next-state mark? dont-advance? substitute-marked substitute-current ))

(define (lookup-table/compile-segment s)
  (make-segment (car s) (cadr s) (caaddr s) (cdaddr s)))

(define (lookup-table/compile-segment-single data)
  (let ((csl '()))
    (let parse ((sl data))
      (if (not (null? sl))
	  (let ((s (car sl)))
	    (set! csl (cons (lookup-table/compile-segment s) csl))
	    (parse (cdr sl)))))
    (set! csl (sort csl
	        (lambda (cs1 cs2)
		  (< (segment/start cs1) (segment/start cs2)))))
    (let validate ((l csl))
      (if (and (not (null? l))
	       (not (null? (cdr l))))
	  (if (>= (segment/end (car l)) (segment/start (cadr l)))
	      (error "Segments overlap:"
		     (list 'segment-1-start (segment/start (car l))
			   'segment-2-start (segment/start (cadr l)))))))
    (list->vector csl)))

(define (lookup-table/compile type data)
  (case type
    ((segment-single)
     (make-lookup-table type (lookup-table/compile-segment-single data)))
    (else
     '())))

(define (lookup-table/lookup-segment-single sv key default)
  (let loop ((lower 0)
	     (upper (vector-length sv)))
    (if (fix:>= lower upper)
	default
	(let* ((k (fix:lsh (fix:+ lower upper) -1))
	       (s (vector-ref sv k)))
	  (cond
	   ((fix:> (segment/start s) key)
	    (loop lower k))
	   ((fix:< (segment/end s) key)
	    (loop (fix:1+ k) upper))
	   (else
	    (case (segment/rator s)
	      ((+) (fix:+ key (car (segment/rands s))))
	      ((-) (fix:- key (car (segment/rands s))))
	      ((=) (car (segment/rands s)))
	      (else
	       (error "Unknown segment operator:" (segment/rator s))))))))))

(define (lookup-table/lookup lt key default)
  (case (lookup-table/type lt)
    ((segment-single)
     (lookup-table/lookup-segment-single
       (lookup-table/table-data lt) key default))
    (else
      default)))

(define (list->vector-8b lon)
  (let* ((n (length lon))
	 (v (make-string n #\Nul)))
    (let loop ((l lon)
	       (k 0))
      (if (not (null? l))
	  (let ((elt (car l)))
	    (if (and (integer? elt) (< elt 256))
		(begin
		  (vector-8b-set! v k elt)
		  (loop (cdr l) (1+ k)))
		(error "Invalid vector-8b element:" elt)))))
    v))

(define (state-table/compile-state-table states)
  (let* ((ns (length states))
	 (st (make-vector ns)))
    (let loop ((l states)
	       (k 0))
      (if (not (null? l))
	  (begin
	    (vector-set! st k (list->vector-8b (car l)))
	    (loop (cdr l) (1+ k)))))
    st))

(define (state-table/compile-contextual-substitution-actions actions args)
  (define (compile-action action)
    (let ((flags (list-ref action 1)))
      (make-contextual-substitution-action
        (list-ref action 0)
	(if (memq 'mark flags) #t #f)
	(if (memq 'dont-advance flags) #t #f)
	(list-ref action 2)
	(list-ref action 3))))
  (let* ((na (length actions))
	 (av (make-vector na)))
    (let loop ((l actions)
	       (k 0))
      (if (not (null? l))
	  (begin
	    (vector-set! av k (compile-action (car l)))
	    (loop (cdr l) (1+ k)))))
    (list av (car args) (list->vector (cadr args)))))

(define (state-table/compile-actions type actions args)
  (case type
    ((contextual-substitution)
     (state-table/compile-contextual-substitution-actions actions args))
    (else
     (error "Can't compile actions for state table of type:" type))))

(define state-table/compile
  (lambda (type glyph-classes states initial-state actions . action-args)
    (let* ((fg (car glyph-classes))
	   (cv (list->vector-8b (cdr glyph-classes)))
	   (st (state-table/compile-state-table states))
	   (at (state-table/compile-actions type actions action-args)))
      (make-state-table type fg cv st initial-state at))))

(define (state-table/run-contextual-substitution st a as gs k)
  (let ((ss (cadr (state-table/actions st)))
	(st (caddr (state-table/actions st)))
	(sm (contextual-substitution-action/substitute-marked a))
	(sc (contextual-substitution-action/substitute-current a)))
    (define (substitute! i o)
      (let* ((lgi (glyph-stream/read-at gs i))
	     (rands (gi/rands lgi))
	     (si (+ (cadr rands) o (- ss))))
	(if (and (not (negative? si))
		 (< si (vector-length st)))
	    (set-cdr! rands (list (vector-ref st si))))))
    (if (not (zero? sm))
	(substitute! (cell-contents as) sm))
    (if (and (not (zero? sc)) (< k (glyph-stream/fill gs)))
	(substitute! k sc))
    (if (contextual-substitution-action/mark? a)
	(set-cell-contents! as k))
    (values (contextual-substitution-action/next-state a)
	    (contextual-substitution-action/dont-advance? a))))

(define *end-of-text*	0)
(define *out-of-bounds* 1)
(define *deleted*       2)
(define *end-of-line*   3)

(define (state-table/run! st gs)
  (let* ((tt (state-table/type st))
	 (ct (state-table/glyph-classes st))
	 (fg (state-table/first-glyph st))
	 (lg (+ fg (string-length ct)))
	 (ng (glyph-stream/fill gs))
	 (s  (state-table/initial-state st))
	 (as (make-cell '())))
    (define (get-glyph k)
      (let ((lgi (glyph-stream/read-at gs k)))
	(case (gi/rator lgi)
	  ((base attach)
	   (cadr (gi/rands lgi)))
	  ((space nbsp)
	   '())
	  (else
	   (error "Invalid LGI in context stream:" (gi/rator lgi))))))
    (define (get-class k)
      (if (= k ng)
	  *end-of-text*
	  (let ((g (get-glyph k)))
	    (if (or (null? g) (< g fg) (>= g lg))
		*out-of-bounds*
		(vector-8b-ref ct (- g fg))))))
    (define (get-action c)
      (let ((sv (vector-ref (state-table/state-array st) s)))
	(vector-ref (car (state-table/actions st)) (vector-8b-ref sv c))))
    (let next-glyph ((k 0))
      (if (<= k ng)
	  (let* ((c (get-class k))
		 (a (get-action c)))
	    (with-values
		(lambda ()
		  (case tt
		    ((contextual-substitution)
		     (state-table/run-contextual-substitution st a as gs k))
		    (else
		     (error "Can't run action for state table of type:" tt))))
	      (lambda (new-state dont-advance?)
		(set! s new-state)
		(next-glyph (if dont-advance? k (1+ k)))))))
      unspecific)))
