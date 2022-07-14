 
;
; Line Primitives
;

(declare (usual-integrations))

(define-record line
  ( column			; column in which line appears
    number			; line number in content unit
    origin			; baseline origin
    length			; line length
    ascent			; max font ascent (or side bearing)
    descent			; max font descent (or side bearing)
    bbox			; ink bounding box (relative to baseline origin)
    orientation			; line orientation in column
    reverse-flow?		; #t for RTL horizontal & BTT vertical
    content			; content unit containing line's text
    logical-glyph-stream	; rendered content
    logical-glyph-stream-start	; start position in rendered content
    logical-glyph-stream-end	; end position in rendered content
    physical-glyph-stream	; formatted content
    next			; next line in column
    previous			; previous line in column
  ))

(define make-line
  (let ((old-make-line make-line))
    (lambda (col prev lno origin length reverse-flow? cu)
      (old-make-line
       col
       lno
       origin
       length
       '()
       '()
       '()
       (column/orientation col)
       reverse-flow?
       cu
       '()
       '()
       '()
       '()
       '()
       prev))))

(define (line/set-baseline! line bl)
  (case (line/orientation line)
    ((horizontal)
     (point/set-y! (line/origin line) bl))
    ((vertical)
     (point/set-x! (line/origin line) bl)))
  unspecific)

(define (line/content-interval line)
  (values (line/content line)
	  (line/content-start line)
	  (line/content-end line)))

(define (line/previous-with-content line cu)
  (let loop ((pl line))
    (if (null? pl)
	'()
	(if (eq? (line/content line) cu)
	    pl
	    (loop (line/previous pl))))))

(define (line/content-position line cu)
  (let ((cl (line/previous-with-content line cu)))
    (if (null? cl)
	0
	(line/logical-glyph-stream-end cl))))

(define (line/redisplay line)
  (let* ((col (line/column line))
	 (gd (column/device col))
	 (pgs (line/physical-glyph-stream line))
	 (gl '())
	 (rf (line/reverse-flow? line))
	 (lm (if rf (line/length line) 0))
	 (ox (point/x (line/origin line)))
	 (bpt (make-point 0 (point/y (line/origin line))))
	 (spt (make-point 0 0)))
    (letrec ((flush-base
	      (lambda ()
		(if (> (length gl) 0)
		    (begin
		      (if rf
			  (begin
			    (point/set-x! bpt (+ ox lm))
			    (device/show-glyphs! gd (list->vector gl) bpt))
			  (device/show-glyphs! gd (list->vector (reverse gl))))
		      (set! gl '())))
		unspecific))
	     (loop
	      (lambda ()
		(if (glyph-stream/end-of-stream? pgs)
		    (flush-base)
		    (let* ((gi (glyph-stream/read pgs))
			   (rator (gi/rator gi))
			   (rands (gi/rands gi)))
		      (if (eq? rator 'end-of-stream)
			  (flush-base)
			  (begin
			    (case rator
			      ((base attach)
			       (if rf
				   (set! lm (- lm (fourth rands))))
			       (set! gl (cons (cadr rands) gl)))
			      ((space)
			       (flush-base)
			       (let ((dx (third rands)))
				 (if rf
				     (set! lm (- lm dx))
				     (begin
				       (point/set-x! spt dx)
				       (device/set-point-relative! gd spt)))))
			      ((font)
			       (flush-base)
			       (device/set-font! gd (car rands))))
			    (loop))))))))
      (glyph-stream/rewind pgs)
      (if (not rf)
	  (device/set-point! gd (line/origin line)))
      (loop))))

;
; Line Layout
;

(define-record compose-state
  (lgs-pos pgs-pos measure ascent descent bbox font-instance))

(define alloc-compose-state)

(define free-compose-state)

(let ((pool '()))
  (set! alloc-compose-state
	(lambda ()
	  (if (null? pool)
	      (make-compose-state 0 0 0 0 0 (make-empty-rect) '())
	      (let ((cs (car pool)))
		(set! pool (cdr pool))
		(subvector-fill! cs 1 (-1+ (vector-length cs)) 0)
		(compose-state/set-bbox! cs (make-empty-rect))
		cs))))
  (set! free-compose-state
	(lambda (cs)
	  (set! pool (cons cs pool)))))
	      
(define (free-compose-states list-of-compose-states)
  (let loop ((l list-of-compose-states))
    (if (not (null? l))
	(begin
	  (free-compose-state (car l))
	  (loop (cdr l))))))

(define (copy-compose-state cs)
  (let ((ncs (alloc-compose-state)))
    (let ((nbb (compose-state/bbox ncs)))
      (subvector-move-right! cs 0 (vector-length cs) ncs 0)
      (rect/copy (compose-state/bbox cs) nbb)
      ncs)))


(define (line/compose l lgs pgs initial-font-instance)
  (let ((gm (make-glyph-metrics))
	(dir (line/orientation l)))
    (define (update-bbox cs gm)
      (let ((lbb (compose-state/bbox cs))
	    (tbb (glyph-metrics/bbox gm))
	    (bo))
	(case dir
	  ((horizontal)
	   (set! bo (make-point (compose-state/measure cs) 0)))
	  ((vertical)
	   (set! bo (make-point 0 (compose-state/measure cs)))))
	(rect/union (rect/translate tbb (point/add (rect/ll tbb) bo)) lbb lbb)))
    (define (set-font! cs fi)
      (case (line/orientation l)
	((horizontal)
	 (compose-state/set-ascent! cs
	   (max (compose-state/ascent cs)
		(abs (font-instance/ascent fi))))
	 (compose-state/set-descent! cs
	   (max (compose-state/descent cs)
		(abs (font-instance/descent fi)))))
	((vertical)
	 (let ((sb (/ (abs (font-instance/width fi)) 2)))
	   (compose-state/set-ascent! cs
	     (max (compose-state/ascent cs) sb))
	   (compose-state/set-descent! cs
	     (max (compose-state/descent cs) sb)))))
      (compose-state/set-font-instance! cs fi))
    (define (loop csl)
      (call/cc
	(lambda (exit)
	  (let ((lgi (glyph-stream/read lgs)) (cs (car csl)))
	    (if (glyph-stream/end-of-stream? lgs)
		(begin
		  (compose-state/set-lgs-pos! cs
		    (glyph-stream/position lgs))
		  (compose-state/set-pgs-pos! cs
		    (glyph-stream/fill pgs))
		  csl)
		(let ((rands (gi/rands lgi)))
		  (case (gi/rator lgi)
		    ((base)
		     (font-instance/glyph-metrics
		       (compose-state/font-instance cs) (cadr rands) gm)
		     (let ((lm (compose-state/measure cs))
			   (dm (glyph-metrics/escapement gm dir)))
		       (if (> (+ lm dm) (line/length l))
			   (exit (cdr csl))
			   (begin
			     (glyph-stream/write pgs
			       (make-gi 'base (append rands (list lm dm))))
			     (update-bbox cs gm)
			     (compose-state/set-measure! cs (+ lm dm))))))
		    ((space)
		     (compose-state/set-lgs-pos! cs
		       (glyph-stream/position lgs))
		     (compose-state/set-pgs-pos! cs
		       (glyph-stream/fill pgs))
		     (set! csl (cons (copy-compose-state cs) csl))
		     (set! cs  (car csl))
		     (let ((lm (compose-state/measure cs)))
		       (if (not (zero? lm))
			   (let ((dm))
			     (font-instance/space-metrics
			       (compose-state/font-instance cs) gm)
			     (set! dm (glyph-metrics/escapement gm
				        (line/orientation l)))
			     (if (> (+ lm dm) (line/length l))
				 (exit csl)
				 (begin
				   (glyph-stream/write pgs
				     (make-gi 'space (list (car rands) lm dm)))
				   (compose-state/set-measure! cs
				     (+ lm dm))))))))
		    ((attach))
		    ((line-break)
		     (compose-state/set-lgs-pos! cs
		       (glyph-stream/position lgs))
		     (compose-state/set-pgs-pos! cs
		       (glyph-stream/fill pgs))
		     (exit csl))
		    ((nbsp))
		    ((bidi))
		    ((font)
		     (let ((fi (apply make-font-instance rands)))
		       (set-font! cs fi)
		       (glyph-stream/write pgs (make-gi 'font (list fi))))))
		  (loop csl)))))))
    (let ((ics (alloc-compose-state)))
      (compose-state/set-lgs-pos! ics (glyph-stream/position lgs))
      (if (not (null? initial-font-instance))
	  (begin
	    (set-font! ics initial-font-instance)
	    (glyph-stream/write pgs
              (make-gi 'font (list initial-font-instance)))))
      (loop (list ics)))))

(define (line/layout l lgs cu)
  (let ((pgs (make-glyph-stream 'physical))
	(lgs-start (glyph-stream/position lgs)))
    (let* ((csl (line/compose l lgs pgs (content-unit/last-font cu))))
      (let ((cs (car csl)))
	(glyph-stream/set-position! lgs (compose-state/lgs-pos cs))
	(glyph-stream/set-fill! pgs (compose-state/pgs-pos cs))
	(line/set-ascent! l (compose-state/ascent cs))
	(line/set-descent! l (compose-state/descent cs))
	(line/set-bbox! l (compose-state/bbox cs))
	(line/set-logical-glyph-stream! l lgs)
	(line/set-logical-glyph-stream-start! l lgs-start)
	(line/set-logical-glyph-stream-end! l (glyph-stream/position lgs))
	(line/set-physical-glyph-stream! l pgs)
	(content-unit/set-last-font! cu (compose-state/font-instance cs))
	(free-compose-states csl)
	unspecific))))
