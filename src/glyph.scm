
;
; Glyph Metrics
;

(declare (usual-integrations))

(define-record glyph-metrics (escapements bbox properties))

(define (make-glyph-metrics #!optional es bb pl)
  (if (default-object? es)
      (set! es (cons (make-point 0 0) (make-point 0 0))))
  (if (default-object? bb)
      (set! bb (make-empty-rect)))
  (if (default-object? pl)
      (set! pl '()))
  ((record-constructor (record-type glyph-metrics)) es bb pl))

;
; When scaling glyph metrics, use glyph metrics object if provided;
; otherwise, cons one.  The resulting glyph metrics object shares
; the property list with original glyph metrics.  So the latter should
; not be mutated!
;

(define (glyph-metrics/scale gm ds #!optional ngm)
  (if (default-object? ngm)
      (set! gm (make-glyph-metrics)))
  (let ((w0  (car (glyph-metrics/escapements gm)))
	(nw0 (car (glyph-metrics/escapements ngm))))
    (if (and w0 nw0)
	(begin
	  (point/set-x! nw0 (* (point/x w0) ds))
	  (point/set-y! nw0 (* (point/y w0) ds)))))
  (let ((w1  (cdr (glyph-metrics/escapements gm)))
	(nw1 (cdr (glyph-metrics/escapements ngm))))
    (if (and w1 nw1)
	(begin
	  (point/set-x! nw1 (* (point/x w1) ds))
	  (point/set-y! nw1 (* (point/y w1) ds)))))
  (let ((bb  (glyph-metrics/bbox gm))
	(nbb (glyph-metrics/bbox ngm)))
    (if (and bb nbb)
	(begin
	  (point/set-x! (rect/ll nbb) (* (point/x (rect/ll bb)) ds))
	  (point/set-y! (rect/ll nbb) (* (point/y (rect/ll bb)) ds))
	  (point/set-x! (rect/ur nbb) (* (point/x (rect/ur bb)) ds))
	  (point/set-y! (rect/ur nbb) (* (point/y (rect/ur bb)) ds)))))
  (glyph-metrics/set-properties! ngm (glyph-metrics/properties gm))
  ngm)

(define (glyph-metrics/escapement gm #!optional direction)
  (if (default-object? direction)
      (set! direction 'horizontal))
  (case direction
    ((horizontal)
     (let ((w0 (car (glyph-metrics/escapements gm))))
       (and w0 (point/x w0))))
    ((vertical)
     (let ((w1 (cdr (glyph-metrics/escapements gm))))
       (and w1 (point/y w1))))))

(define (glyph-metrics/cross-escapement gm #!optional direction)
  (if (default-object? direction)
      (set! direction 'horizontal))
  (case direction
    ((horizontal)
     (let ((w0 (car (glyph-metrics/escapements gm))))
       (and w0 (point/y w0))))
    ((vertical)
     (let ((w1 (cdr (glyph-metrics/escapements gm))))
       (and w1 (point/x w1))))))

(define (glyph-metrics/whitespace? gm)
  (and (assq 'whitespace (glyph-metrics/properties gm)) #t))

;
; Glyph Instructions and Glyph Streams
;

(define-record gi (rator rands))

(define-record glyph-stream (type position fill stream end-of-stream?))

(define (make-glyph-stream type)
  ((record-constructor (record-type glyph-stream)) type 0 0 '() #f))

(define (glyph-stream/set-position! gs pos)
  (if (<= pos (glyph-stream/fill gs))
      ((record-updater (record-type glyph-stream) 'position) gs pos))
  unspecific)

(define (glyph-stream/empty? gs)
  (zero? (glyph-stream/fill gs)))

(define (glyph-stream/set-fill! gs fill)
  (if (<= fill (glyph-stream/fill gs))
      ((record-updater (record-type glyph-stream) 'fill) gs fill))
  unspecific)

(define (glyph-stream/flush! gs)
  (glyph-stream/set-fill! gs 0))

(define (glyph-stream/rewind gs)
  (glyph-stream/set-position! gs 0)
  (glyph-stream/set-end-of-stream?! gs #f))

(define (glyph-stream/read gs)
  (let ((sv (glyph-stream/stream gs))
	(p (glyph-stream/position gs)))
    (if (glyph-stream/end-of-stream? gs)
	(make-gi 'end-of-stream '())
	(if (>= p (glyph-stream/fill gs))
	    (begin
	      (glyph-stream/set-end-of-stream?! gs #t)
	      (make-gi 'end-of-stream '()))
	    (let ((gi (vector-ref sv p)))
	      (glyph-stream/set-position! gs (1+ p))
	      gi)))))

(define (glyph-stream/read-at gs position)
  (if (>= position (glyph-stream/fill gs))
      (make-gi 'end-of-stream '())
      (vector-ref (glyph-stream/stream gs) position)))

(define (glyph-stream/unread gs gi)
  gi	;ignore
  (let ((p (glyph-stream/position gs)))
    (if (> p 0)
	(glyph-stream/set-position! gs (- p 1)))
    unspecific))

(define (glyph-stream/write gs gi)
  (let ((sv (glyph-stream/stream gs)))
    (if (null? sv)
	(begin
	  (set! sv (make-vector 64))
	  (glyph-stream/set-stream! gs sv)
	  (glyph-stream/set-fill! gs 0)))
    (let ((sf (glyph-stream/fill gs)))
      (if (>= sf (vector-length sv))
	  (begin
	    (set! sv (vector-grow sv (* (vector-length sv) 2)))
	    (glyph-stream/set-stream! gs sv)))
      (vector-set! sv sf gi)
      ((record-updater (record-type glyph-stream) 'fill) gs (1+ sf)))))

(define (glyph-stream/append! gs1 gs2)
  (let* ((sv (glyph-stream/stream gs1))
	 (fill (glyph-stream/fill gs1))
	 (have (- (vector-length sv) fill))
	 (need (glyph-stream/fill gs2)))
    (if (< have need)
	(set! sv (vector-grow sv (+ (vector-length sv) need)))
	(glyph-stream/set-stream! gs1 sv))
    (subvector-move-right! (glyph-stream/stream gs2) 0 have sv fill)
    ((record-updater (record-type glyph-stream) 'fill) gs1 (+ fill need))
    unspecific))

(define (glyph-stream/dump gs)
  (letrec ((loop
	    (lambda ()
	      (if (not (glyph-stream/end-of-stream? gs))
		  (begin
		    (pp (glyph-stream/read gs))
		    (loop))))))
    (let ((p (glyph-stream/position gs)))
      (glyph-stream/rewind gs)
      (loop)
      (glyph-stream/set-position! gs p)
      unspecific)))
