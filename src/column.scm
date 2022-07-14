
;
; Column Module
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(declare (usual-integrations))

(define-record column
  ( page			; page in which column appears
    device			; device column is formatted for
    name			; column name
    content-type		; primary content type tag
    origin			; origin in page's coordinate space
    width			; width
    height			; height (or depth)
    bbox			; ink bounding box
    orientation			; line orientation
    progression			; line progression
    reverse-fill?		; reverse column line fill
    border			; border present
    first-line			; first line in column
    last-line			; last line in column
    next			; next column in column's page
    previous			; previous column in column's page
    next-content		; next column in content stream
    previous-content		; previous column in content stream
  ))

(define make-column
  (let ((old-make-column make-column))
    (lambda (p nm content-type origin wd ht orient prog rev-fill? border)
      (old-make-column
       p
       '()
       nm
       content-type
       origin
       wd
       ht
       (make-empty-rect)
       orient
       prog
       rev-fill?
       border
       '()
       '()
       '()
       '()
       '()
       '()))))

(define column/device
  (let ((device column/device)
	(set-device! column/set-device!))
    (lambda (col)
      (or (device col)
	  (let ((d (page/device (column/page col))))
	    (set-device! col d)
	    d)))))

(define (column/font-catalog col)
  (device/font-catalog (column/device col)))

(define (column/rect col)
  (let ((co (column/origin col)))
    (make-rect co (make-point
		   (+ (point/x co) (column/width col))
		   (+ (point/y co) (column/height col))))))

(define (column/previous-content-line col)
  (if (null? col)
      '()
      (let ((pc (column/previous-content col)))
	(and pc (column/last-line pc)))))

(define (column/baseline-delta col l leading)
  (let* ((pl (line/previous l)))
    (case (column/progression col)
      ((normal)
       (cond
	 ((zero? leading)
	  (- (+ (if pl (line/descent pl) 0) (line/ascent l))))
	 ((positive? leading)
	  (let ((dy (+ (if pl (line/descent pl) 0)
		       (line/ascent l))))
	    (- (if (< dy leading) leading dy))))
	 (else leading)))
      ((reverse)
       (cond
	 ((zero? leading)
	  (+ (if pl (line/ascent pl) 0) (line/descent l)))
	 ((positive? leading)
	  (let ((dy (+ (if pl (line/ascent pl) 0)
		       (line/descent l))))
	    (if (< dy leading) leading dy)))
	 (else (- leading)))))))

(define (column/initial-baseline col)
  (case (column/orientation col)
    ((horizontal)
     (case (column/progression col)
       ((normal)
	(column/height col))
       ((reverse)
	0)))
    ((vertical)
     (case (column/progression col)
       ((normal)
	(column/width col))
       ((reverse)
	0)))))

;
; Return baseline point for given line if it fits in column
; being formatted; otherwise, return #f.
;

(define (column/next-baseline col l previous-baseline leading)
  (let* ((bd (column/baseline-delta col l leading))
	 (bl (+ previous-baseline bd)))
    (case (column/orientation col)
      ((horizontal)
       (if (negative? (- bl (line/descent l))) #f bl))
      ((vertical)
       (case (column/progression col)
	 ((normal)
	  (if (negative? (- bl (line/descent l))) #f bl))
	 ((reverse)
	  (if (> (+ bl (line/descent l)) (column/width col)) #f bl)))))))
	      
(define (column/redisplay col #!optional erase)
  (let ((d (column/device col))
	(cr (list->rect (list 0 0 (column/width col) (column/height col)))))
    (device/with-origin d (column/origin col)
      (if (and (not (default-object? erase)) erase)
	  (device/clear-rectangle d cr))
      (case (column/border col)
	((solid dashed dotted)
	 (device/set-line-style! d (column/border col))
	 (device/draw-rectangle d cr)))
      (let redisplay-line ((l (column/first-line col)))
	(if (not (null? l))
	    (begin
	      (line/redisplay l)
	      (redisplay-line (line/next l)))))
      unspecific)))

(define (column/refresh col)
  (column/redisplay col #t))

(define-record column-formatting (column baseline))

(define (make-column-formatting col)
  ((record-constructor (record-type column-formatting)) col '()))

(define (column-formatting/format cf cu)
  (let ((col (column-formatting/column cf)))
    (if (null? (column-formatting/baseline cf))
	(column-formatting/set-baseline! cf (column/initial-baseline col)))
    (let ((lgs (content-unit/rendering cu (column/font-catalog col))))
      (glyph-stream/set-position! lgs
        (line/content-position (column/previous-content-line col) cu))
      (column-formatting/layout cf lgs cu))))

;
; Layout content into column formatting's column until we run out of
; content or we hit the end of the column.  Return #t if room remains
; in column for more content; otherwise, return #f.
;
; Notes:
;
; 1. Should encapsulate <lgs>, <style>, and <content unit state> into
; single object which is passed in as an argument.  The only reason the
; content unit is passed in is to record the last font seen while formatting
; that content unit.  Perhaps this should go into the column-formatting
; state?
;

(define (column-formatting/layout cf lgs cu)
  (let* ((col (column-formatting/column cf))
	 (s (content-unit/style cu))
	 (dir (column/orientation col))
	 (leading (style/get s 'leading 0)))
    (define (new-line)
      (let* ((pl (column/last-line col))
	     (lno (if (and pl (eq? lgs (line/logical-glyph-stream pl)))
		      (1+ (line/number pl)) 0))
	     (si (style/get s 'start-indentation 0))
	     (ei (style/get s 'end-indentation 0))
	     (rf (style/get s 'reverse-flow? #f))
	     (ll)
	     (lo))
	(if (< lno (style/get s 'indent-first-lines 0))
	    (set! si (+ si (style/get s 'first-indentation 0))))
	(case dir
	  ((horizontal)
	   (set! ll (- (column/width col) (+ si ei)))
	   (set! lo (make-point (if rf ei si) 0)))
	  ((vertical)
	   (set! ll (- (column/height col) (+ si ei)))
	   (set! lo (make-point 0 (if rf ei si)))))
	(make-line col pl lno lo ll rf cu)))
    (define (add-line! l bl)
      (if (null? (column/first-line col))
	  (column/set-first-line! col l))
      (let ((pl (column/last-line col)))
	(line/set-previous! l pl)
	(if (not (null? pl))
	    (line/set-next! pl l))
	(column/set-last-line! col l)
	(if (and (zero? (line/number l)) pl)
	    (let ((ps (content-unit/style (line/content pl))))
	      (case (column/progression col)
		((normal)
		 (set! bl (+ bl (- (style/get ps 'spread-after 0))
			        (- (style/get s 'spread-before 0)))))
		((reverse)
		 (set! bl (+ bl (style/get ps 'spread-after 0)
			        (style/get s 'spread-before 0)))))))
	(line/set-baseline! l bl)
	(column-formatting/set-baseline! cf bl)))
    (let set-line ()
      (if (glyph-stream/end-of-stream? lgs)
	  #t						; end of content
	  (let ((sp (glyph-stream/position lgs))
		(l (new-line)))
	    (line/layout l lgs cu)
	    (let* ((obl (column-formatting/baseline cf))
		   (bl (column/next-baseline col l obl leading)))
	      (if (null? bl)
		  (begin
		    (glyph-stream/set-position! lgs sp)
		    #f)					; end of column
		  (begin
		    (add-line! l bl)
		    (set-line)))))))))

(define (column/find-line col line-number)
  (let loop ((l (column/first-line col)))
    (if (not (null? l))
	(if (= line-number (line/number l))
	    l
	    (loop (line/next l))))))
