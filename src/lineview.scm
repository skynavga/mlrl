
;
; Line Viewer
;

(define-record lineview (device line margins baseline-origin bbox scale))

(define *default-lineview-margins*
  (let ((m (in->pt 0.5)))
    (make-margins m m m m)))

(define (lineview/open v column-name lno #!optional viewer-height)
  (if (default-object? viewer-height)
      (set! viewer-height 300))
  (let ((l (viewer/find-line v column-name lno)))
    (if (null? l)
	'()
	(let* ((lm *default-lineview-margins*)
	       (sh (- viewer-height (+ (margins/top lm) (margins/bottom lm))))
	       (lb (line/bbox l))
	       (ls (/ sh (rect/height lb)))
	       (sw (* (line/length l) ls))
	       (so (make-point (margins/left lm) (margins/bottom lm)))
	       (bd (point/difference (make-point 0 0) (rect/ll (line/bbox l))))
	       (bo (point/add so (make-point 0 (* (point/y bd) ls))))
	       (sb (make-rect so (point/add so (make-point sw sh))))
	       (gd (device/open (viewer/device-type v)
				  (+ sw (margins/left lm) (margins/right lm))
				  (+ sh (margins/top lm) (margins/bottom lm))))
	       (lv (make-lineview gd l lm bo sb ls)))
	  (lineview/refresh lv)
	  lv))))

(define (lineview/close lv)
  (device/close (lineview/device lv)))

(define (lineview/refresh lv)
  (device/erase (lineview/device lv))
  (lineview/redisplay lv))

(define (lineview/show-line lv)
  (let* ((gd (lineview/device lv))
	 (line (lineview/line lv))
	 (pgs (line/physical-glyph-stream line))
	 (gl '())
	 (fi)
	 (rf (line/reverse-flow? line))
	 (lm (if rf (line/length line) 0))
	 (pt (make-point 0 0)))			;reusable point record
    (letrec ((flush-base
	      (lambda ()
		(if (> (length gl) 0)
		    (begin
		      (if rf
			  (begin
			    (point/set-x! pt lm)
			    (device/show-glyphs! gd (list->vector gl) pt))
			  (device/show-glyphs! gd (list->vector (reverse gl))))
		      (set! gl '())))
		unspecific))
	     (loop
	      (lambda (bl)
		(call/cc
		  (lambda (exit)
		    (if (glyph-stream/end-of-stream? pgs)
			(begin
			  (flush-base)
			  (exit bl)))
		    (let* ((gi (glyph-stream/read pgs))
			   (rator (gi/rator gi))
			   (rands (gi/rands gi)))
		      (case rator
			((end-of-stream)
			 (flush-base)
			 (exit bl))
			((base attach)
			 (let* ((gc (second rands))
				(dx (fourth rands))
				(gm (font-instance/glyph-metrics fi gc)))
			   (if rf
			       (set! lm (- lm dx)))
			   (set! bl (cons (list (make-point lm 0)
						(glyph-metrics/bbox gm)
						dx) bl))
			   (if (not rf)
			       (set! lm (+ lm dx)))
			   (set! gl (cons gc gl))))
;			((attach))
			((space)
			 (flush-base)
			 (let ((dx (third rands)))
			   (if rf
			       (set! lm (- lm dx))
			       (begin
				 (point/set-x! pt dx)
				 (device/set-point-relative! gd pt)
				 (set! lm (+ lm dx))))))
			((font)
			 (flush-base)
			 (set! fi (car rands))
			 (device/set-font! gd fi)))
		      (loop bl)))))))
      (glyph-stream/rewind pgs)
      (device/set-point! gd (make-point 0 0))
      (reverse (loop '())))))

(define (lineview/show-boxes lv bl)
  (let ((gd (lineview/device lv)))
    (letrec ((loop
	      (lambda (l)
		(if (not (null? l))
		    (let* ((b (car l))
			   (bb (cadr b)))
		      (if (not (rect/empty? bb))
			  (device/with-origin gd (car b)
			    (device/draw-rectangle gd bb)))
		      (loop (cdr l)))))))
      (loop bl))))

(define (lineview/show-crosshairs lv bl)
  (let ((gd (lineview/device lv))
	(chlen (/ (rect/height (line/bbox (lineview/line lv))) 16)))
    (letrec ((loop
	      (lambda (l)
		(if (not (null? l))
		    (let ((b (car l)))
		      (if (not (zero? (third b)))
			  (device/draw-crosshair gd (car b) chlen))
		      (loop (cdr l)))))))
      (loop bl))))

(define *lineview-origins-font*
  (font-spec/intern
    '("Helvetica" ((weight . medium) (posture . upright)) 10.0)))

(define lineview/show-origins
  (let ((fi '()) (fa 0))
    (lambda (lv bl)
      (let ((gd (lineview/device lv)))
	(if (null? fi)
	    (let* ((sfs *lineview-origins-font*)
		   (fc (device/font-catalog gd)))
	      (set! fi (font-catalog/ascii-font-instance fc sfs))
	      (if (not (null? fi))
		  (set! fa (font-instance/ascent fi)))))
	(let* ((ls (lineview/scale lv))
	       (line (lineview/line lv))
	       (rf (line/reverse-flow? line))
	       (ll (line/length line))
	       (oo (* (/ (rect/height (line/bbox line)) 32) ls))
	       (od (make-point oo (- (max (* fa 2) oo)))))
	  (letrec ((loop
		    (lambda (l)
		      (if (not (null? l))
			  (let* ((b  (car l))
				 (bo (car b))
				 (pt (point/add (point/scale bo ls ls) od))
				 (x  (point/x bo))
				 (ux (if rf (- ll x) x))
				 (rx (/ (floor (* ux 100)) 100))
				 (ry (/ (floor (* (point/y bo) 100)) 100)))
			    (if (not (zero? (third b)))
				(device/show-glyph-string! gd
			          (format #f "[~A,~A]" rx ry) pt))
			    (loop (cdr l)))))))
	    (if (not (null? fi))
		(device/set-font! gd fi))
	    (loop bl)))))))

(define (lineview/redisplay lv)
  (let* ((gd (lineview/device lv))
	 (lb (lineview/bbox lv))
	 (ls (lineview/scale lv)))
    (device/set-line-style! gd 'solid)
    (device/draw-rectangle gd lb)
    (device/with-origin gd (lineview/baseline-origin lv)
      (device/with-scale gd ls ls
        ; draw glyphs in light blue
	(device/set-color! gd 0.8 0.8 1)
        (let ((bl (lineview/show-line lv)))
	  ; draw glyph bounding boxes black
	  (if (not (null? bl))
	      (begin
		(device/set-color! gd 0 0 0)
		(lineview/show-boxes lv bl)))
	  ; draw baseline in red
	  (device/set-color! gd 1 0 0)
	  (device/draw-rectangle gd
	    (make-rect (make-point 0 0)
		       (make-point (line/length (lineview/line lv)) 0)))
	  ; draw crosshairs in light green
	  (if (not (null? bl))
	      (begin
		(device/set-color! gd 0.1 1 0.1)
		(lineview/show-crosshairs lv bl)
		(device/set-color! gd 0 0 0)
		(device/with-scale gd (/ 1 ls) (/ 1 ls)
		  (lineview/show-origins lv bl)))))))))
	
