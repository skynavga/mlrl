
;
; Postscript Graphics Device Support
;

;
; Postscript Object Definitions
;

(declare (usual-integrations))

(define-record postscript-device (scheme-graphics-device font-table font-count))

(begin
  (define ps/dispatch
    (lambda (pd rator . rands)
      (let ((proc
	     (case rator
	       ((close) ps/close)
	       ((bbox) ps/bbox)
	       ((push-xfrm!) ps/push-xfrm!)
	       ((pop-xfrm!) ps/pop-xfrm!)
	       ((set-point!) ps/set-point!)
	       ((set-point-relative!) ps/set-point-relative!)
	       ((set-line-style!) ps/set-line-style!)
	       ((set-color!) ps/set-color!)
	       ((set-font!) ps/set-font!)
	       ((download-prologue) ps/download-prologue)
	       ((download-epilogue) ps/download-epilogue)
	       ((font-ascent) ps/font-ascent)
	       ((font-descent) ps/font-descent)
	       ((font-glyph-metrics) ps/font-glyph-metrics)
	       ((draw-rectangle) ps/draw-rectangle)
	       ((fill-rectangle) ps/fill-rectangle)
	       ((clear-rectangle) ps/clear-rectangle)
	       ((draw-grid) ps/draw-grid)
	       ((draw-crosshair) ps/draw-crosshair)
	       ((show-glyphs!) ps/show-glyphs!)
	       ((show-glyph-string!) ps/show-glyph-string!)
	       ((fonts) ps/fonts)
	       (else
		(lambda rest
		  rest ;ignore
		  (error "Unknown graphics operation:" rator))))))
	(apply proc (cons pd rands)))))
  (2d-put! 'postscript 'dispatch ps/dispatch))

(define *postscript-defs*
  "/Tj { show } bind def
   /M  { moveto } bind def
   /Mx { 0 moveto } bind def
   /My { 0 exch moveto } bind def
   /R  { rmoveto } bind def
   /Rx { 0 rmoveto } bind def
   /Ry { 0 exch rmoveto } bind def
   /gs {
     /dy exch def
     /dx exch def
     /h  exch def
     /w  exch def
     gsave
     translate
     0.33 setgray
     newpath 0 dx w { 0 moveto 0 h rlineto } for stroke
     newpath 0 dy h { 0 exch moveto w 0 rlineto } for stroke
     grestore
   } bind def
   /ch {
     /l exch def
     gsave
     translate
     l 8 div setlinewidth
     newpath l neg 0 moveto l 0 lineto 0 l moveto 0 l neg lineto stroke
     grestore
   } bind def" )

(define *printer-defs*
  "/rectfill { } bind def
   /rectstroke {
     gsave
     newpath
     /h exch def
     /w exch def
     moveto
     w 0 rlineto
     0 h rlineto
     w neg 0 rlineto
     closepath
     stroke
     grestore
   } bind def" )


(define (ps/send d ps)
  (graphics-operation d 'draw-postscript ps)
  unspecific)

(define (ps/move-to d x y)
  (cond
   ((zero? y)
    (ps/send d (format #f "~A Mx" x)))
   ((zero? x)
    (ps/send d (format #f "~A My" y)))
   (else
    (ps/send d (format #f "~A ~A M" x y)))))

(define (ps/rel-move-to d x y)
  (cond
   ((zero? y)
    (ps/send d (format #f "~A Rx" x)))
   ((zero? x)
    (ps/send d (format #f "~A Ry" y)))
   (else
    (ps/send d (format #f "~A ~A R" x y)))))

;
; Side Effects: user dictionary
;

(begin
  (define ps/open
    (lambda (width height)
      (let ((d (make-graphics-device
		 schematik-style-graphics-device-type
		 'points width height)))
	(graphics-set-coordinate-limits d 0 0 width height)
	(ps/send d *postscript-defs*)
	(make-postscript-device d (make-string-hash-table) 0))))
  (2d-put! 'postscript 'open ps/open))

(define ps/close
  (lambda (pd)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (graphics-close d))))
    
(define ps/bbox
  (lambda (pd)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (with-values
	  (lambda ()
	    (graphics-coordinate-limits d))
	(lambda (llx lly urx ury)
	  (make-rect (make-point llx lly) (make-point urx ury)))))))

(define roundoff
  (let* ((sv #(1. 10. 100. 1000. 10000.))
	 (svlen (vector-length sv)))
    (lambda (n digits)
      (if (and (integer? n) (exact? n))
	  n
	  (begin
	    (if (exact? n)
		(set! n (exact->inexact n)))
	    (let* ((ip (flo:truncate n))
		   (fp (flo:- n ip))
		   (rs (if (< digits svlen)
			   (vector-ref sv digits)
			   (exact->inexact (expt 10 digits))))
		   (rf (flo:/ (flo:round (flo:* fp rs)) rs)))
	      (if (flo:zero? rf)
		  (inexact->exact ip)
		  (flo:+ ip rf))))))))
      
;
; Side Effects: graphics state ( graphics state stack, CTM )
;
; N.B.: the following must be paired with a subsequent invocation of
; ps/pop-xfrm!.

(define (ps/push-xfrm! pd xfrm)
  (let ((d (postscript-device/scheme-graphics-device pd)))
    (ps/send d
	     (format #f
		   "gsave [~A ~A ~A ~A ~A ~A] concat"
		   (xfrm/a  xfrm)
		   (xfrm/b  xfrm)
		   (xfrm/c  xfrm)
		   (xfrm/d  xfrm)
		   (roundoff (xfrm/tx xfrm) 2)
		   (roundoff (xfrm/ty xfrm) 2)))))

;
; Side Effects: graphics state ( graphics state stack, CTM )
;
; N.B.: the following must be paired with a prior invocation of
; ps/push-xfrm!.

(define (ps/pop-xfrm! pd)
  (let ((d (postscript-device/scheme-graphics-device pd)))
    (ps/send d "grestore")))

;
; Side Effects: graphics state ( point )
;

(define ps/set-point!
  (lambda (pd pt)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/move-to d (roundoff (point/x pt) 2) (roundoff (point/y pt) 2))
      unspecific)))

(define ps/set-point-relative!
  (lambda (pd pt)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/rel-move-to d (roundoff (point/x pt) 2) (roundoff (point/y pt) 2))
      unspecific)))

;
; Side Effects: graphics state ( color )
;

(define ps/set-color!
  (lambda (pd r g b)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/send d (format #f "~A ~A ~A setrgbcolor" r g b)))))

;
; Side Effects: graphics state ( dash pattern )
;

(define ps/set-line-style!
  (lambda (pd style)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (case style
	((dotted) (ps/send d "[1 4] 11 setdash"))
	((dashed) (ps/send d "[3 5] 6 setdash"))
	((solid) (ps/send d "[] 0 setdash")))
      unspecific)))

(define ps/draw-rectangle
  (lambda (pd rect)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/send d
	       (format #f
		       "~A ~A ~A ~A rectstroke"
		       (roundoff (rect/origin-x rect) 2)
		       (roundoff (rect/origin-y rect) 2)
		       (roundoff (rect/width rect) 2)
		       (roundoff (rect/height rect) 2)))
      unspecific)))

(define ps/fill-rectangle
  (lambda (pd rect)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/send d
	       (format #f
		       "~A ~A ~A ~A rectfill"
		       (roundoff (rect/origin-x rect) 2)
		       (roundoff (rect/origin-y rect) 2)
		       (roundoff (rect/width rect) 2)
		       (roundoff (rect/height rect) 2)))
      unspecific)))

(define ps/clear-rectangle
  (lambda (pd rect)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/send d
	       (format #f
		       "gsave 1 setgray ~A ~A ~A ~A rectfill grestore"
		       (roundoff (rect/origin-x rect) 2)
		       (roundoff (rect/origin-y rect) 2)
		       (roundoff (rect/width rect) 2)
		       (roundoff (rect/height rect) 2)))
      unspecific)))

(define ps/draw-grid
  (lambda (pd rect grid-units)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/send d
	       (format #f
		       "~A ~A ~A ~A ~A ~A gs"
		       (rect/origin-x rect)
		       (rect/origin-y rect)
		       (point/x (rect/ur rect))
		       (point/y (rect/ur rect))
		       (exact->inexact (/ (rect/width rect) grid-units))
		       (exact->inexact (/ (rect/height rect) grid-units))))
      unspecific)))

(define ps/draw-crosshair
  (lambda (pd pt len)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (ps/send d (format #f "~A ~A ~A ch"
			 (exact->inexact (point/x pt))
			 (exact->inexact (point/y pt))
			 (exact->inexact len)))
      unspecific)))

;
; Side Effects: user dictionary
;

(define ps/make-font!
  (lambda (pd fn fs)
    (let* ((d (postscript-device/scheme-graphics-device pd))
	   (fc (postscript-device/font-count pd))
	   (id (string-append "f" (number->string fc))))
      (postscript-device/set-font-count! pd (1+ fc))
      (ps/send d (format #f "/~A /~A findfont ~A scalefont def" id fn fs))
      id)))

;
; Side Effects: graphics state ( font )
;

(define ps/set-font!
  (lambda (pd fi)
    (let* ((d (postscript-device/scheme-graphics-device pd))
	   (ft (postscript-device/font-table pd))
	   (fn (afm/font-name
		(font/device-font (font-instance/font fi))))
	   (fs (font-instance/size fi))
	   (id (hash-table/lookup
	         ft
		 fn
		 (lambda (fl)
		   (1d-table/lookup
		     fl
		     fs
		     (lambda (id) id)
		     (lambda ()
		       (let ((id (ps/make-font! pd fn fs)))
			 (1d-table/put! fl fs id)
			 id))))
		 (lambda ()
		   (let ((fl (make-1d-table))
			 (id (ps/make-font! pd fn fs)))
		     (1d-table/put! fl fs id)
		     (hash-table/put! ft fn fl)
		     id)))))
      (ps/send d (format #f "~A setfont" id)))))

(define (ps/font-ascent pd fi)
  pd	;ignore
  (let ((afm (font/device-font (font-instance/font fi))))
    (/ (* (afm/ascender afm) (font-instance/size fi)) 1000.0)))

(define (ps/font-descent pd fi)
  pd	;ignore
  (let ((afm (font/device-font (font-instance/font fi))))
    (/ (* (afm/descender afm) (font-instance/size fi)) 1000.0)))

(define (ps/font-glyph-metrics pd fi gc gm)
  pd	;ignore
  (let* ((afm (font/device-font (font-instance/font fi)))
	 (fgm (afm/glyph-metrics afm gc)))
    (and fgm
	 (glyph-metrics/scale fgm (/ (font-instance/size fi) 1000.0) gm))))

;
; Printer Support
;

(define *standard-postscript-fonts*
  '( "Times-Roman"
     "Times-Italic"
     "Times-Bold"
     "Times-BoldItalic"
     "Helvetica"
     "Helvetica-Oblique"
     "Helvetica-Bold"
     "Helvetica-BoldOblique"
     "Courier"
     "Courier-Oblique"
     "Courier-Bold"
     "Courier-BoldOblique"
     "Symbol"
   ))

(define *postscript-font-directories*
  '( "/NextLibrary/Fonts/outline"
     "/LocalLibrary/Fonts/outline"
   ))

(define (ps/font-file-name fn)
  (define (canonicalize directory fn)
    (call/cc
      (lambda (exit)
	(bind-condition-handler
	  (list error-type:open-file)
	  (lambda (c)
	    c ; ignore
	    (exit '()))
	  (lambda ()
	    (canonicalize-input-filename (string-append directory "/" fn)))))))
  (let loop ((dl *postscript-font-directories*))
    (if (null? dl)
	'()
	(let ((cfn (canonicalize (car dl) fn)))
	  (if (not (null? cfn))
	      cfn
	      (loop (cdr dl)))))))

(define (ps/download-printer-font pd fn)
  (let ((ffn (ps/font-file-name fn))
	(d (postscript-device/scheme-graphics-device pd)))
    (if (null? ffn)
	(warn (format #f "Can't find font ~A to download!" fn))
	(with-input-from-file ffn
          (lambda ()
	    (ps/send d (read-string (char-set))))))))

(define (ps/download-printer-fonts pd)
  (let ((d (postscript-device/scheme-graphics-device pd))
	(ft (postscript-device/font-table pd)))
    (hash-table/for-each ft
      (lambda (fn fl)
	fl ; ignore
	(if (not (member fn *standard-postscript-fonts*))
	    (ps/download-printer-font pd fn))))
    (hash-table/for-each ft
      (lambda (fn fl)
	(let loop ((fsl (1d-table/alist fl)))
	  (if (not (null? fsl))
	      (let ((fsp (car fsl)))
		(ps/send d
                  (format #f "/~A /~A findfont ~A scalefont def"
			  (cdr fsp) fn (car fsp))))))))))

(define (ps/download-prologue pd)
  (let ((d (postscript-device/scheme-graphics-device pd)))
    (ps/send d "%!")
    (ps/send d *postscript-defs*)
    (ps/send d *printer-defs*)
    (ps/download-printer-fonts pd)
    unspecific))

(define (ps/download-epilogue pd)
  (let ((d (postscript-device/scheme-graphics-device pd)))
    (ps/send d "showpage")))

;
; Side Effects: graphics state ( point )
;

(define ps/show-glyphs!
  (lambda (pd gv #!optional pt)
    (let ((d (postscript-device/scheme-graphics-device pd)))
      (letrec ((requires-escapes?
		(lambda (gv)
		  (call/cc
		    (lambda (exit)
		      (let ((n (vector-length gv)))
			(let loop ((k 0))
			  (if (>= k n)
			      #f
			      (let ((gc (vector-ref gv k)))
				(if (or (< gc #x20)
					(= gc #x28)
					(= gc #x29)
					(= gc #x5c)
					(> gc #x7e))
				    (exit #t)
				    (loop (1+ k)))))))))))
	       (escape
		(lambda (gc)
		  (cond
		    ((or (< gc #x20) (> gc #x7e))
		     (string-append "\\" (number->string gc 8)))
		    ((= gc #x28) "\\(")
		    ((= gc #x29) "\\)")
		    ((= gc #x5c) "\\\\")
		    (else (string gc)))))
	       (glyph-vector->glyph-string
		(lambda (gv)
		  (if (requires-escapes? gv)
		      (apply string-append
			     (map (lambda (gc)
				    (if (> gc 255)
					(string #\NUL)
					(escape gc)))
				  (vector->list gv)))
		      (let* ((n (vector-length gv))
			     (s (make-string n)))
			(let loop ((k 0))
			  (if (>= k n)
			      s
			      (begin
				(vector-8b-set! s k
				  (vector-ref gv k))
				(loop (1+ k))))))))))
	(if (not (default-object? pt))
	    (ps/move-to d
	      (roundoff (point/x pt) 2) (roundoff (point/y pt) 2)))
	(ps/send d (format #f "(~A) Tj" (glyph-vector->glyph-string gv)))))))

(define string->vector
  (lambda (s)
    (let* ((n (string-length s))
           (v (make-vector n)))
      (letrec ((loop
                (lambda (k)
                  (if (>= k n)
                      v
                      (begin
                        (vector-set! v k (vector-8b-ref s k))
                        (loop (1+ k)))))))
	(loop 0)))))

(define ps/show-glyph-string!
  (lambda (pd gs #!optional pt)
    (if (default-object? pt)
	(ps/show-glyphs! pd (string->vector gs))
	(ps/show-glyphs! pd (string->vector gs) pt))))

;
; Return list of font initialization arg lists, where each font
; initialization arg list is composed as follows:
;
; (<device-font> <glyph-comp> <ext-metrics> <font-spec> <writing-systems>)
;
; If computing glyph complement and/or extended metrics is non-trivial,
; then they should be promises which will be forced upon reference.
;

(define (ps/fonts pd)
  pd	;ignore
  (let ((fl '()))
    (afm-cache/preload)
    (afm-cache/for-each
      (lambda (font-spec afm)
	(set! fl (cons (list afm 
			     (afm/glyph-complement afm)
			     (afm/extended-metrics afm)
			     (font-spec/intern font-spec)
			     (afm/writing-systems afm))
		       fl))))
    (reverse fl)))
