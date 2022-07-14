
;
; Adobe Font Metrics Functions
;

(declare (usual-integrations))

(define (afm-cache/load-afm-table afm-cache)
  (let ((version)
	(afm-count)
	(afm-entry-size)
	(string-vector)
	(lookup-table-offset)
	(glyph-table-offset-vector)
	(afm-table-vector)
	(ip (current-input-port)))
    (letrec ((load-entries
	      (lambda ()
		(let load ((k 0))
		  (if (< k afm-count)
		      (let ((afm (make-afm))
			    (sp (input-port/position ip)))
			(afm/set-font-name! afm (read-16))
			(afm/set-full-name! afm (read-16))
			(afm/set-family-name! afm (read-16))
			(afm/set-weight! afm (read-16))
			(afm/set-character-set! afm (read-16))
			(afm/set-encoding-scheme! afm (read-16))
			(afm/set-ascender! afm (read-16 #t))
			(afm/set-descender! afm (read-16 #t))
			(afm/set-cap-height! afm (read-16 #t))
			(afm/set-x-height! afm (read-16 #t))
			(let* ((llx (read-16 #t))
			       (lly (read-16 #t))
			       (urx (read-16 #t))
			       (ury (read-16 #t)))
			  (afm/set-font-bbox! afm
			    (make-rect
			      (make-point llx lly) (make-point urx ury))))
			(afm/set-underline-position! afm (read-16 #t))
			(afm/set-underline-thickness! afm (read-16 #t))
			(afm/set-italic-angle! afm (read-16.16))
			(afm/set-fixed-pitch?! afm
			  (if (zero? (read-16)) #f #t))
			(afm/set-screen-font?! afm
			  (if (zero? (read-16)) #f #t))
			(afm/set-screen-font-size! afm (read-16))
			(let ((n (- afm-entry-size
				    (- (input-port/position ip) sp))))
			  (if (positive? n)
			      (let skip ((k 0))
				(if (< k n)
				    (begin
				      (read-char)
				      (skip (1+ k)))))))
			(vector-set! afm-table-vector k afm)
			(load (1+ k)))))))
	     (load-strings
	      (lambda ()
		(let* ((ns (read-16))
		       (sv (make-vector ns)))
		  (let skip ((k 0))
		    (if (< k ns)
			(begin
			  (read-16)
			  (skip (1+ k)))))
		  (let ((ecs (char-set #\Nul)))
		    (let load ((k 0))
		      (if (< k ns)
			  (begin
			    (vector-set! sv k (read-string ecs))
			    (read-char)
			    (load (1+ k))))))
		  sv)))
	     (load-glyph-table-offsets
	      (lambda ()
		(let ((ov (make-vector afm-count)))
		  (let load ((k 0))
		    (if (< k afm-count)
			(begin
			  (vector-set! ov k (+ (read-32) lookup-table-offset))
			  (load (1+ k)))))
		  ov)))
	     (update-strings-and-glyph-offsets
	      (lambda ()
		(let update ((k 0))
		  (if (< k afm-count)
		      (let ((afm (vector-ref afm-table-vector k)))
			(afm/set-font-name! afm
			  (string-copy
			    (vector-ref string-vector
			      (afm/font-name afm))))
			(afm/set-full-name! afm
			  (string-copy
			    (vector-ref string-vector
			      (afm/full-name afm))))
			(afm/set-family-name! afm
			  (string-copy
			    (vector-ref string-vector
			      (afm/family-name afm))))
			(afm/set-weight! afm
			  (string-copy
			    (vector-ref string-vector
			      (afm/weight afm))))
			(afm/set-character-set! afm
			  (string-copy
			    (vector-ref string-vector
			      (afm/character-set afm))))
			(afm/set-encoding-scheme! afm
			  (string-copy
			    (vector-ref string-vector
			      (afm/encoding-scheme afm))))
			(afm/set-glyph-table-offset! afm
			  (vector-ref glyph-table-offset-vector k))
			(update (1+ k)))))))
	     (load-afm-table
	      (lambda (at)
		(let load ((k 0))
		  (if (< k afm-count)
		      (let ((afm (vector-ref afm-table-vector k)))
			(hash-table/put! at (afm/font-name afm) afm)
			(load (1+ k))))))))
      (set! version (read-16 #t))
      (set! version (+ version (exact->inexact (/ (read-16) 10000))))
      (set! afm-count (read-16))
      (set! afm-table-vector (make-vector afm-count))
      (set! afm-entry-size (read-16))
      (set! lookup-table-offset (read-32))
      (load-entries)
      (set! string-vector (load-strings))
      (input-port/set-position (current-input-port) lookup-table-offset)
      (set! glyph-table-offset-vector (load-glyph-table-offsets))
      (update-strings-and-glyph-offsets)
      (load-afm-table (afm-cache/afm-table afm-cache)))))

(define (afm-cache/load afm-cache)
  (afm-cache/set-file-name! afm-cache
    (canonicalize-input-filename (afm-cache/file-name afm-cache)))
  (with-input-from-file (afm-cache/file-name afm-cache)
    (lambda ()
      (afm-cache/load-afm-table afm-cache))))

(define (afm-cache/preload #!optional force-reload)
  (if (default-object? force-reload)
      (set! force-reload #f))
  (if (or (null? *afm-cache*) force-reload)
      (let* ((at (make-string-hash-table))
	     (ac (make-afm-cache *default-afm-cache-file-name* at)))
	(afm-cache/load ac)
	(set! *afm-cache* ac)))
  unspecific)

(define (afm-cache/lookup font-name)
  (hash-table/get (afm-cache/afm-table *afm-cache*) font-name '()))

(define (afm-cache/for-each receiver)
  (let ((at (and *afm-cache* (afm-cache/afm-table *afm-cache*))))
    (if at
	(hash-table/for-each at
	  (lambda (name afm)
	    name					;ignore
	    (receiver (afm/font-spec afm) afm))))
    unspecific))

;
; Basic AFM Procedures
;

(define (afm/extract-style afm)
  (let ((weight 'medium) (posture 'upright) (width 'medium))
    (letrec ((parse
	      (lambda ()
		(let loop ((key (read)))
		  (if (not (eof-object? key))
		      (begin
			(case key
			  ((black bold semibold demibold medium regular light
				  poster nord super display heavy thin)
			   (set! weight key))
			  ((italic oblique sloped slanted inclined upright)
			   (set! posture key))
			  ((extended condensed compact narrow compressed)
			   (set! width key)))
			(loop (read))))))))
      (with-input-from-string (afm/full-name afm)
	(lambda ()
	  (read)	;skip family name
	  (parse)))
      (list (cons 'weight weight)
	    (cons 'posture posture)
	    (cons 'width width)))))

(define (afm/font-spec afm)
  (list (afm/family-name afm)
	(afm/extract-style afm)
	(if (afm/screen-font? afm)
	    (cons 'points (afm/screen-font-size afm))
	    '*)))

;
; Note:
;
; 1. Use a hack for glyph properties for now!  [glyph index 32 is
; hardwired as whitespace!!]
;
  
(define (afm/load-glyphs afm)
  (let ((fg) (ng) (gv))
    (letrec ((load-glyphs
	      (lambda ()
		(let load ((k 0))
		  (if (< k ng)
		      (let* ((w0x (read-16 #t))
			     (w0y (read-16 #t))
			     (w1x (read-16 #t))
			     (w1y (read-16 #t))
			     (llx (read-16 #t))
			     (lly (read-16 #t))
			     (urx (read-16 #t))
			     (ury (read-16 #t))
			     (w0 '())
			     (w1 '())
			     (es (cons '() '()))
			     (bb '())
			     (pl '()))
			(if (not (= w0x #x7fff))
			    (set-car! es (make-point w0x w0y)))
			(if (not (= w1x #x7fff))
			    (set-cdr! es (make-point w1x w1y)))
			(if (not (= llx #x7fff))
			    (set! bb
			      (make-rect
			        (make-point llx lly) (make-point urx ury))))
			(if (= (+ k fg) 32)			;XXX
			    (set! pl '((whitespace . #t))))	;XXX
			(if (or w0 w1 bb pl)
			    (vector-set! gv k (make-glyph-metrics es bb pl)))
			(load (1+ k))))))))
      (input-port/set-position
        (current-input-port) (afm/glyph-table-offset afm))
      (if (not (= 8 (read-16)))
	  (error "Invalid glyph table:" afm))
      (set! fg (read-16))
      (set! ng (read-16))
      (set! gv (make-vector ng))
      (load-glyphs)
      (afm/set-first-glyph! afm fg)
      (afm/set-glyphs! afm gv)
      (afm/set-glyphs-loaded?! afm #t))))

(define (afm/load-afm-glyphs afm)
  (if (and (null? (afm/glyphs-loaded? afm)) *afm-cache*)
      (with-input-from-file (afm-cache/file-name *afm-cache*)
	(lambda ()
	  (afm/load-glyphs afm))))
  (afm/glyphs-loaded? afm))

(define (afm/glyph-metrics afm gno)
  (if (null? (afm/glyphs-loaded? afm))
      (afm/load-afm-glyphs afm))
  (let* ((fg (afm/first-glyph afm))
	 (gv (afm/glyphs afm))
	 (gi (- gno fg)))
    (if (or (null? gv)
	    (< gno fg)
	    (>= gi (vector-length gv)))
	'()
	(vector-ref gv gi))))

;
; AFM Extensions (note: extensions may return promises)
;

;
; The following are temporary only!
;

(define (afm/glyph-complement afm)					; XXX
  (call/cc
    (lambda (exit)
      (cond
        ((string=? (afm/encoding-scheme afm) "AdobeStandardEncoding")
	 'adobe-standard)
	((string=? (afm/family-name afm) "Symbol")
	 'adobe-symbol)
	((string=? (afm/family-name afm) "Baghdad")
	 'apple-arabic)
	(else
	 (with-input-from-string (afm/full-name afm)
	   (lambda ()
	     (let loop ((token (read)))
	       (if (not (eof-object? token))
		   (begin
		     (case token
		       ((expert)
			(exit 'adobe-expert))
		       ((ornaments)
			(exit 'adobe-ornaments)))
		     (loop (read)))))
	     '())))))))

(define (afm/extended-metrics afm)					; XXX
  (cond
   ((string=? (afm/family-name afm) "Baghdad")
    *baghdad-extended-metrics*)
   ((string=? (afm/family-name afm) "Ramatgan")
    *ramatgan-extended-metrics*)
   (else
    '())))

(define (afm/writing-systems afm)					; XXX
  (case (string->symbol (string-downcase (afm/family-name afm)))
    ((courier helvetica times)
     (list 'english))
    ((utopia)
     (if (string=? (afm/encoding-scheme afm) "AdobeStandardEncoding")
	 (list 'english)
	 '()))
    ((baghdad)
     (list 'arabic 'farsi 'urdu))
    ((ramatgan)
     (list 'hebrew 'yiddish 'ladino))
    (else
     '())))
