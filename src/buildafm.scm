
;
; Adobe Font Metrics Compiler
;

(declare (usual-integrations))

(define (afm/filename font-name #!optional dirname)
  (if (default-object? dirname)
      (set! dirname "."))
  (string-append dirname "/" font-name ".afm"))

(define afm/parse-glyph
  (let ((cs (char-set-invert (char-set #\Space #\;)))
	(ws char-set:whitespace)
	(ecs (char-set)))
    (lambda (s)
      (let ((g (make-afm-glyph))
	    (n (string-length s))
	    (k 0))
	(letrec ((next-field
		  (lambda ()
		    (if (>= k n)
			'()
			(let ((nk (substring-find-next-char s k n #\;)))
			  (if (not nk)
			      (set! nk n))
			  (let ((sf (string-trim (substring s k nk) cs)))
			    (set! k (1+ nk))
			    sf)))))
		 (loop
		  (lambda ()
		    (let ((sf (next-field)))
		      (if (or (null? sf) (zero? (string-length sf)))
			  g
			  (with-input-from-string sf
			    (lambda ()
			      (case (read)
			       ((c)
				(afm-glyph/set-number! g (read)))
			       ((w w0)
				(let ((p (vector-ref (afm-glyph/widths g) 0)))
				  (if (null? p)
				      (begin
					(set! p (make-point 0 0))
					(vector-set!
					  (afm-glyph/widths g) 0 p)))
				  (point/set-x! p (read))
				  (point/set-y! p (read))))
			       ((wx w0x)
				(let ((p (vector-ref (afm-glyph/widths g) 0)))
				  (if (null? p)
				      (begin
					(set! p (make-point 0 0))
					(vector-set!
					  (afm-glyph/widths g) 0 p)))
				  (point/set-x! p (read))))
			       ((wy w0y)
				(let ((p (vector-ref (afm-glyph/widths g) 0)))
				  (if (null? p)
				      (begin
					(set! p (make-point 0 0))
					(vector-set!
					  (afm-glyph/widths g) 0 p)))
				  (point/set-y! p (read))))
			       ((w1)
				(let ((p (vector-ref (afm-glyph/widths g) 1)))
				  (if (null? p)
				      (begin
					(set! p (make-point 0 0))
					(vector-set!
					  (afm-glyph/widths g) 1 p)))
				  (point/set-x! p (read))
				  (point/set-y! p (read))))
			       ((w1x)
				(let ((p (vector-ref (afm-glyph/widths g) 1)))
				  (if (null? p)
				      (begin
					(set! p (make-point 0 0))
					(vector-set!
					  (afm-glyph/widths g) 1 p)))
				  (point/set-x! p (read))))
			       ((w1y)
				(let ((p (vector-ref (afm-glyph/widths g) 1)))
				  (if (null? p)
				      (begin
					(set! p (make-point 0 0))
					(vector-set!
					  (afm-glyph/widths g) 1 p)))
				  (point/set-y! p (read))))
			       ((vv)
				(let* ((vx (read))
				       (vy (read)))
				  (afm-glyph/set-vvector!
				    g (make-point vx vy))))
			       ((n)
				(afm-glyph/set-name! g
				  (string-trim (read-string ecs) cs)))
			       ((b)
				(let* ((llx (read))
				       (lly (read))
				       (urx (read))
				       (ury (read)))
				  (afm-glyph/set-bbox! g
				    (make-rect (make-point llx lly)
					       (make-point urx ury)))))
			       ((l)
				(let* ((successor) (ligature))
				  (set! successor
					(string-trim (read-string ws) cs))
				  (set! ligature
					(string-trim (read-string ws) cs))
				  (afm-glyph/set-ligatures! g
				    (cons (cons successor ligature)
					  (afm-glyph/ligatures g)))))
			       (else
				(warn "Unknown AFM glyph field:" sf)))
			      (loop))))))))
	  (loop)
	  g)))))
    
(define (afm/add-glyph! afm g)
  (if (> (afm-glyph/number g) 0)
      (begin
	(if (null? (afm/first-glyph afm))
	    (afm/set-first-glyph! afm (afm-glyph/number g)))
	(if (null? (afm/glyphs afm))
	    (afm/set-glyphs! afm (list g))
	    (append! (afm/glyphs afm) (list g)))))
  unspecific)

(define (load-glyph-vector! l v fg)
  (letrec ((loop
	    (lambda (l)
	      (if (not (null? l))
		  (let ((g (car l)))
		    (vector-set! v (- (afm-glyph/number g) fg) g)
		    (loop (cdr l)))))))
    (loop l)))

(define (glyph-list->glyph-vector afm max-glyph-number)
  (let ((gv (make-vector (1+ (- max-glyph-number (afm/first-glyph afm))))))
    (load-glyph-vector! (afm/glyphs afm) gv (afm/first-glyph afm))
    (afm/set-glyphs! afm gv)
    unspecific))

(define (afm/load-glyphs afm glyph-count)
  (call/cc
    (lambda (exit)
      (let ((eol (char-set #\Newline))
	    (max-glyph-number 0))
	(letrec ((read-lines
		  (lambda ()
		    (let ((s (read-string eol)))
		      (if (eof-object? s)
			  (exit unspecific))
		      (with-input-from-string s
			(lambda ()
			  (case (read)
			    ((c)
			     (let ((g (afm/parse-glyph s)))
			       (afm/add-glyph! afm g)
			       (set! max-glyph-number
				     (max max-glyph-number
					  (afm-glyph/number g)))))
			    ((endcharmetrics)
			     (exit unspecific)))))
		      (read-char)
		      (read-lines)))))
	  (dynamic-wind
	    (lambda () unspecific)
	    (lambda () (read-lines))
	    (lambda () (glyph-list->glyph-vector afm max-glyph-number))))))))

(define (afm/load-kern-data afm)
  (call/cc
    (lambda (exit)
      (let ((eol (char-set #\Newline)))
	(letrec ((read-lines
		  (lambda ()
		    (let ((s (read-string eol)))
		      (if (eof-object? s)
			  (exit unspecific))
		      (with-input-from-string s
			(lambda ()
			  (case (read)
			    ((endkerndata)
			     (exit unspecific)))))
		      (read-char)
		      (read-lines)))))
	  (read-lines))))))

(define (afm/load-composites afm composites-count)
  (call/cc
    (lambda (exit)
      (let ((eol (char-set #\Newline)))
	(letrec ((read-lines
		  (lambda ()
		    (let ((s (read-string eol)))
		      (if (eof-object? s)
			  (exit unspecific))
		      (with-input-from-string s
			(lambda ()
			  (case (read)
			    ((endcomposites)
			     (exit unspecific)))))
		      (read-char)
		      (read-lines)))))
	  (read-lines))))))

(define (afm/load file-name)
  (call/cc
    (lambda (exit)
      (bind-condition-handler
        (list error-type:open-file)
	(lambda (c)
	  (condition/write-report c)
	  (exit '()))
	(lambda ()
	  (with-input-from-file file-name
	    (lambda ()
	      (let ((afm (make-afm))
		    (ecs (char-set))
		    (eol (char-set #\Newline))
		    (ip (current-input-port)))
		(letrec ((read-lines
			  (lambda ()
			    (let ((s (read-string eol)))
			      (if (eof-object? s)
				  (exit afm))
			      (with-input-from-string s
				(lambda ()
				  (case (read)
				    ((fontname)
				     (read-char)
				     (afm/set-font-name! afm
				       (read-string ecs)))
				    ((fullname)
				     (read-char)
				     (afm/set-full-name! afm
				       (read-string ecs)))
				    ((familyname)
				     (read-char)
				     (afm/set-family-name! afm
					(read-string ecs)))
				    ((weight)
				     (read-char)
				     (afm/set-weight! afm
					(read-string ecs)))
				    ((characterset)
				     (read-char)
				     (afm/set-character-set! afm
					(read-string ecs)))
				    ((encodingscheme)
				     (read-char)
				     (afm/set-encoding-scheme! afm
					(read-string ecs)))
				    ((ascender)
				     (afm/set-ascender! afm (read)))
				    ((descender)
				     (afm/set-descender! afm (read)))
				    ((capheight)
				     (afm/set-cap-height! afm (read)))
				    ((xheight)
				     (afm/set-x-height! afm (read)))
				    ((underlineposition)
				     (afm/set-underline-position! afm (read)))
				    ((underlinethickness)
				     (afm/set-underline-thickness! afm (read)))
				    ((italicangle)
				     (afm/set-italic-angle! afm (read)))
				    ((isfixedpitch)
				     (read-char)
				     (case (read)
				       ((false)
					(afm/set-fixed-pitch?! afm #f))
				       ((true)
					(afm/set-fixed-pitch?! afm #t))))
				    ((startcharmetrics)
				     (let ((count (read)))
				       (with-input-from-port ip
					 (lambda ()
					   (afm/load-glyphs afm count)))))
				    ((startkerndata)
				     (with-input-from-port ip
				       (lambda ()
					 (afm/load-kern-data afm))))
				    ((startcomposites)
				     (let ((count (read)))
				       (with-input-from-port ip
					 (lambda ()
					   (afm/load-composites afm count)))))
				    ((endfontmetrics)
				     (exit afm)))))
			      (read-char)
			      (read-lines)))))
		  (read-lines)
		  afm)))))))))


;
; Compiled AFM Cache Format
;
;     Compiled AFM File Format Version		# 4 bytes, required
;     # AFM Entries				# 2 bytes, required
;     AFM Entry Size (bytes)			# 2 bytes, required
;     Lookup Table Offset			# 4 bytes, required
;     Array of AFM Entries			# NE * ES bytes, optional
;     String Table*				# 2 NS + SS bytes, optional
;     Array of Lookup Tables*			# variable length, optional
;
;   *Must begin on DWORD boundary.
;
; AFM Entry Record Format
;
;     Font Name
;     Full Name
;     Family Name
;     Weight
;     Character Set
;     Encoding Scheme
;     Ascender
;     Descender
;     CapHeight
;     XHeight
;     Font BBOX
;     IsFixedPitch
;     UnderlinePosition
;     UnderlineThickness
;     ItalicAngle
;

(define (afm-cache/max-entry-size afm-table) *afm-entry-size*)

(define (afm-cache/entry-count afm-table)
  (hash-table/count afm-table))

(define (afm-cache/dump-header afm-table version lookup-table-offset)
  (write-16 (inexact->exact (floor version)))
  (write-16 (inexact->exact (floor (* 10000 version))))
  (write-16 (hash-table/count afm-table))
  (write-16 (afm-cache/max-entry-size afm-table))
  (write-32 lookup-table-offset)
  unspecific)

(define (afm-cache/dump-entries afm-table st es)
  (let ((p (current-output-port)))
    (let dump-entry ((l (hash-table/entries-list afm-table)))
      (if (not (null? l))
	(let ((name (caar l))
	      (afm (cdar l))
	      (pp (output-port/position p)))
	  (write-16 (afm-cache/string-table-index st name))
	  (write-16 (afm-cache/string-table-index st (afm/full-name afm)))
	  (write-16 (afm-cache/string-table-index st (afm/family-name afm)))
	  (write-16 (afm-cache/string-table-index st (afm/weight afm)))
	  (write-16 (afm-cache/string-table-index st (afm/character-set afm)))
	  (write-16
	    (afm-cache/string-table-index st (afm/encoding-scheme afm)))
	  (write-16 (afm/ascender afm) #t)
	  (write-16 (afm/descender afm) #t)
	  (write-16 (afm/cap-height afm) #t)
	  (write-16 (afm/x-height afm) #t)
	  (let ((fr (afm/font-bbox afm)))
	    (write-16 (point/x (rect/ll fr)) #t)
	    (write-16 (point/y (rect/ll fr)) #t)
	    (write-16 (point/x (rect/ur fr)) #t)
	    (write-16 (point/y (rect/ur fr)) #t))
	  (write-16 (afm/underline-position afm) #t)
	  (write-16 (afm/underline-thickness afm) #t)
	  (write-16.16 (afm/italic-angle afm))
	  (write-16 (if (afm/fixed-pitch? afm) 1 0))
	  (write-16 (if (afm/screen-font? afm) 1 0))
	  (write-16 (afm/screen-font-size afm))
	  (write-string
	    (make-string (- es (- (output-port/position p) pp)) #\Nul))
	  (dump-entry (cdr l)))))))

(define (afm-cache/string-table-index string-table str)
  (if (null? str)
      (set! str ""))
  (let ((index (hash-table/get string-table str '())))
    (if (not (null? index))
	index
	(let ((index (hash-table/count string-table)))
	  (hash-table/put! string-table str index)
	  index))))
	
(define (afm-cache/dump-string-table string-table)
  (letrec ((concat-length
	    (lambda (l)
	      (if (null? l)
		  0
		  (+ (string-length (caar l)) 1 (concat-length (cdr l)))))))
    (let* ((sl (sort
		 (hash-table/entries-list string-table)
		 (lambda (e1 e2)
		   (< (cdr e1) (cdr e2)))))
	   (sc (length sl))
	   (ov (make-vector sc))
	   (cs (make-string (concat-length sl))))
      (letrec ((load-strings!
		(lambda (l k so)
		  (if (< k sc)
		      (let* ((ns (string-append (caar l) (string #\Nul)))
			     (nsl (string-length ns)))
			(vector-set! ov k (+ so (* 2 (1+ sc))))
			(substring-move-right! ns 0 nsl cs so)
			(load-strings! (cdr l) (1+ k) (+ so nsl)))))))
	(load-strings! sl 0 0)
	(write-16 sc)
	(for-each
	  (lambda (so)
	    (write-16 so))		; String Offset Array Element
	  (vector->list ov))
	(write-string cs)		; String Buffer
	unspecific))))

fm;
; Dump limited per-glyph information for time being: W0X, W1X, and BBOX.
;

(define (afm-cache/dump-glyph-table afm)
  (let* ((gv (afm/glyphs afm))
	 (n (vector-length gv)))
    (letrec ((dump-empty-glyph
	      (lambda ()
		(write-32 #x7fffffff)			; W0
		(write-32 #x7fffffff)			; W1X
		(write-32 #x7fffffff)			; BBOX.LL
		(write-32 #x7fffffff)))			; BBOX.UR
	     (dump-glyph
	      (lambda (g)
		(let ((w0 (vector-ref (afm-glyph/widths g) 0))
		      (w1 (vector-ref (afm-glyph/widths g) 1))
		      (bb (afm-glyph/bbox g)))
		  (if (null? w0)
		      (write-32 #x7fffffff)		; W0
		      (begin
			(write-16 (point/x w0) #t)
			(write-16 (point/y w0) #t)))
		  (if (null? w1)
		      (write-32 #x7fffffff)		; W1
		      (begin
			(write-16 (point/x w1) #t)
			(write-16 (point/y w1) #t)))
		  (if (null? bb)
		      (begin
			(write-32 #x7fffffff)
			(write-32 #x7fffffff))
		      (let ((ll (rect/ll bb))
			    (ur (rect/ur bb)))
			(write-16 (point/x ll) #t)	; BBOX
			(write-16 (point/y ll) #t)
			(write-16 (point/x ur) #t)
			(write-16 (point/y ur) #t))))))

	     (dump-glyphs
	      (lambda (k)
		(if (< k n)
		    (begin
		      (let ((g (vector-ref gv k)))
			(if (null? g)
			    (dump-empty-glyph)
			    (dump-glyph g)))
		      (dump-glyphs (1+ k)))))))
      (write-16 8)				;trimmed array lookup table
      (write-16 (or (afm/first-glyph afm) 0))
      (write-16 n)
      (if (not (zero? n))
	  (dump-glyphs 0)))))

(define (afm-cache/dump-glyph-tables afm-table)
  (let* ((tc (hash-table/count afm-table))
	 (ov (make-vector tc)))
    (letrec ((dump-tables
	      (lambda (l k)
		(if (< k tc)
		    (let ((op (current-output-port)))
		      (vector-set! ov k (+ (output-port/position op) (* 4 tc)))
		      (afm-cache/dump-glyph-table (cdar l))
		      (dump-tables (cdr l) (1+ k))))))
	     (dump-offsets
	      (lambda (k)
		(if (< k tc)
		    (begin
		      (write-32 (vector-ref ov k))
		      (dump-offsets (1+ k)))))))
      (let ((tmpnam (format #f "/tmp/afm~A" (random 10000))))
	(dynamic-wind
	  (lambda () unspecific)
	  (lambda ()
	    (with-output-to-file tmpnam
	      (lambda ()
		(dump-tables (hash-table/entries-list afm-table) 0)))
	    (dump-offsets 0)
	    (write-string
	     (with-input-from-file tmpnam
	       (lambda ()
		 (read-string (char-set))))))
	  (lambda ()
	    (delete-file tmpnam)))))))

(define (afm-cache/dump afm-table)
  (let ((string-table (make-string-hash-table))
	(entry-buffer)
	(string-table-buffer))
    (set! entry-buffer
	  (with-output-to-string
	    (lambda ()
	      (afm-cache/dump-entries
	        afm-table
		string-table
		(afm-cache/max-entry-size afm-table)))))
    (set! string-table-buffer
	  (with-output-to-string
	    (lambda ()
	      (afm-cache/dump-string-table string-table))))
    (let* ((eblen (string-length entry-buffer))
	   (ebpad (- (round-up eblen 4) eblen))
	   (sblen (string-length string-table-buffer))
	   (sbpad (- (round-up sblen 4) sblen)))
      (afm-cache/dump-header afm-table *afm-cache-version*
        (+ *afm-header-size* eblen ebpad sblen sbpad))
      (write-string entry-buffer)
      (if (not (zero? ebpad))
	  (write-string (make-string ebpad #\Nul)))
      (write-string string-table-buffer)
      (if (not (zero? sbpad))
	  (write-string (make-string sbpad #\Nul)))
      (afm-cache/dump-glyph-tables afm-table)
      unspecific)))

(define (afm-cache/dump-to-file afm-table file-name)
  (with-output-to-file file-name
    (lambda ()
      (afm-cache/dump afm-table))))

(define (afm-cache/build #!optional build-noisily?)
  (if (default-object? build-noisily?)
      (set! build-noisily? #f))
  (let ((file-name
	 (canonicalize-output-filename *default-afm-cache-file-name*)))
    (let ((afm-table (make-string-hash-table)))
      (for-each
       (lambda (afm-directory)
	 (let ((afm-files (directory-read afm-directory)))
	   (for-each
	    (lambda (afm-file)
	      (let ((fn (pathname->string afm-file)))
		(if (not (string-prefix? "Screen" (pathname-name afm-file)))
		    (begin
		      (if build-noisily?
			  (format #t "~%Parsing ~S -- " fn))
		      (let ((afm (afm/load fn)))
			(hash-table/put! afm-table (afm/font-name afm) afm))
		      (if build-noisily?
			  (format #t "Done."))))))
	    afm-files)))
       *default-afm-directories*)
      (if build-noisily?
	  (format #t "~%Dumping ~S -- " file-name))
      (afm-cache/dump-to-file afm-table file-name)
      (if build-noisily?
	  (format #t "Done."))
      unspecific)))
