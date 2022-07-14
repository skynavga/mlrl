
;
; Rendering System
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(declare (usual-integrations))

(define (decompose-code-elt e i lgs)
  (letrec ((loop
	    (lambda (dl)
	      (if (not (null? dl))
		  (let ((de (car dl)))
		    (glyph-stream/write lgs
		      (make-gi (if (nsm? de) 'attach 'base)
			       (list i (car dl))))
		    (loop (cdr dl)))))))
    (cond
     ((fix:= e #x0020)
      (glyph-stream/write lgs (make-gi 'space (list i))))
     ((fix:= e #x00A0)
      (glyph-stream/write lgs (make-gi 'nbsp (list i))))
     ((fix:= e #x2028)
      (glyph-stream/write lgs (make-gi 'line-break (list i))))
     (else
      (loop (decomp/get e))))))

(define (decompose t start end)
  (let ((styles (or (text/styles t start end)
		    (list (cons 0 *default-style*))))
	(cv (text/content t))
	(lgs (make-glyph-stream 'logical)))
    (letrec ((loop
	      (lambda (k cs rs)
		(if (< k end)
		    (if (and (not (null? rs))
			     (>= k (caar rs)))
			(begin
			  (glyph-stream/write lgs
			    (make-gi 'style (list (cdar rs))))
			  (loop k (car rs) (cdr rs)))
			(begin
			  (decompose-code-elt (vector-ref cv k) k lgs)
			  (loop (1+ k) cs rs)))))))
      (loop start '() styles)
      lgs)))

(define (reorder gs)
  gs)

;
; Glyph Selection Processing
;
; Notes:
;
; 1. Reconsing a new glyph stream is bound to be a performance pig!
;
; 2. This flush may not be necessaary if we see 'end-of-stream LGI first.
;
; 3. Need to specify a reasonable default mapping; but, where should we
; get the default?
;
; 4. Once mapped, does the base/attach semantics still hold, or should
; they be re-evaluated based on the properties of the mapped glyphs?
;
; 5. Need a default font with default glyphs for each script that gets
; used when we can't map a UCS code.
;
; 6. Pass 'space and 'nbsp through context mapping if active.
;

(define (select igs fc)
  (let ((ngs (make-glyph-stream 'logical))	; new glyph stream
	(cgs (make-glyph-stream 'logical))	; context glyph stream
	(fs '())				; active font set
	(ufs '())				; active unified font spec
	(f '()))				; active font
    (call/cc
      (lambda (exit)
	(define (flush-context-stream)
	  (if (not (glyph-stream/empty? cgs))
	      (begin
		(font/context-map! f cgs)
		(glyph-stream/append! ngs cgs)
		(glyph-stream/flush! cgs))))
	(let loop ((lgi (glyph-stream/read igs)))			; N1
	  (if (glyph-stream/end-of-stream? igs)
	      (begin
		(flush-context-stream)					; N2
		(exit ngs))
	      (let ((rator (gi/rator lgi)))
		(case rator
		  ((style)
		   (let* ((s (car (gi/rands lgi)))
			  (sfs (style/font s)))
		     (flush-context-stream)
		     (set! fs
		       (font-catalog/lookup fc sfs #t))
		     (set! ufs
		       (font-spec/unification sfs (font-set/font-spec fs)))
		     (glyph-stream/write ngs (make-gi 'style (list s)))))
		  ((base attach)
		   (let* ((rands (gi/rands lgi))
			  (cc (cadr rands)))
		     (with-values
			 (lambda ()
			   (font-set/map fs cc '()))			; N3
		       (lambda (gc nf contextual?)
			 (if (not (null? gc))
			     (begin
			       (if (not (eq? nf f))
				   (let ((fsz (font-spec/size ufs))
					 (fst (font-spec/face ufs)))
				     (flush-context-stream)
				     (glyph-stream/write ngs
				       (make-gi 'font (list nf fsz fst)))
				     (set! f nf)))
			       (set-cdr! (gi/rands lgi) (list gc))
			       (if contextual?
				   (glyph-stream/write cgs lgi)
				   (glyph-stream/write ngs lgi)))	; N4
			     (warn "Can't map code:" cc))))))		; N5
		  ((end-of-stream)
		   (flush-context-stream)
		   (glyph-stream/write ngs lgi)
		   (exit ngs))
		  ((space nbsp)
		   (if (not (glyph-stream/empty? cgs))			; N6
		       (glyph-stream/write cgs lgi)		
		       (glyph-stream/write ngs lgi)))
		  (else
		   (flush-context-stream)
		   (glyph-stream/write ngs lgi)))
		(loop (glyph-stream/read igs)))))))))

(define (compose igs fc)
  fc
  igs)

(define (attach igs fc)
  fc
  igs)

(define (text/render t fc #!optional start end)
  (if (not (text? t))
      (error "Invalid text object:" t))
  (if (default-object? start)
      (set! start 0))
  (if (< start 0)
      (set! start 0))
  (let ((tlen (text/length t)))
    (if (default-object? end)
	(set! end tlen))
    (if (> end tlen)
	(set! end tlen)))
  (if (> start end)
      (set! end start))
  (attach
    (compose
      (select
	(reorder
	  (decompose t start end)) fc) fc) fc))
