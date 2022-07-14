
;
; Minimal TeX Reader
;

(declare (usual-integrations))

(define *scodes*)
(define *max-scode*	128)
(define *default-sf*    1000)
(define *large-sf*    	1000)

(define *non-french-sfcodes*
  '(( #\) . 0    )
    ( #\' . 0    )
    ( #\] . 0    )
    ( #\. . 3000 )
    ( #\? . 3000 )
    ( #\! . 3000 )
    ( #\: . 2000 )
    ( #\; . 1500 )
    ( #\, . 1250 )))

(define init-sfcodes
  (let ((ucstart (char->integer #\A))
	(ucend (char->integer #\Z))
	(ucsf (-1+ *default-sf*)))
    (lambda (specials)
      (set! *scodes* (make-vector *max-scode* *default-sf*))
      (let ucloop ((k ucstart))
	(if (< k ucend)
	    (begin
	      (vector-set! *scodes* k ucsf)
	      (ucloop (1+ k)))))
      (let specloop ((sl specials))
	(if (not (null? sl))
	    (let ((s (car sl)))
	      (vector-set! *scodes* (char->integer (car s)) (cdr s))
	      (specloop (cdr sl))))))))

(init-sfcodes *non-french-sfcodes*)

(define (char->sf ch)
  (let ((code (char->integer ch)))
    (if (>= code *max-scode*)
	*default-sf*
	(vector-ref *scodes* code))))

(define *tex-default-font*
  '("Times" ((weight . medium) (posture . upright) (width . medium)) 10.5))

(define *tex-font-catalog*
  (device/font-catalog (make-device 'postscript '() '())))

(define (tex/default-font-instance)
  (font-catalog/ascii-font-instance *tex-font-catalog*
    (font-spec/intern *tex-default-font*)))

(define (tex/read)
  (let ((tgs (make-glyph-stream 'tex))
	(sf *default-sf*)
	(fi (tex/default-font-instance))
	(pos 0)
	(gm (make-glyph-metrics))
	(bcs (char-set-invert (char-set #\\ #\Space #\Tab #\Linefeed))))
    (define (next-char)
      (set! pos (1+ pos))
      (read-char))
    (define (box-character? ch)
      (and (not (eof-object? ch))
	   (char-set-member? bcs ch)))
    (define (measure chars fi)
      (if (null? chars)
	  0
	  (let ((dm 0))
	    (if (char? chars)
		(set! chars (list chars)))
	    (let loop ((chl chars))
	      (if (null? chl)
		  dm
		  (let ((code (char->integer (car chl))))
		    (font-instance/glyph-metrics fi code gm)
		    (set! dm (+ dm (glyph-metrics/escapement gm)))
		    (loop (cdr chl))))))))
    (define (make-box positions chars)
      (make-gi 'box (list positions chars (measure chars fi))))
    (define (make-kern ch1 ch2)
      ch1 ch2 ;ignore
      (make-gi 'kern (list pos 0)))
    (define (make-disc pre post repl)
      (let ((prem (measure pre fi))
	    (postm (measure post fi))
	    (replm (measure repl fi)))
	(make-gi 'disc (list pos repl replm pre prem post postm))))
    (define (make-glue sf fi)
      (let* ((nominal (measure '(#\Space) fi))
	     (stretch (/ nominal 2.0))
	     (shrink (/ nominal 3.0))
	     (extra shrink))
	(if (>= sf *large-sf*)
	    (set! nominal (+ nominal extra)))
	(if (not (= sf *default-sf*))
	    (begin
	      (set! stretch (* stretch (/ sf 1000.0)))
	      (set! shrink (* shrink (/ 1000.0 sf)))))
	(make-gi 'glue
	   (list pos
		 (find-glue-spec nominal stretch 'normal shrink 'normal)))))
    (define (control-word cw) cw '())
    (define (control-symbol cs)
      (case cs
	((#\\)
	 (let ((nsf (char->sf ch)))
	   (if (not (zero? nsf))
	       (set! sf nsf)))
	 (glyph-stream/write tgs (make-box pos #\\)))
	((#\/)
	 (glyph-stream/write tgs (make-kern '() '())))
	((#\-)
	 (glyph-stream/write tgs (make-disc #\- '() '())))
	((#\Space)
	 (glyph-stream/write tgs (make-glue sf fi))
	 (set! sf *default-sf*))))
    (let loop ((ch (read-char)))
      (if (eof-object? ch)
	  tgs
	  (begin
	    (case ch
	      ((#\\)
	       (let ((cchl '()))
		 (if (and (not (eof-object? (peek-char)))
			  (char-alphabetic? (peek-char)))
		     (let read-control-word ((cch (next-char)))
		       (set! cchl (cons cch cchl))
		       (let ((nch (peek-char)))
			 (if (and (not (eof-object? nch))
				  (not (char-alphabetic? nch)))
			     (begin
			       (if (char-whitespace? nch)
				   (next-char))
			       (control-word
				 (string->symbol (string (reverse cchl)))))
			     (read-control-word (next-char)))))
		     (control-symbol (next-char)))))
	      ((#\Space #\Tab #\Linefeed)
	       (glyph-stream/write tgs (make-glue sf fi))
	       (set! sf *default-sf*)
	       (let skip ((sch (peek-char)))
		 (if (and (not (eof-object? sch))
			  (char-whitespace? sch))
		     (begin
		       (next-char)
		       (skip (peek-char))))))
	      (else
	       (let box-loop ((bch ch) (bchl '()) (posl '()))
		 (set! bchl (cons bch bchl))
		 (set! posl (cons pos posl))
		 (let ((nsf (char->sf bch)))
		   (if (not (zero? nsf))
		       (set! sf nsf)))
		 (if (eq? bch #\-)
		     (begin
		       (glyph-stream/write tgs
		         (make-box (reverse posl) (reverse bchl)))
		       (glyph-stream/write tgs
			 (make-disc '() '() '())))
		     (if (not (box-character? (peek-char)))
			 (glyph-stream/write tgs
			  (make-box (reverse posl) (reverse bchl)))
			 (box-loop (next-char) bchl posl))))))
	    (loop (next-char)))))))

(define (tex/read-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (tex/read))))
