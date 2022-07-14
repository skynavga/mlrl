
(declare (usual-integrations))

(define-record rtf
  (version encoding fonts fontsets style-catalog styles paragraphs))

(define *rtf-version*	1)

(define error-type:eof
  (make-error-type '() "Unexpected end of file"))

(define error-type:bad-hex-character
  (make-error-type '()
    (lambda (c p)
      p ; ignore
      (format #t "Invalid hexidecimal character encoding: ~S"
	      (car (condition/irritants c))))))

(define error-type:rtf-syntax
  (make-error-type '()
    (lambda (c p)
      p ; ignore
      (let ((il (condition/irritants c)))
	(format #t "Bad RTF Syntax: Expected ~S, Got ~S" (car il) (cadr il))))))

(define error-type:rtf-reference
  (make-error-type '()
    (lambda (c p)
      p ; ignore
      (let ((il (condition/irritants c)))
	(format #t "Bad RTF ~A Reference: ~A" (car il) (cadr il))))))

(define-macro (signal type . irritants)
  (let ((cont (generate-uninterned-symbol)))
    `(CALL-WITH-CURRENT-CONTINUATION
       (LAMBDA (,cont)
	 (SIGNAL-ERROR (MAKE-CONDITION ,type (LIST ,@irritants) ,cont))))))

(define (twip->pt twips)
  (let ((pts (/ twips 20)))
    (if (and (not (integer? pts))
	     (exact? pts))
	(exact->inexact pts)
	pts)))

(define (nibbles->char h1 h2)
  (let ((i1 (char->digit h1 16))
	(i2 (char->digit h2 16)))
    (cond
     ((not i1) (signal error-type:bad-hex-character h1))
     ((not i2) (signal error-type:bad-hex-character h2))
     (else
      (integer->char (+ (* i1 16) i2))))))

(define (rtf/read-control-parameter)
  (let ((nc (peek-char))
	(negative? #f))
    (if (eq? nc #\-)
	(begin
	  (read-char)
	  (set! negative? #t)))
    (let get-parameter ((nc (peek-char)) (p '()))
      (if (not (char-numeric? nc))
	  (if negative? (- p) p)
	  (begin
	    (read-char)
	    (if (null? p) (set! p 0))
	    (get-parameter (peek-char) (+ (* 10 p) (char->digit nc))))))))

(define (rtf/read-control-token)
  (let ((c (read-char)))
    (if (eof-object? c)
	(signal 'eof)
	(if (char-alphabetic? c)
	    (let* ((control
		     (string->symbol
		       (let get-control ((nc (peek-char)) (cl (list c)))
			 (if (not (char-alphabetic? nc))
			     (apply string (reverse cl))
			     (begin
			       (read-char)
			       (get-control (peek-char) (cons nc cl)))))))
		   (parameter (rtf/read-control-parameter)))
	      (if (null? parameter)
		  (list control)
		  (list control parameter)))
	    (case c
	      ((#\')
	       (let ((h1 (read-char)))
		 (if (eof-object? h1)
		     (signal 'eof)
		     (let ((h2 (read-char)))
		       (if (eof-object? h2)
			   (signal 'eof)
			   (list 'text (nibbles->char h1 h2)))))))
	      ((#\Return))
	      ((#\{ #\} #\\)
	       (list 'text c))
	      ((#\Linefeed)
	       (list 'text #x2028))
	      (else
	       (let* ((control (string->symbol (string c)))
		      (parameter (rtf/read-control-parameter)))
		 (if (null? parameter)
		     (list control)
		     (list control parameter)))))))))

(define rtf/read-token)

(define rtf/unread-token)

(let ((last-token '()))
  (set! rtf/read-token
	(lambda ()
	  (if (not (null? last-token))
	      (let ((t last-token))
		(set! last-token '())
		t)
	      (let ((c (read-char)))
		(if (eof-object? c)
		    (list 'eof)
		    (case c
		      ((#\{) (list 'begin))
		      ((#\}) (list 'end))
		      ((#\\) (rtf/read-control-token))
		      ((#\Tab) (list 'tab))
		      ((#\Linefeed #\Return) (rtf/read-token))
		      (else (list 'text c))))))))
  (set! rtf/unread-token
	(lambda (t)
	  (if (not (null? last-token))
	      (error "Would overwrite unread token:" last-token)
	      (set! last-token t))
	  unspecific)))

(define (rtf/want-token control-or-list-of-controls)
  (let ((t (rtf/read-token)))
    (or (and (symbol? control-or-list-of-controls)
	     (eq? (car t) control-or-list-of-controls)
	     t)
	(and (list? control-or-list-of-controls)
	     (memq (car t) control-or-list-of-controls)
	     t)
	(signal error-type:rtf-syntax control-or-list-of-controls (car t)))))

(define (rtf/read-group-rest receiver)
  (let loop ((t (rtf/read-token)))
    (case (car t)
      ((eof)
       (signal error-type:eof))
      ((end)
       (receiver t))
      (else
       (receiver t)
       (loop (rtf/read-token))))))

(define (rtf/skip-group)
  (let loop ((t (rtf/read-token)) (level 0))
    (case (car t)
      ((eof)
       (signal error-type: eof))
      ((begin)
       (loop (rtf/read-token) (1+ level)))
      ((end)
       (if (not (zero? level))
	   (loop (rtf/read-token) (-1+ level))))
      (else
       (loop (rtf/read-token) level)))))

(define (rtf/read-group receiver #!optional discard?)
  (if (default-object? discard?)
      (set! discard? #f))
  (let loop ((t (rtf/read-token)))
    (if (eq? (car t) 'begin)
	(rtf/read-group-rest receiver)
	(if (not discard?)
	    (signal error-type:rtf-syntax 'begin (car t))
	    (loop (rtf/read-token))))))

(define (string-grow old-string new-length fill-char)
  (let ((ns (make-string new-length fill-char)))
    (substring-move-right! old-string 0 (string-length old-string) ns 0)
    ns))

(define (rtf/read-text first-char)
  (let ((s (make-string 16 #\Nul)))
    (vector-8b-set! s 0 (char->integer first-char))
    (let loop ((k 1))
      (let ((n (string-length s)))
	(if (= k n)
	    (set! s (string-grow s (* 2 n) #\Nul)))
	(let ((t (rtf/read-token)))
	  (if (not (eq? (car t) 'text))
	      (begin
		(rtf/unread-token t)
		(substring s 0 k))
	      (let ((c (char->integer (cadr t))))
		(if (not (and (= c #x20)
			      (= (vector-8b-ref s (- k 1)) #x20)))
		    (begin
		      (vector-8b-set! s k c)
		      (loop (1+ k)))
		    (loop k)))))))))

(define (rtf/find-font rtf fno)
  (let ((fv (rtf/fonts rtf)))
    (if (or (null? fv)
	    (>= fno (vector-length fv)))
	(signal error-type:rtf-reference "Font" fno)
	(vector-ref fv fno))))

(define (rtf/read-font-table rtf)
  (define font-families '(fnil froman fswiss fmodern fscript fdecor ftech))
  (define tcs (char-set-invert (char-set #\Space #\;)))
  (define (read-font fno)
    (let* ((ff (car (rtf/want-token font-families)))
	   (fn (string-trim (rtf/read-text (cadr (rtf/want-token 'text))) tcs)))
      (list fno ff fn)))
  (let ((max-font-number 0))
    (define (add-font f)
      (set! max-font-number (max max-font-number (car f)))
      (if (null? (rtf/fonts rtf))
	  (rtf/set-fonts! rtf (list f))
	  (append! (rtf/fonts rtf) (list f))))
    (define (validate-fonts)
      (let ((fv (make-vector (1+ max-font-number))))
	(let loop ((fl (rtf/fonts rtf)))
	  (if (not (null? fl))
	      (let ((f (car fl)))
		(vector-set! fv (car f) (caddr f))
		(loop (cdr fl)))))
	(rtf/set-fonts! rtf fv)))
    (let loop ((t (rtf/read-token)))
      (case (car t)
	((f)
	 (add-font (read-font (cadr t)))
	 (loop (rtf/read-token)))
	((end)
	 (validate-fonts))
	(else
	 (signal error-type:rtf-syntax '(f end) (car t)))))))

(define (rtf/find-fontset rtf fsno)
  (let ((fsv (rtf/fontsets rtf)))
    (if (or (null? fsv)
	    (>= fsno (vector-length fsv)))
	(signal error-type:rtf-reference "Font set" fsno)
	(vector-ref fsv fsno))))

(define (rtf/read-fontset-table rtf)
  (let ((max-fontset-number 0))
    (define tcs (char-set-invert (char-set #\Space #\;)))
    (define (read-fontset)
      (let ((fsno '()) (fsn "") (fl '()) (ff '()) (fst '()) (fsz '*))
	(define (add-face-style fs)
	  (set! fst
		(if (assq (car fs) fst)
		    (cons fs (del-assq (car fs) fst))
		    (cons fs fst))))
	(let next-token ((t (rtf/read-token)))
	  (if (eq? (car t) 'end)
	      (begin
		(if (number? fsno)
		    (begin
		      (set! max-fontset-number (max max-fontset-number fsno))
		      (list fsno (string-trim fsn tcs) fl))))
	      (begin
		(case (car t)
		  ((fst)
		   (set! fsno (cadr t)))
		  ((text)
		   (set! fsn (string-append fsn (rtf/read-text (cadr t)))))
		  ((f)
		   (if (not (null? ff))
		       (let ((fsc (font-spec/intern (list ff fst fsz))))
			 (if (null? fl)
			     (set! fl (list fsc))
			     (append! fl (list fsc)))))
		   (set! ff (rtf/find-font rtf (cadr t))))
		  ((b)
		   (add-face-style (cons 'weight 'bold)))
		  ((i)
		   (add-face-style (cons 'posture 'italic)))
		  ((fs)
		   (set! fsz (/ (cadr t) 2)))
		  ((bloff)			; baseline offsets
		   (add-face-style (cons 'baseline-offset (/ (cadr t) 10))))
		  ((xs)
		   (add-face-style
		    (cons 'horizontal-scale (/ (cadr t) 10))))
		  ((ys)
		   (add-face-style
		    (cons 'vertical-scale (/ (cadr t) 10)))))
		(next-token (rtf/read-token)))))))
    (define (add-fontset fs)
      (if (null? (rtf/fontsets rtf))
	  (rtf/set-fontsets! rtf (list fs))
	  (append! (rtf/fontsets rtf) (list fs))))
    (define (validate-fontsets)
      (let ((fsv (make-vector (1+ max-fontset-number))))
	(let next-fontset ((fsl (rtf/fontsets rtf)))
	  (if (null? fsl)
	      (rtf/set-fontsets! rtf fsv)
	      (let ((fs (car fsl)))
		(vector-set! fsv (car fs) (cdr fs))
		(next-fontset (cdr fsl)))))))
    (let next-fontset ((t (rtf/read-token)))
      (case (car t)
	((begin)
	 (add-fontset (read-fontset))
	 (next-fontset (rtf/read-token)))
	((end)
	 (validate-fontsets))
	(else
	 (signal error-type:rtf-syntax '(begin end) (car t)))))))

(define *rtf-language-identifiers*
  '((#x041c . albanian)
    (#x0401 . arabic)
    (#x0421 . bahasa)
    (#x0813 . dutch)		; belgian
    (#x080c . french)		; belgian
    (#x0416 . portuguese)	; brazilian
    (#x0402 . bulgarian)
    (#x0403 . catalan)
    (#x041a . serbo-croatian)	; roman script
    (#x0405 . czech)
    (#x0406 . danish)
    (#x0c09 . english)		; australian
    (#x0809 . english)		; british
    (#x0409 . english)		; american
    (#x040b . finnish)
    (#x040c . french)
    (#x0c0c . french)		; canadian
    (#x0407 . german)
    (#x0408 . greek)
    (#x040d . hebrew)
    (#x040e . hungarian)
    (#x040f . icelandic)
    (#x0410 . italian)
    (#x0411 . japanese)
    (#x0412 . korean)
    (#x0414 . norwegian)	; bokmal
    (#x0814 . norwegian)	; nynorsk
    (#x0415 . polish)
    (#x0816 . portuguese)
    (#x0417 . rhaeto-romance)
    (#x0418 . romanian)
    (#x0419 . russian)
    (#x081a . serbo-croatian)	; cyrillic script
    (#x0804 . chinese)		; simplified script
    (#x041b . slovak)
    (#x040a . spanish)		; castilian
    (#x080a . spanish)		; mexican
    (#x041d . swedish)
    (#x100c . french)		; swiss
    (#x0807 . german)		; swiss
    (#x0810 . italian)		; swiss
    (#x041e . thai)
    (#x0404 . chinese)		; traditional script
    (#x041f . turkish)
    (#x0420 . urdu)))
  
(define *rtf-language-id-table*
  (let ((lt (make-hbb)))
    (let loop ((l *rtf-language-identifiers*))
      (if (null? l)
	  lt
	  (let ((lid (car l)))
	    (hbb/put! lt (car lid) (cdr lid))
	    (loop (cdr l)))))))

(define (rtf-language-id->language id)
  (hbb/get *rtf-language-id-table* id))

(define (rtf/make-style-catalog-style rtf name based-on next properties)
  (let ((bs '()) (ns '()))
    (if (number? based-on)
	(begin
	  (set! bs (rtf/find-style rtf based-on))
	  (if (null? bs)
	      (signal error-type:rtf-reference "Style" based-on))))
    (if (number? next)
	(begin
	  (set! ns (rtf/find-style rtf next))
	  (if (null? ns)
	      (signal error-type:rtf-reference "Style" next))))
    (let ((sc (rtf/style-catalog rtf)))
      (if (null? sc)
	  (begin
	    (set! sc (make-style-catalog '()))
	    (rtf/set-style-catalog! rtf sc)))
      (let ((s (style-catalog/new sc name bs)) (ff '()) (fst '()) (fsz '*))
	(define (add-face-style fs)
	  (set! fst
		(if (assq (car fs) fst)
		    (cons fs (del-assq (car fs) fst))
		    (cons fs fst))))
	(let next-prop ((pl properties))
	  (if (null? pl)
	      (begin
		(if (not (null? ff))
		  (style/set-font! s (font-spec/intern (list ff fst fsz))))
		s)
	      (let ((p (car pl)))
		(case (car p)
		  ((lang)
		   (let ((lang (rtf-language-id->language (cadr p))))
		     (if (not (null? lang))
			 (style/put! s 'language lang))))
		  ((f fst)
		   (set! ff (cadr p)))
		  ((fs)
		   (set! fsz (/ (cadr p) 2)))
		  ((plain)
		   (add-face-style '(weight . medium))
		   (add-face-style '(posture . upright))
		   (add-face-style '(width . medium)))
		  ((b)
		   (add-face-style '(weight . bold)))
		  ((i)
		   (add-face-style '(posture . italic)))
		  ((rtl)
		   (style/put! s 'reverse-flow? #t))
		  ((rtl)
		   (style/put! s 'reverse-flow? #f))
		  ((fi)
		   (let ((lc (if (null? (cdr p)) 1 (cadr p))))
		     (style/put! s 'indent-first-lines lc)
		     (style/put! s 'first-indentation (twip->pt (cadr p)))))
		  ((li)
		   (let ((rf (style/get s 'reverse-flow? #f)))
		     (style/put! s
		       (if rf 'end-indentation 'start-indentation)
		       (twip->pt (cadr p)))))
		  ((ri)
		   (let ((rf (style/get s 'reverse-flow? #f)))
		     (style/put! s
		       (if rf 'start-indentation 'end-indentation)
		       (twip->pt (cadr p)))))
		  ((sb)
		   (style/put! s 'spread-before (twip->pt (cadr p))))
		  ((sa)
		   (style/put! s 'spread-after (twip->pt (cadr p))))
		  ((sl)
		   (style/put! s 'leading (twip->pt (cadr p))))
		  ((ql qr qc qj)
		   (style/put! s 'justification
			       (case (car p)
				 ((ql) 'left)
				 ((qr) 'right)
				 ((qc) 'center)
				 ((qj) 'full)))))
		(next-prop (cdr pl)))))))))
  
(define (rtf/find-style rtf sno)
  (let ((sv (rtf/styles rtf)))
    (if (or (null? sv)
	    (>= sno (vector-length sv)))
	(signal error-type:rtf-reference "Style" sno)
	(vector-ref sv sno))))

(define (rtf/read-style-table rtf)
  (let ((max-style-number 0))
    (define tcs (char-set-invert (char-set #\Space #\;)))
    (define (read-style)
      (let ((sno '()) (sb '()) (snx '()) (sn "") (sp '()))
	(define (add-style-prop t)
	  (if (null? sp)
	      (set! sp (list t))
	      (append! sp (list t))))
	(let next-token ((t (rtf/read-token)))
	  (if (eq? (car t) 'end)
	      (begin
		(if (number? sno)
		    (begin
		      (set! max-style-number (max max-style-number sno))
		      (list sno (string-trim sn tcs) sb snx sp))))
	      (begin
		(case (car t)
		  ((s)
		   (set! sno (cadr t)))
		  ((sbasedon)
		   (set! sb (cadr t)))
		  ((snext)
		   (set! snx (cadr t)))
		  ((text)
		   (set! sn (string-append sn (rtf/read-text (cadr t)))))
		  ((f)
		   (add-style-prop
		     (list (car t) (rtf/find-font rtf (cadr t)))))
		  ((fst)
		   (add-style-prop
		     (list (car t) (rtf/find-fontset rtf (cadr t)))))
		  (else
		   (add-style-prop t)))
		(next-token (rtf/read-token)))))))
    (define (add-style s)
      (if (null? (rtf/styles rtf))
	  (rtf/set-styles! rtf (list s))
	  (append! (rtf/styles rtf) (list s))))
    (define (validate-styles)
      (let ((sv (make-vector (1+ max-style-number)))
	    (styles (rtf/styles rtf)))
	(rtf/set-styles! rtf sv)
	(let next-style ((sl styles))
	  (if (not (null? sl))
	      (let ((s (car sl)))
		(vector-set! sv (car s)
                  (apply rtf/make-style-catalog-style
			 (append (list rtf) (cdr s))))
		(next-style (cdr sl)))))))
    (let next-style ((t (rtf/read-token)))
      (case (car t)
	((begin)
	 (add-style (read-style))
	 (next-style (rtf/read-token)))
	((end)
	 (validate-styles))
	(else
	 (signal error-type:rtf-syntax '(begin end) (car t)))))))

(define rtf/accept-top-level)

(define rtf/read)

(let ((text '())
      (current-style '())
      (current-writing-system '()))
  (set! rtf/accept-top-level
    (lambda (rtf t)
      (define (guarantee-text)
	(if (null? text)
	    (begin
	      (set! text (make-text))
	      (if (not (null? current-style))
		  (text/set-default-style! text current-style))
	      (if (not (null? current-writing-system))
		  (text/set-default-writing-system! text
		    current-writing-system)))))
      (case (car t)
	((begin)
	 (let ((nt (rtf/read-token)))
	   (case (car nt)
	     ((fonttbl)
	      (rtf/read-font-table rtf))
	     ((fontsettbl)
	      (rtf/read-fontset-table rtf))
	     ((stylesheet)
	      (rtf/read-style-table rtf))
	     ((end))
	     ((text))
	     (else
	      (rtf/skip-group)))))
	((text)
	 (guarantee-text)
	 (text/append! text (rtf/read-text (cadr t))))
	((par)	; end of paragraph
	 (if (not (null? text))
	     (begin
	       (if (null? (rtf/paragraphs rtf))
		   (rtf/set-paragraphs! rtf (list text))
		   (append! (rtf/paragraphs rtf) (list text)))
	       (set! text '()))))
	((pard))				; default paragraph style
	((s)					; style setting
	 (guarantee-text)
	 (let* ((s (rtf/find-style rtf (cadr t)))
		(ws (style/get s 'language '())))
	   (text/set-default-style! text s)
	   (set! current-style s)
	   (text/set-default-writing-system! text ws)
	   (set! current-writing-system ws)))
	((f fst fs)				; font setting
	 (guarantee-text))
	((ql qr qj qc))				; justification
	((fi li ri))				; indentations
	((sb sa sl))				; para/line leading
	((plain b i expnd))			; basic typeface styles
	((outl shad))				; special typeface styles
	((scaps caps))				; type transforms
	((up dn))				; subs/sups
	((strike ul ulw uld uldb ulnone)) 	; striking/underlining
	((v))					; hidden text
	((- ~ _ tab)))))			; specials
  (set! rtf/read
    (lambda ()
      (define encodings '(ansi mac pc pca utf))
      (let ((rtf (make-rtf '() '() '() '() '() '() '())))
	(rtf/want-token 'begin)
	(let ((version (cadr (rtf/want-token 'rtf))))
	  (if (not (= version *rtf-version*))
	      (error "Version mismatch, expected ~A, got ~A"
		     *rtf-version* version))
	  (rtf/set-version! rtf version))
	(rtf/set-encoding! rtf (car (rtf/want-token encodings)))
	(rtf/read-group-rest
	  (lambda (t)
	    (rtf/accept-top-level rtf t)))
	rtf))))

(define (rtf/read-from-file file)
  (with-input-from-file file
    (lambda ()
      (rtf/read))))

(define (rtf/extract-body-stream rtf)
  (let ((cs (make-content-stream)))
    (let loop ((pl (rtf/paragraphs rtf)))
      (if (null? pl)
	  cs
	  (begin
	    (content-stream/insert! cs (make-content-unit (car pl)))
	    (loop (cdr pl)))))
    cs))

#|
(define (rtf/tokenize file receiver)
  (with-input-from-file file
    (lambda ()
      (let loop ((t (rtf/read-token)))
	(if (not (eq? (car t) 'eof))
	    (begin
	      (receiver t)
	      (loop (rtf/read-token))))))))
|#

;
; RTF Documents
;

(define *default-document-content-type* 'body)

(define-record rtf-document (rtf streams))

(define (document/set-content-stream! rtf-doc type cs)
  (let ((sl (rtf-document/streams rtf-doc))
	(s (cons type cs)))
    (rtf-document/set-streams! rtf-doc
      (if (null? sl)
	  (list s)
          (cons s (del-assq type sl))))))

(define (document/content-stream rtf-doc type)
  (let ((s (assq type (rtf-document/streams rtf-doc))))
    (and s (cdr s))))

(define (document/content-exhausted? rtf-doc #!optional type)
  (if (default-object? type)
      (set! type *default-document-content-type*))
  (let ((cs (document/content-stream rtf-doc type)))
    (or (null? cs) (content-stream/end-of-stream? cs))))

(define (document/open filename)
  (let ((rtf (rtf/read-from-file filename)))
    (let ((rtf-doc (make-rtf-document rtf '())))
      (document/set-content-stream! rtf-doc *default-document-content-type*
        (rtf/extract-body-stream rtf))
      rtf-doc)))
