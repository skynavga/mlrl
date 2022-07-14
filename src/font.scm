
;
; Font System
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;
; Notes:
;
; 1. face/canonicalize should canonicalize the order of style
; elements; otherwise, different external font specs which are
; equivalent except for order of face style elements will not
; hash to same value.
;
; 2. need to add numeric intervals as potential values of face
; style elements in order to support MM and TT font variations.
;

(declare (usual-integrations))

(define-record font-spec (family face size))

(define-record fontset-spec (name components))

(define-record font
  ( device
    device-font
    glyph-complement
    extended-metrics
    font-spec
    writing-system
  ))

(define-record font-instance (font size style))

(define-record font-set
  (fonts glyph-complement extended-metrics font-spec writing-systems))

(define-record font-catalog (font-sets writing-systems families))

(define *face-style-table* (make-symbol-hash-table))

(define *face-style-spec-table* (make-symbol-hash-table))

;
; Font Specifications
;

(define (parse-font-spec-size size)
  (if (number? size)
      size
      (if (eq? size '*)
	  0
	  (let ((value (car size))
		(units (cadr size)))
	    (case units
	      ((point points pt pts)
	       value)
	      ((micropoint micropoints upt upts)
	       (/ value 65536))
	      ((inch inches in)
	       (* 72.27 value))
	      ((centimeter centimeters cm)
	       (* 72.27 (/ value 2.54)))
	      ((millimeter millimeters mm)
	       (* 72.27 (/ value 25.4)))
	      ((micron microns)
	       (* 72.27 (/ value 25400)))
	      (else
	       (error "Unknown font size units:" units)))))))

(define (font-spec/hash-mod xfs k)
  (let ((family (car xfs)))
    (cond
     ((string? family)
      (string-hash-mod (string-upcase family) k))
     ((symbol? family)
      (string-hash-mod (string-upcase (symbol->string family)) k))
     ((fontset-spec? family)
      (string-hash-mod (string-upcase (fontset-spec/name family)) k))
     (else (error "Invalid family in font specification:" family)))))

(define (family/equal? f1 f2)
  (if (symbol? f1)
      (set! f1 (symbol->string f1)))
  (if (symbol? f2)
      (set! f2 (symbol->string f2)))
  (or (and (string? f1)
	   (string? f2)
	   (string-ci=? f1 f2))
      (and (fontset-spec? f1)
	   (fontset-spec? f2)
	   (eq? f1 f2))))

(define (family/fontset-spec? f)
  (and (list? f)
       (= (length f) 2)
       (string? (car f))
       (let loop ((cl (cadr f)))
	 (if (null? cl)
	     #t
	     (if (not (font-spec? (car cl)))
		 #f
		 (loop (cdr cl)))))))

(define (face-style/register-named-value key name value)
  (let ((st (hash-table/get *face-style-spec-table* key '())))
    (if (string? key)
	(set! key (string->symbol (string-downcase key))))
    (if (string? name)
	(set! name (string->symbol (string-downcase name))))
    (if (not (and (symbol? key)
		  (symbol? name)
		  (number? value)))
	(error "Invalid face style named value:" (list key name value)))
    (if (null? st)
	(begin
	  (set! st (make-symbol-hash-table))
	  (hash-table/put! *face-style-spec-table* key st)))
    (hash-table/put! st name value)))

(define (face-style/lookup-named-value key value)
  (let ((st (hash-table/get *face-style-spec-table* key '())))
    (and (not (null? st))
	 (hash-table/get st value '()))))

(define (face-style/register name face-style)
  (if (string? name)
      (set! name (string->symbol (string-downcase name))))
  (if (not (face/style-list? face-style))
      (error "Invalid face style:" (list name face-style)))
  (hash-table/put! *face-style-table* name face-style))

(define (face-style/lookup name)
  (hash-table/get *face-style-table* name '()))

(define (face-style-elt-pair? e)
  (let ((key (car e))
	(value (cdr e)))
    (if (string? key)
	(set! key (string->symbol (string-downcase key))))
    (if (string? value)
	(set! value (string->symbol (string-downcase value))))
    (and (symbol? key)
	 (or (number? value)
	     (and (symbol? value)
		  (not (null? (face-style/lookup-named-value key value))))))))

(define (face-style-elt? e)
  (or (symbol? e)
      (string? e)
      (and (pair? e)
	   (face-style-elt-pair? e))))

(define (face-style-elt/equal? e1 e2)
  (cond
   ((and (not (pair? e1)) (not (pair? e2)))
    (if (symbol? e1)
	(set! e1 (symbol->string e1)))
    (if (symbol? e2)
	(set! e2 (symbol->string e2)))
    (string-ci=? e1 e2))
   ((and (pair? e1)
	 (pair? e2)
	 (= (cdr e1) (cdr e2)))
    (face-style-elt/equal? (car e1) (car e2)))
   (else #f)))
      
(define (face/style-list? fsl)
  (call/cc
   (lambda (exit)
     (letrec ((loop
	       (lambda (l)
		 (if (null? l)
		     #t
		     (if (not (face-style-elt? (car l)))
			 (exit #f)
			 (loop (cdr l)))))))
       (and (list? fsl)
	    (loop fsl))))))

(define (face/style-equal? f1 f2)
  (call/cc
   (lambda (exit)
     (letrec ((sort-descriminator
	       (lambda (k1 k2)
		 (let ((s1 (if (pair? k1) (car k1) k1))
		       (s2 (if (pair? k2) (car k2) k2)))
		   (if (symbol? s1) (set! s1 (symbol->string s1)))
		   (if (symbol? s2) (set! s2 (symbol->string s2)))
		   (string-ci<? s1 s2))))
	      (same?
	       (lambda (f1 f2)
		 (if (null? f1)
		     (exit (null? f2))
		     (if (or (null? f2)
			     (not (face-style-elt/equal? (car f1) (car f2))))
			 (exit #f)
			 (same? (cdr f1) (cdr f2)))))))
       (same? (sort f1 sort-descriminator)
	      (sort f2 sort-descriminator))))))

(define (face/equal? f1 f2)
  (if (and (not (list? f1))
	   (not (list? f2)))
      (begin
	(if (symbol? f1)
	    (set! f1 (symbol->string f1)))
	(if (symbol? f2)
	    (set! f2 (symbol->string f2)))
	(string-ci=? f1 f2))
      (if (and (list? f1) (list? f2))
	  (face/style-equal? f1 f2)
	  (begin
	    (if (not (list? f1))
		(face/style-equal? (face-style/lookup f1) f2)
		(face/style-equal? (face-style/lookup f2) f1))))))

(define (face/style f key)
  (let ((s (assq key f)))
    (and s (cdr s))))

(define (face-style/unifies? fs1 fs2)
  (or (null? fs1)
      (eq? fs1 '*)
      (null? fs2)
      (eq? fs2 '*)
      (and (number? fs1)
	   (number? fs2)
	   (= fs1 fs2))))

;
; The following two functions, face/unify? and face/unification are
; not generic, given they make assumptions about the range of possible
; face styles.
;

(define (face/unify? f1 f2)
  (and (face-style/unifies? (face/style f1 'weight)
			    (face/style f2 'weight))
       (face-style/unifies? (face/style f1 'posture)
			    (face/style f2 'posture))
       (face-style/unifies? (face/style f1 'width)
			    (face/style f2 'width))))

(define (face/unification f1 f2)
  (let ((wt (or (face/style f1 'weight)
		(face/style f2 'weight)
		0.5))
	(ps (or (face/style f1 'posture)
		(face/style f2 'posture)
		0.0))
	(wd (or (face/style f1 'width)
		(face/style f2 'width)
		0.5)))
    (list (cons 'weight wt) (cons 'posture ps) (cons 'width wd))))

(define (face/canonicalize face)
  (cond
   ((string? face)
    (face/canonicalize (string->symbol (string-downcase face))))
   ((symbol? face)
    (or (face/canonicalize (face-style/lookup face)) face))
   ((list? face)
    (reverse
      (let loop ((sl face)
		 (nsl '()))
	(if (null? sl)
	    nsl
	    (let ((e (car sl)))
	      (if (or (string? e) (symbol? e))
		  (begin
		    (if (string? e)
			(set! e (string->symbol (string-downcase e))))
		    (let ((ne (face-style/lookup e)))
		      (if (null? ne)
			  (loop (cdr sl) (cons e nsl))
			  (loop (cdr sl)
				(append
				  (reverse (face/canonicalize ne)) nsl)))))
		  (begin
		    (if (pair? e)
			(let ((key (car e))
			      (value (cdr e)))
			  (if (string? key)
			      (set! key
				    (string->symbol (string-downcase key))))
			  (if (string? value)
			      (set! value
				    (string->symbol (string-downcase value))))
			  (cond
			   ((number? value)
			    (set! e (cons key value)))
			   ((symbol? value)
			    (let ((nv
				   (face-style/lookup-named-value key value)))
			      (if (not (null? nv))
				  (set! e (cons key nv))))))))
		    (loop (cdr sl) (cons e nsl)))))))))
   (else
     face)))

(define (font-spec/equal? xfs1 xfs2)
  (and (family/equal? (first xfs1) (first xfs2))
       (face/equal? (second xfs1) (second xfs2))
       (= (parse-font-spec-size (third xfs1))
	  (parse-font-spec-size (third xfs2)))))

(define make-font-spec-hash-table
  (hash-table/constructor
    font-spec/hash-mod font-spec/equal? cons #t car cdr set-cdr!))

(define *font-spec-hash-table* (make-font-spec-hash-table))

(define (font-spec/canonicalize xfs)
  (let ((family (car xfs)))
    (set! family
	  (cond
	   ((null? family) "")
	   ((symbol? family) (symbol->string family))
	   ((list? family) (fontset-spec/intern family))
	   (else family)))
    (list family
	  (face/canonicalize (cadr xfs))
	  (parse-font-spec-size (caddr xfs)))))

(define (font-spec/intern xfs)
  (if (or (not (list? xfs))
	  (not (= (length xfs) 3)))
      (error "Invalid font specification:" xfs)
      (let ((family (first xfs))
	    (face (second xfs))
	    (size (third xfs)))
	(if (and (not (null? family))
		 (not (symbol? family))
		 (not (string? family))
		 (not (family/fontset-spec? family)))
	    (error "Invalid family in font specification:" xfs))
	(if (and (not (symbol? face))
		 (not (string? face))
		 (not (and (list? face)
			   (face/style-list? face))))
	    (error "Invalid face in font specification:" xfs))
	(if (and (not (number? size))
		 (not (eq? size '*))
		 (not (and (pair? size)
			   (number? (car size))
			   (symbol? (cdr size)))))
	    (error "Invalid size in font specification:" xfs))
	(set! xfs (font-spec/canonicalize xfs))
	(call/cc
	 (lambda (exit)
	   (hash-table/lookup
	     *font-spec-hash-table*
	     xfs
	     (lambda (fs)
	       (exit fs))
	     (lambda ()
	       (let ((fs (make-font-spec (first xfs) (second xfs)(third xfs))))
		 (hash-table/put! *font-spec-hash-table* xfs fs)
		 (exit fs)))))))))

(define (font-spec/match fs1 list-of-font-specs)
  (call/cc
    (lambda (exit)
      (let match ((fsl list-of-font-specs))
	(if (null? fsl)
	    (exit '())
	    (let ((fs2 (car fsl)))
	      (if (and (string-ci=? (font-spec/family fs1)
				    (font-spec/family fs2))
		       (face/unify? (font-spec/face fs1)
				    (font-spec/face fs2))
		       (let ((s1 (font-spec/size fs1))
			     (s2 (font-spec/size fs2)))
			 (or (= s1 s2)
			     (zero? s1)
			     (zero? s2))))
		  (exit fs2)
		  (match (cdr fsl)))))))))

(define (font-spec/unification fs1 fs2)
  (let ((family (or (font-spec/family fs1)
		    (font-spec/family fs2)
		    ""))
	(face (face/unification (font-spec/face fs1) (font-spec/face fs2)))
	(size (let ((s1 (font-spec/size fs1))
		    (s2 (font-spec/size fs2)))
		(or (and (positive? s1) s1)
		    (and (positive? s2) s2)
		    0))))
    (font-spec/intern (list family face size))))

;
; Font Set Specs
;

(define (fontset-spec/hash-mod xfs k)
  (let ((name (car xfs)))
    (if (not (string? name))
	(error "Invalid name in fontset specification:" name)
	(string-hash-mod (string-upcase name) k))))

(define (fontset-spec/equal? xfs1 xfs2)
  (equal? xfs1 xfs2))

(define make-fontset-spec-hash-table
  (hash-table/constructor
    fontset-spec/hash-mod fontset-spec/equal? cons #t car cdr set-cdr!))

(define *fontset-spec-hash-table* (make-fontset-spec-hash-table))

(define (fontset-spec/canonicalize xfs)
  (list (car xfs)
	(sort (cadr xfs)
	      (lambda (fs1 fs2)
		(< (object-hash fs1) (object-hash fs2))))))

(define (fontset-spec/intern xfs)
  (if (or (not (list? xfs))
	  (not (= (length xfs) 2)))
      (error "Invalid fontset specification:" xfs))
  (set! xfs (fontset-spec/canonicalize xfs))
  (call/cc
    (lambda (exit)
      (hash-table/lookup
       *fontset-spec-hash-table*
       xfs
       (lambda (fs)
	 (exit fs))
       (lambda ()
	 (let ((fs (make-fontset-spec (car xfs) (cadr xfs))))
	   (hash-table/put! *fontset-spec-hash-table* xfs fs)
	   (exit fs)))))))

;
; Register Standard Styles & Named Style Values
;

(define (register-named-style-values list-of-named-style-values)
  (let loop ((l list-of-named-style-values))
    (if (not (null? l))
	(let ((nsv (car l)))
	  (face-style/register-named-value
	    (first nsv) (second nsv) (third nsv))
	  (loop (cdr l))))))

(define (register-styles list-of-styles)
  (let loop ((l list-of-styles))
    (if (not (null? l))
	(begin
	  (face-style/register (caar l) (cdar l))
	  (loop (cdr l))))))

(register-named-style-values
 '((weight  black 1.0)
   (weight  heavy 1.0)
   (weight  bold 0.8)
   (weight  semibold 0.67)
   (weight  demibold 0.67)
   (weight  medium 0.5)
   (weight  regular 0.5)
   (weight  light 0.33)
   (weight  thin 0.2)
   (posture upright 0.0)
   (posture italic 1.0)
   (posture oblique 1.0)
   (posture slanted 1.0)
   (width   extended 0.8)
   (width   medium 0.5)
   (width   condensed 0.33)
   (width   narrow 0.33)))

(register-styles
 '((bold	 . ((weight . bold)))
   (italic	 . ((posture . italic)))
   (oblique	 . ((posture . oblique)))
   (bold-italic  . ((weight . bold ) (posture . italic)))
   (bold-oblique . ((weight . bold ) (posture . oblique)))
   (narrow	 . ((width . narrow)))))

;
; Fonts
;

(define font/extended-metrics
  (let ((old-extended-metrics font/extended-metrics))
    (lambda (f #!optional type)
      (let ((em (old-extended-metrics f)))
	(if (default-object? type)
	    em
	    (let ((tem (assq type em)))
	      (and tem (cdr tem))))))))

;
; Given a font and a UCS code, map it to a glyph if available;
; otherwise, return default.
;
; The following is a QUICK HACK until we build real glyph complements!
;
; Notes:
;
; 1. Should take style variations argument in order to discriminate
; between multiple potential maps; e.g., swash, small cap, old style
; figures, etc.
;
;

(define (font/map f ucs default)
  (case (font/glyph-complement f)
    ((adobe-standard)
     (if (fix:< ucs #x80)
	 ucs
	 default))
    ((adobe-expert)
     (if (or (and (fix:>= ucs #x20)
		  (fix:<  ucs #x40))
	     (and (fix:>= ucs #x60)
		  (fix:<  ucs #x7b)))
	 ucs
	 default))
    ((apple-arabic)
     (let ((pm (font/extended-metrics f 'pre-map)))
       (if (not (null? pm))
	   (lookup-table/lookup pm ucs default)
	   default)))
    (else
     default)))

(define (font/contextual? f)
  (and (font/extended-metrics f 'context-maps) #t))

;
; The post mapper can be merged into the context mapper with a little
; work.  It would only apply on the last context map run.
;

(define (font/context-map! f cgs)
  (let ((cm (font/extended-metrics f 'context-maps))
	(pm (font/extended-metrics f 'post-map)))
    (let context-map ((l cm))
      (if (not (null? l))
	  (begin
	    (state-table/run! (car l) cgs)
	    (context-map (cdr l)))))
    (let post-map ((k 0))
      (let* ((lgi (glyph-stream/read-at cgs k))
	     (rator (gi/rator lgi)))
	(if (not (eq? rator 'end-of-stream))
	    (begin
	      (case (gi/rator lgi)
		((base attach)
		 (let* ((rands (gi/rands lgi))
			(code (cadr rands)))
		   (set-cdr! rands
			     (list (lookup-table/lookup pm code code))))))
	      (post-map (1+ k))))))
    unspecific))

;
; Font Instance
;

(define (font-instance/device fi)
  (font/device (font-instance/font fi)))

(define (font-instance/ascent fi)
  (device/font-ascent (font-instance/device fi) fi))

(define (font-instance/descent fi)
  (device/font-descent (font-instance/device fi) fi))

(define (font-instance/glyph-metrics fi gc #!optional gm)
  (if (default-object? gm)
      (device/font-glyph-metrics (font-instance/device fi) fi gc)
      (device/font-glyph-metrics (font-instance/device fi) fi gc gm)))

;
; Make the grossly unrealistic assumption that every font uses #x20 as
; its nominal space glyph!  [This will do for now.]
;
; Note:
;
; 1. Need to have standard way of having a font specify which glyph is
; its nominal "space" glyph.
;

(define (font-instance/space-metrics fi #!optional gm)
  (if (default-object? gm)
      (device/font-glyph-metrics (font-instance/device fi) fi #x20)
      (device/font-glyph-metrics (font-instance/device fi) fi #x20 gm)))

;
; Font Sets
;

(define make-font-set
  (let ((old-make-font-set make-font-set))
    (lambda ()
      (old-make-font-set '() '() '() '() '()))))

(define (font-set/font-spec font-or-font-set)
  (if (font-set? font-or-font-set)
      ((record-accessor *font-set-type* 'font-spec) font-or-font-set)
      (font/font-spec font-or-font-set)))

(define (font-set/extend! fs font-or-font-list)
  (if (not (list? font-or-font-list))
      (set! font-or-font-list (list font-or-font-list)))
  (let extend ((l font-or-font-list))
    (if (not (null? l))
	(let* ((f (car l))
	       (gc (font/glyph-complement f)))
	  (if (not (assq gc (font-set/fonts fs)))
	      (begin
		(font-set/set-fonts! fs
		  (cons (cons gc f) (font-set/fonts fs)))
		(if (null? (font-set/font-spec fs))
		    (font-set/set-font-spec! fs (font/font-spec f)))
		(font-set/set-glyph-complement! fs
		  (cons gc (font-set/glyph-complement fs)))
		(let update ((wsl (font/writing-systems f)))
		  (if (not (null? wsl))
		      (let ((ws (car wsl)))
			(if (not (memq ws (font-set/writing-systems fs)))
			    (font-set/set-writing-systems! fs
			      (cons ws (font-set/writing-systems fs))))
			(update (cdr wsl)))))
		;need to extended metrics
		))
	  (extend (cdr l))))))

;
; Given a font set and a UCS character code, map it to a glyph
; if available; otherwise, return default.
;
; The following should perform maps on the font set's glyph complement
; directly rather than iterating over the font set's fonts.  This will
; have to wait until we compile a real glyph complement for a font set.
;
; Notes:
; 
; 1. Should take style variations argument in order to discriminate
; between multiple potential maps; e.g., swash, small cap, old style
; figures, etc.
;
; 2. Returns multiple values (to be precise, returns 3 values).
;

(define (font-set/map font-or-font-set ucs default)
  (if (font? font-or-font-set)
      (let ((gc (font/map font-or-font-set ucs '())))
	(if (not (null? gc))
	    (values gc font-or-font-set (font/contextual? font-or-font-set))
	    (values default '() '())))
      (let loop ((fl (font-set/fonts font-or-font-set)))
	(if (null? fl)
	    (values default '() '())
	    (let ((gc (font/map (car fl) ucs '())))
	      (if (not (null? gc))
		  (let ((f (car fl)))
		    (values gc f (font/contextual? f)))
		  (loop (cdf fl))))))))

;
; Font Catalogs
;

(define make-font-catalog
  (let ((old-make-font-catalog make-font-catalog))
    (lambda ()
      (old-make-font-catalog (make-object-hash-table) '() '()))))

(define (font-catalog/compute-families fc)
  (let ((ft (make-string-hash-table)))
    (hash-table/for-each (font-catalog/font-sets fc)
      (lambda (fs font-or-font-set)
	font-or-font-set	;ignore
	(let ((ffs (hash-table/get ft (font-spec/family fs) '())))
	  (set! ffs (cons fs ffs))
	  (hash-table/put! ft (font-spec/family fs) ffs))))
    ft))

(define font-catalog/families
  (let ((families font-catalog/families)
	(set-families! font-catalog/set-families!))
    (lambda (fc)
      (let ((ft (families fc)))
	(if (null? ft)
	    (begin
	      (set! ft (font-catalog/compute-families fc))
	      (set-families! fc ft)))
	ft))))

(define (font-catalog/family-names fc)
  (let ((ft (font-catalog/families fc)))
    (and ft
	 (sort
	   (map (lambda (ff) (car ff)) (hash-table/entries-list ft))
	   string-ci<?))))
	 
;
; Need to cull duplicates in the result of the following due to
; multiple fonts in family with same faces but different sizes.
; That is, the family table maps a family to a list of font face/size
; instances, not simply to a face list.
;

(define (font-catalog/family/faces fc family-name)
  (let ((ft (font-catalog/families fc)))
    (and ft
	 (let ((fl (hash-table/get ft family-name '())))
	   (and fl
		(map (lambda (fs) (font-spec/face fs)) fl))))))

;
; Support fuzzy matching on font lookup.  If no exact match,
; then lookup for font instances of specified family, unifying
; specified font spec with the list of font instances.
;

(define (font-catalog/lookup fc fs #!optional fuzzy?)
  (if (default-object? fuzzy?)
      (set! fuzzy? #f))
  (or (hash-table/get (font-catalog/font-sets fc) fs '())
      (and fuzzy?
	   (let ((ft (font-catalog/families fc)))
	     (and ft
		  (let ((fl (hash-table/get ft (font-spec/family fs) '())))
		    (and fl
			 (let ((nfs (font-spec/match fs fl)))
			   (font-catalog/lookup fc nfs #f)))))))))

;
; Special Case Lookup - find & create font instance for ASCII using
; font from matching font set.
;
; Notes:
;
; 1. Should have a font-set/map that takes a range/list of codes and
; returns boolean indicating potential map.  For now, just see if 'A' maps.
;

(define (font-catalog/ascii-font-instance fc fs)
  (let ((afs (font-catalog/lookup fc fs #t)))
    (and afs
	 (with-values
	     (lambda ()
	       (font-set/map afs #x41 '()))				; N1
	   (lambda (gc f contextual?)
	     gc		 ;ignore
	     contextual? ;ignore
	     (and f
		  (let ((ufs (font-spec/unification fs (font/font-spec f))))
		    (make-font-instance f
					(font-spec/size ufs)
					(font-spec/face ufs)))))))))

(define (font-catalog/extend! fc font-or-font-set)
  (letrec ((put!
	    (lambda (f)
	      (hash-table/put!
	       (font-catalog/font-sets fc) (font-set/font-spec f) f)))
	   (update!
	    (lambda ()
	      ;update writing system cache
	      unspecific)))
    (let ((of (font-catalog/lookup fc (font-set/font-spec font-or-font-set))))
      (if (null? of)
	  (put! font-or-font-set)
	  (if (font? font-or-font-set)
	      (if (font? of)
		  (let ((fs (make-font-set)))
		    (font-set/extend! fs (list of font-or-font-set))
		    (put! fs))
		  (font-set/extend! of font-or-font-set))
	      (if (font-set? of)
		  (font-set/extend! of (font-set/fonts font-or-font-set))
		  (begin
		    (font-set/extend! font-or-font-set of)
		    (put! font-or-font-set)))))
      (update!))))

