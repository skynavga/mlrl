
;
; Content Units
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(declare (usual-integrations))

;
; Content units serve as a binding of a text object and its rendered
; form.  A list of renderings for the text object are cached for future
; reference.  Since a rendering is sensitive to the font catalog used,
; we use the font catalog as the key for an alist containing the results
; of calls to text/render.
;
; The last-font slot serves as a register to hold the last font instance
; encountered during previous formatting of this content unit.  That is,
; this content unit was broken across a column boundary and thus its
; formatting must be resumed on a new column in the future.  When this
; happens we need to know what font instance to start with; otherwise we
; would have to search through the logical glyph stream for the content
; unit in order to recover the last font.
;
; Notes:
;
; 1. Is this the right place to save such information?
;
; 2. There is a problem in storing this here rather than in the
;    renderings alist, since one might format part of the content unit
;    with one font catatlog then format another part with another font
;    catalog.  In this case, the last-font register wouldn't be coherent
;    since it won't necessarily match the font catalog.
;
; 3. Do we also need to save the last style?
;

(define-record content-unit (text renderings last-font next previous))

(define (make-content-unit text)
  ((record-constructor (record-type content-unit)) text '() '() '() '()))

(define (content-unit/style cu)
  (text/default-style (content-unit/text cu)))

(define (content-unit/set-rendering! cu fc lgs)
  (let* ((rl (content-unit/renderings cu))
	 (r (assq fc rl)))
    (if (not (null? r))
	(set! rl (del-assq! fc rl)))
    (content-unit/set-renderings! cu
      (if (null? rl)
	  (list (cons fc lgs))
	  (cons (cons fc lgs) rl)))))

(define (content-unit/clear-rendering! cu fc)
  (let* ((rl (content-unit/renderings cu))
	 (r (assq fc rl)))
    (if (not (null? r))
	(set! rl (del-assq! fc rl)))
    (content-unit/set-renderings! cu rl)))

(define (content-unit/clear-renderings! cu)
  (content-unit/set-renderings! cu '()))

(define (content-unit/rendering cu fc)
  (let ((r (assq fc (content-unit/renderings cu))))
    (or (and r (cdr r))
	(let ((lgs (text/render (content-unit/text cu) fc)))
	  (content-unit/set-rendering! cu fc lgs)
	  lgs))))
