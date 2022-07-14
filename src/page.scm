
;
; Page Primitives
;

(define-record margins (left right top bottom))

(define-record page
  ( viewer		;viewer where this page may appear
    visible?		;#t if cuurently visible
    origin		;origin in viewer's coordinate space
    width		;horizontal width
    height		;vertical height
    margins		;margins for page
    first-column	;head of page's column list
    last-column		;tail of page's column list
    number		;page number
    next		;next page in chain
    previous		;previous page in chain
  ))

(define *default-page-template* 'standard-double)

(define make-page
  (let ((old-make-page make-page))
    (lambda (v origin wd ht margins pno prev)
      (if (default-object? pt)
	  (set! pt *default-page-template*))
      (let ((p (old-make-page v #f origin wd ht margins '() '() pno '() prev)))
	(if (not (null? prev))
	    (page/set-next! prev p))
	p))))

(define (page/device p)
  (viewer/device (page/viewer p)))

(define (page/for-each-column p proc)
  (let loop ((c (page/first-column p)))
    (if (not (null? c))
	(begin
	  (proc c)
	  (loop (column/next c)))))
  unspecific)

;
; Page bounding box in viewer's coordinates.
;

(define (page/bbox p)
  (let ((po (page/origin p)))
    (make-rect po (make-point
		   (+ (point/x po) (page/width p))
		   (+ (point/y po) (page/height p))))))

(define (page/erase p)
  (let ((gd (page/device p)))
    (device/clear-rectangle gd (page/bbox p))
    unspecific))

(define (page/redisplay p #!optional erase)
  (let ((gd (page/device p)))
    (if (and (not (default-object? erase)) erase)
	(page/erase p))
    (device/with-origin gd (page/origin p)
      (page/for-each-column p
        (lambda (c)
	  (column/redisplay c))))
    unspecific))

(define (page/show p origin #!optional redisplay-now)
  (page/set-visible?! p #t)
  (page/set-origin! p origin)
  (if (or (default-object? redisplay-now) redisplay-now)
      (page/redisplay p))
  unspecific)

(define (page/hide p)
  (page/set-visible?! p #f)
  (page/erase p)
  (page/set-origin! p '())
  unspecific)
    
;
; Add column to end of page's column list.
;

(define (page/add-column! p c)
  (let ((lc (page/last-column p)))
    (if (null? lc)
	(page/set-first-column! p c)
	(begin
	  (column/set-next! lc c)
	  (column/set-previous! c lc)))
    (page/set-last-column! p c)
    (column/set-page! c p)
    unspecific))

;
; Remove column from page's column list.
;

(define (page/remove-column! p c)
  (let ((prev (column/previous c))
	(next (column/next c)))
    (if (not (null? prev))
	(column/set-next! prev next))
    (if (not (null? next))
	(column/set-previous! next prev))
    (if (eq? (page/first-column p) c)
	(page/set-first-column! p next))
    (if (eq? (page/last-column p) c)
	(page/set-last-column! p prev))
    (column/set-next! c '())
    (column/set-previous! c '())
    (column/set-page! c '())
    unspecific))

(define (page/set-borders! p)
  (page/for-each-column p
    (lambda (c)
      (column/set-border! c 'solid))))

(define (page/clear-borders! p)
  (page/for-each-column p
    (lambda (c)
      (column/set-border! c 'none))))

;
; Search for first column of specified TYPE on pages following P.
;

(define (page/find-column-forward p type)
  (let loop ((np (page/next p)))
    (if (null? np)
	'()
	(or (page/find-first-column np type)
	    (loop (page/next p))))))

;
; Search for last column of specified TYPE on pages preceeding P.
;

(define (page/find-column-backward p type)
  (let loop ((pp (page/previous p)))
    (if (null? pp)
	'()
	(or (page/find-last-column pp type)
	    (loop (page/previous p))))))

;
; Search for first column of specified TYPE on page P.
;

(define (page/find-first-column p type)
  (call/cc
    (lambda (exit)
      (let loop ((c (page/first-column p)))
	(if (null? c)
	    '()
	    (if (eq? (column/content-type c) type)
		(exit c)
		(loop c (column/next c))))))))

;
; Search for last column of specified TYPE on page P.
;

(define (page/find-last-column p type)
  (call/cc
    (lambda (exit)
      (let loop ((c (page/last-column p)))
	(if (null? c)
	    '()
	    (if (eq? (column/content-type c) type)
		(exit c)
		(loop c (column/previous c))))))))

;
; Find column on page with specific name.
;

(define (page/find-named-column p name)
  (call/cc
    (lambda (exit)
      (page/for-each-column p
        (lambda (c)
	  (if (string-ci=? (column/name c) name)
	      (exit c))))
      '())))

;
; Find given line in specified column.
;

(define (page/find-line p column-name line-number)
  (let ((c (page/find-named-column p column-name)))
    (and c
	 (column/find-line c line-number))))
