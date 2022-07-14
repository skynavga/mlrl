
;
; Document Viewer
;

(define-record viewer
  ( document		;document instance
    formatter		;formatter instance
    pages		;list of formatted pages
    visible-pages	;list of visible pages
    page-origin		;default page origin
    page-width		;default page width
    page-height		;default page height
    page-margins	;default page margins
    page-template	;default page template
    last-page-number ))	;highest formatted page number

(define (viewer/open page-template filename . device-args)
  (call/cc
    (lambda (exit)
      (bind-condition-handler
        (list error-type:open-file)
	(lambda (c)
	  (condition/write-report c)
	  (exit '()))
	(lambda ()
	  (set! filename (canonicalize-input-filename filename))
	  (let* ((f (formatter/open device-args))
		 (gr (device/bbox (formatter/device f)))
		 (v (make-viewer
		       (document/open filename)
		       f
		       '()
		       '()
		       (make-point 0 0)
		       (rect/width gr)
		       (rect/height gr)
		       (make-margins (in->pt 0.5)
				     (in->pt 0.5)
				     (in->pt 0.5)
				     (in->pt 0.5))
		       page-template
		       0)))
	    (viewer/goto-page v 1 #t)
	    v))))))

(define (viewer/device v)
  (formatter/device (viewer/formatter v)))

(define (viewer/device-type v)
  (device/type (viewer/device v)))

(define (viewer/close v)
  (device/close (viewer/device v)))

(define (viewer/refresh v)
  (device/erase (viewer/device v))
  (for-each
    (lambda (p)
      (page/redisplay p))
    (viewer/visible-pages v))
  unspecific)

(define (viewer/refresh-page v p)
  v	;ignore!
  (if (page/visible? p)
      (page/redisplay p #t))
  unspecific)

(define (viewer/format-page v pno)
  (let* ((pp (if (= pno 1) '() (viewer/find-page v (- pno 1))))
	 (p (make-page v
		       (viewer/page-origin v)
		       (viewer/page-width v)
		       (viewer/page-height v)
		       (viewer/page-margins v)
		       pno
		       pp)))
    (page/make-columns! p (viewer/page-template v))
    (formatter/format (viewer/formatter v) (viewer/document v) p)
    (viewer/set-pages! v (cons p (viewer/pages v)))
    (viewer/set-last-page-number!
      v (max (page/number p) (viewer/last-page-number v)))
    p))

(define (viewer/find-page v pno)
  (if (<= pno (viewer/last-page-number v))
      (list-search-positive
	  (viewer/pages v)
	(lambda (p)
	  (eq? pno (page/number p))))
      (if (document/content-exhausted? (viewer/document v))
	  (viewer/find-page v (viewer/last-page-number v))
	  (viewer/format-page v pno))))

(define (viewer/goto-page v pno #!optional erase)
  (if (default-object? erase)
      (set! erase #f))
  (let ((p (viewer/find-page v pno)))
    (if (or (null? p)
	    (not (page/visible? p)))
	(begin
	  (for-each
	   (lambda (p)
	     (page/hide p))
	   (viewer/visible-pages v))
	  (viewer/set-visible-pages! v (list p))
	  (page/show p (make-point 0 0) erase)))
    unspecific))

(define (viewer/set-borders! v)
  (for-each
    (lambda (p)
      (page/set-borders! p))
    (viewer/pages v))
  (viewer/refresh v))

(define (viewer/clear-borders! v)
  (for-each
    (lambda (p)
      (page/clear-borders! p))
    (viewer/pages v))
  (viewer/refresh v))

(define (viewer/dump-page v file)
  (with-output-to-file file
    (lambda ()
      (let ((old-sender ps/send)
	    (gd (viewer/device v)))
	(set! ps/send (lambda (d ps) (format #t "~A~%" ps)))
	(device/download-prologue gd)
	(viewer/refresh v)
	(device/download-epilogue gd)
	(set! ps/send old-sender)
	unspecific))))

(define (viewer/find-line v column-name line-number)
  (let ((p (and (not (null? (viewer/visible-pages v)))
		(car (viewer/visible-pages v)))))
    (if (null? p)
	'()
	(page/find-line p column-name line-number))))
