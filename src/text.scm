
;
; Text System
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(declare (usual-integrations))

(define-record text (content gap-offset gap-length annotations))

(define-record annotations (markers intervals))

(define make-text
  (let ((old-make-text make-text))
    (lambda (#!optional utf)
      (if (default-object? utf)
	  (set! utf (string)))
      (old-make-text (utf->ucs utf) 0 0 (make-annotations '() '())))))

(define text/length
  (lambda (t)
    (if (not (text? t))
	(error "Invalid text object:" t)
	(- (vector-length (text/content t)) (text/gap-length t)))))

(define text/point
  (lambda (t)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (text/gap-offset t)))

(define text/set-point!
  (lambda (t offset)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (< offset 0)
	(set! offset 0))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (if (not (= (text/gap-offset t) offset))
	(text/move-gap! t offset))))

(define text/insert!
  (lambda (t utf #!optional offset)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (if (not (= (text/gap-offset t) offset))
	(text/move-gap! t offset))
    (let* ((ucs (utf->ucs utf))
	   (ucslen (ucs/length ucs)))
      (if (< (text/gap-length t) ucslen)
	  (text/grow-gap! t ucslen))
      (subvector-move-right! ucs 0 ucslen
			    (text/content t) (text/gap-offset t))
      (text/adjust-gap! t ucslen))))

(define text/append!
  (lambda (t utf)
    (text/insert! t utf (text/length t))))

(define text/delete!
  (lambda (t #!optional start end)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start (- (text/point t) 1)))
    (if (< start 0)
	(set! start 0))
    (let ((tlen (text/length t)))
      (if (default-object? end)
	  (set! end (text/point t)))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! end start))
    (if (not (= (text/gap-offset t) end))
	(text/move-gap! t end))
    (text/adjust-gap! t (- start end))))

(define text/clear!
  (lambda (t)
    (text/delete! t 0 (text/length t))))

(define text/replace!
  (lambda (t utf #!optional start end)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start (text/point t)))
    (if (default-object? end)
	(set! end (text/point t)))
    (let ((tlen (text/length t)))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! start end))
    (if (not (= (text/gap-offset t) end))
	(text/move-gap! t end))
    (text/adjust-gap! t (- start end))
    (let* ((ucs (utf->ucs utf))
	   (ucslen (ucs/length ucs)))
      (if (< (text/gap-length t) ucslen)
	  (text/grow-gap! t ucslen))
      (subvector-move-left! ucs 0 ucslen
			    (text/content t) (text/gap-offset t))
      (text/adjust-gap! t ucslen))))

(define text/extract
  (lambda (t #!optional start end)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start 0))
    (let ((tlen (text/length t)))
      (if (default-object? end)
	  (set! end tlen))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! start end))
    (let ((goff (text/gap-offset t))
	  (reset-gap #f))
      (if (< goff end)
	  (begin
	    (text/move-gap! t end)
	    (set! reset-gap #t)))
      (let ((utf (ucs->utf (subvector (text/content t) start end))))
	(if reset-gap
	    (text/move-gap! t goff))
	utf))))

;
; Search Functions
;

(define (text/search-forward-positive t cset #!optional offset)
  (if (not (text? t))
      (error "Invalid text object:" t))
  (if (default-object? offset)
      (set! offset (text/point t)))
  (let ((tlen (text/length t)))
    (if (> offset tlen)
	(set! offset tlen))
    (if (not (cset/empty? cset))
      (let ((goff (text/gap-offset t))
	    (glen (text/gap-length t))
	    (cv (text/content t)))
	(letrec ((loop
		  (lambda (k)
		    (cond
		      ((>= k tlen) tlen)
		      ((cset/member? cset
			 (vector-ref cv (if (>= k goff) (+ k glen) k))) k)
		      (else (loop (1+ k)))))))
	  (loop offset)))
      tlen)))

(define (text/search-forward-negative t cset #!optional offset)
  (if (not (text? t))
      (error "Invalid text object:" t))
  (if (default-object? offset)
      (set! offset (text/point t)))
  (let ((tlen (text/length t)))
    (if (> offset tlen)
	(set! offset tlen))
    (if (not (cset/empty? cset))
      (let ((goff (text/gap-offset t))
	    (glen (text/gap-length t))
	    (cv (text/content t)))
	(letrec ((loop
		  (lambda (k)
		    (cond
		      ((>= k tlen) tlen)
		      ((not
			 (cset/member? cset
			   (vector-ref cv (if (>= k goff) (+ k glen) k)))) k)
		      (else (loop (1+ k)))))))
	  (loop offset)))
      offset)))

;
; Gap Management Functions
;

(define *reset-gap-contents* #f)
(define *large-gap-slop-limit* 1024)
(define *small-gap-slop* 64)
(define *large-gap-slop* 256)

(define text/grow-gap!
  (lambda (t len)
    (let* ((ov (text/content t))
	   (ovlen (vector-length ov))
	   (t2 (+ (text/gap-offset t) (text/gap-length t)))
	   (t2len (- ovlen t2))
	   (slop (if (< (text/length t) *large-gap-slop-limit*)
		     *small-gap-slop* *large-gap-slop*))
	   (grow (- (+ len slop) (text/gap-length t)))
	   (nv (vector-grow ov (+ ovlen grow))))
      (subvector-move-right! nv t2 ovlen nv (- (vector-length nv) t2len))
      (text/set-gap-length! t (+ (text/gap-length t) grow))
      (text/set-content! t nv)
      (if *reset-gap-contents*
	  (subvector-fill! nv
	    (text/gap-offset t)
	    (+ (text/gap-offset t) (text/gap-length t)) 0)))))

(define text/move-gap!
  (lambda (t off)
    (let* ((cv (text/content t))
	   (goff (text/gap-offset t))
	   (glen (text/gap-length t)))
      (cond
       ((< off goff)
	(subvector-move-right! cv off goff cv (+ off glen)))
       ((> off goff)
	(subvector-move-left! cv (+ goff glen) (+ off glen) cv goff)))
      (text/set-gap-offset! t off)
      (if *reset-gap-contents*
	  (subvector-fill! cv
	    (text/gap-offset t)
	    (+ (text/gap-offset t) (text/gap-length t)) 0)))))

(define text/adjust-gap!
  (lambda (t cnt)
    (text/set-gap-offset! t (+ (text/gap-offset t) cnt))
    (text/set-gap-length! t (+ (text/gap-length t) (- cnt)))
    (if *reset-gap-contents*
	(subvector-fill! (text/content t)
	  (text/gap-offset t)
	  (+ (text/gap-offset t) (text/gap-length t)) 0))))

;
; Marker Annotation Functions
;

;
; Markers have a primary and secondary ordering.  A marker key
; consists of a pair ( offset . assoc ), where offset determines
; the primary order, and assoc determines the secondary order.
; The assoc is one of the keywords in *marker-associations*,
; currently defined as :before, :empty, and :after, this being their
; defined ordering.
;

(define *marker-associations*
  '(:empty :before :after))

(define *default-marker-association* ':empty)

(define compare-markers
  (lambda (mk1 mk2)
    (let ((o1 (car mk1))
	  (o2 (car mk2)))
      (cond
       ((< o1 o2) -1)
       ((> o1 o2)  1)
       (else
	(let ((a1 (cdr mk1))
	      (a2 (cdr mk2)))
	  (if (eq? a1 a2)
	      0
	      (case a1
		((:before) -1)
		((:after)   1)
		((:empty)
		 (case a2
		   ((:before) 1)
		   ((:after) -1)
		   (else (error "Invalid marker association:" a2))))
		(else
		 (error "Invalid marker association:" a1))))))))))

(define text/set-marker!
  (lambda (t key value #!optional offset assoc)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (if (default-object? assoc)
	(set! assoc *default-marker-association*))
    (let ((hbb (annotations/markers (text/annotations t))))
      (if (null? hbb)
	  (begin
	    (set! hbb (make-hbb))
	    (annotations/set-markers! (text/annotations t) hbb)))
      (let ((mk (cons offset assoc)))
	(hbb/get hbb mk compare-markers
	  (lambda (bc)
	    (if (bindings/bound? bc key)
		(bindings/assign! bc key value)
		(bindings/extend! bc key value)))
	  (lambda ()
	    (let ((bc (make-bindings)))
	      (bindings/extend! bc key value)
	      (hbb/put! hbb mk bc compare-markers))))))))

(define text/clear-marker!
  (lambda (t key #!optional offset assoc)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (if (default-object? assoc)
	(set! assoc *default-marker-association*))
    (let ((hbb (annotations/markers (text/annotations t))))
      (if hbb
	  (let ((mk (cons offset assoc)))
	    (hbb/get hbb mk compare-markers
              (lambda (bc)
		(begin
		 (if (bindings/bound? bc key)
		     (bindings/unassign! bc key))
		 (if (bindings/empty? bc)
		     (hbb/clear! hbb mk compare-markers))))))))))

(define text/clear-markers!
  (lambda (t #!optional key start end assoc)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start (text/point t)))
    (let ((tlen (text/length t)))
      (if (default-object? end)
	  (set! end tlen))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! start end))
    (let ((hbb (annotations/markers (text/annotations t))))
      (if hbb
	  (hbb/for-each hbb
	    (lambda (mk bc)
	      (if (or (default-object? assoc)
		      (eq? (cdr mk) assoc))
		  (if (default-object? key)
		      (hbb/clear! hbb mk compare-markers)
		      (begin
			(if (not (text/defaults-marker? mk key))
			    (bindings/unassign! bc key))
			(if (bindings/empty? bc)
			    (hbb/clear! hbb mk compare-markers))))))
	    (cons start ':before) (cons end ':after) compare-markers)))))

(define text/clear-all-markers!
  (lambda (t)
    (text/clear-markers! t)
    (text/clear-defaults! t)))

(define text/get-marker
  (lambda (t key default-value #!optional offset assoc)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (if (default-object? assoc)
	(set! assoc *default-marker-association*))
    (let ((hbb (annotations/markers (text/annotations t))))
      (if (null? hbb)
	  default-value
	  (let ((mk (cons offset assoc)))
	    (hbb/get hbb mk compare-markers
	      (lambda (bc)
		(if (bindings/bound? bc key)
		    (bindings/lookup bc key)
		    default-value))
	      (lambda () default-value)))))))

(define text/for-each-marker
  (lambda (t proc #!optional key start end assoc)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start (text/point t)))
    (let ((tlen (text/length t)))
      (if (default-object? end)
	  (set! end tlen))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! start end))
    (let ((hbb (annotations/markers (text/annotations t))))
      (if hbb
	  (hbb/for-each hbb
	    (lambda (mk bc)
	      (if (or (default-object? assoc)
		      (eq? (cdr mk) assoc))
		  (if (default-object? key)
		      (bindings/for-each bc
		        (lambda (k v)
			  (proc (car mk) (cdr mk) k v)))
		      (begin
			(if (bindings/bound? bc key)
			    (proc (car mk) (cdr mk)
			      key (bindings/lookup bc key)))))))
	    (cons start ':before) (cons end ':after) compare-markers)))))

;
; Interval Annotation Functions
;

(define text/set-interval!
  (lambda (t key value start end)
    t		;ignore
    key		;ignore
    value	;ignore
    start	;ignore
    end		;ignore
    unspecific))

(define text/clear-interval!
  (lambda (t key start end)
    t		;ignore
    key		;ignore
    start	;ignore
    end		;ignore
    unspecific))

(define text/clear-intervals!
  (lambda (t key #!optional start end)
    key		;ignore
    (if (default-object? start)
	(set! start 0))
    (if (default-object? end)
	(set! end (text/length t)))
    unspecific))

(define text/get-interval
  (lambda (t key start end)
    t		;ignore
    key		;ignore
    start	;ignore
    end		;ignore
    (let ((value))
      value)))

(define text/for-each-interval
  (lambda (t key proc #!optional start end)
    key		;ignore
    proc	;ignore
    (if (default-object? start)
	(set! start 0))
    (if (default-object? end)
	(set! end (text/length t)))
    unspecific))
    
;
; Defaults Functions
;

(define *defaults-marker* '(0 . :before))

(define *defaults-marker-key* 'defaults)

(define text/defaults-marker?
  (lambda (mk key)
    (and (eq? (car mk) (car *defaults-marker*))
	 (eq? (cdr mk) (cdr *defaults-marker*))
	 (eq? key *defaults-marker-key*))))

(define text/set-default!
  (lambda (t key value)
    (let ((db (text/get-marker t *defaults-marker-key* '()
			       (car *defaults-marker*)
			       (cdr *defaults-marker*))))
      (if (null? db)
	  (begin
	    (set! db (make-bindings))
	    (text/set-marker! t *defaults-marker-key* db
			      (car *defaults-marker*)
			      (cdr *defaults-marker*))))
      (bindings/extend! db key value))))

(define text/clear-default!
  (lambda (t key)
    (let ((db (text/get-marker t *defaults-marker-key* '()
			       (car *defaults-marker*)
			       (cdr *defaults-marker*))))
      (if db
	  (bindings/unassign! db key)))))

(define text/clear-defaults!
  (lambda (t)
    (text/clear-marker! t *defaults-marker-key*
			(car *defaults-marker*)
			(cdr *defaults-marker*))))

(define text/get-default
  (lambda (t key default-value)
    (let ((db (text/get-marker t *defaults-marker-key* '()
			       (car *defaults-marker*)
			       (cdr *defaults-marker*))))
      (if (and (not (null? db))
	       (bindings/bound? db key))
	  (bindings/lookup db key)
	  default-value))))

;
; Writing System Functions
;

(define *writing-system-marker-key* 'ws)

(define text/set-default-writing-system!
  (lambda (t ws)
    (text/set-default! t *writing-system-marker-key* ws)))

(define text/default-writing-system
  (lambda (t)
    (text/get-default t *writing-system-marker-key* '())))

(define text/set-writing-system!
  (lambda (t ws #!optional offset)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (text/set-marker! t *writing-system-marker-key* ws offset ':after)))
    
(define text/writing-system
  (lambda (t #!optional offset)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (call-with-current-continuation
     (lambda (exit)
       (let ((last (make-cell (text/default-writing-system t))))
	 (text/for-each-marker t
	   (lambda (o a k v)
	     a  ;ignore
	     k  ;ignore
	     (cond
	       ((< o offset)
		(set-cell-contents! last v))
	       ((= o offset)
		(exit (or v (text/default-writing-system t))))
	       (else
		(exit (cell-contents last)))))
	   *writing-system-marker-key* 0)
	 (cell-contents last))))))

(define remove-subsequent-duplicates
  (lambda (l compare)
    (letrec ((loop
	      (lambda (h t)
		(if (null? t)
		    (list h)
		    (if (compare h (car t))
			(loop h (cdr t))
			(cons h (loop (car t) (cdr t))))))))
      (if (or (null? l)
	      (not (pair? l)))
	  l
	  (loop (car l) (cdr l))))))

(define text/writing-systems
  (lambda (t #!optional start end)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start (text/point t)))
    (let ((tlen (text/length t)))
      (if (default-object? end)
	  (set! end tlen))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! start end))
    (let* ((default (text/default-writing-system t))
	   (first (text/writing-system t start))
	   (wsc (make-cell (list (cons start first)))))
      (text/for-each-marker t
	(lambda (o a k v)
	  a  ;ignore
	  k  ;ignore
	  (set-cell-contents! wsc
	    (cons (cons o (or v default)) (cell-contents wsc))))
	*writing-system-marker-key* start end)
      (remove-subsequent-duplicates
        (reverse (cell-contents wsc))
	(lambda (p1 p2)
	  (eq? (cdr p1) (cdr p2)))))))

;
; Style Functions
;

(define *style-marker-key* 'style)

(define text/set-default-style!
  (lambda (t s)
    (text/set-default! t *style-marker-key* s)))

(define text/default-style
  (lambda (t)
    (text/get-default t *style-marker-key* '())))

(define text/set-style!
  (lambda (t s #!optional offset)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (text/set-marker! t *style-marker-key* s offset ':after)))
    
(define text/style
  (lambda (t #!optional offset)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? offset)
	(set! offset (text/point t)))
    (let ((tlen (text/length t)))
      (if (> offset tlen)
	  (set! offset tlen)))
    (call-with-current-continuation
     (lambda (exit)
       (let ((last (make-cell '())))
	 (text/for-each-marker t
	   (lambda (o a k v)
	     a  ;ignore
	     k  ;ignore
	     (cond
	       ((< o offset)
		(set-cell-contents! last v))
	       ((= o offset)
		(exit (or v (text/default-style t))))
	       (else
		(exit (or (cell-contents last) (text/default-style t))))))
	   *style-marker-key* 0)
	 (or (cell-contents last)
	     (text/default-style t)))))))

(define text/styles
  (lambda (t #!optional start end)
    (if (not (text? t))
	(error "Invalid text object:" t))
    (if (default-object? start)
	(set! start (text/point t)))
    (let ((tlen (text/length t)))
      (if (default-object? end)
	  (set! end tlen))
      (if (> end tlen)
	  (set! end tlen)))
    (if (> start end)
	(set! start end))
    (let* ((default (text/default-style t))
	   (first (text/style t start))
	   (sc (make-cell (list (cons start first)))))
      (text/for-each-marker t
	(lambda (o a k v)
	  a  ;ignore
	  k  ;ignore
	  (set-cell-contents! sc
	    (cons (cons o (or v default)) (cell-contents sc))))
	*style-marker-key* start end)
      (remove-subsequent-duplicates
        (reverse (cell-contents sc))
	(lambda (p1 p2) (style-equal? (cdr p1) (cdr p2)))))))
