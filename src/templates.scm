
;
; Standard Page Template Columns
;

(define (page/make-standard-single! p pt)
  pt	;ignore
  (let* ((pm (page/margins p))
	 (c (make-column p
			 "STANDARD-1"
			 *default-document-content-type*		; XXX
			 (make-point (margins/left pm) (margins/bottom pm))
			 (- (page/width p)
			    (+ (margins/left pm) (margins/right pm)))
			 (- (page/height p)
			    (+ (margins/top pm) (margins/bottom pm)))
			 'horizontal
			 'normal
			 #f
			 'dotted
			 )))
    (page/add-column! p c)
    unspecific))

(define (page/make-standard-double! p pt)
  pt	;ignore
  (let* ((pm (page/margins p))
	 (cs (in->pt 0.20))	;column separation
	 (cw (* 0.5
		(- (page/width p)
		   (+ (margins/left pm) (margins/right pm) (in->pt 0.25)))))
	 (ch (- (page/height p) (+ (margins/top pm) (margins/bottom pm))))
	 (c1 (make-column p
			  "STANDARD-2-1"
			  *default-document-content-type*		; XXX
			  (make-point (margins/left pm)
				      (margins/bottom pm))
			  cw
			  ch
			  'horizontal
			  'normal
			  #f
			  'dotted
			  ))
	 (c2 (make-column p
			  "STANDARD-2-2"
			  *default-document-content-type*		; XXX
			  (make-point (+ (margins/left pm) cw cs)
				      (margins/bottom pm))
			  cw
			  ch
			  'horizontal
			  'normal
			  #f
			  'dotted
			  )))
    (page/add-column! p c1)
    (page/add-column! p c2)
    unspecific))
  

(define (page/make-columns! p pt)
  (cond
   ((symbol? pt)
    (case pt
      ((standard-single single) (page/make-standard-single! p pt))
      ((standard-double double) (page/make-standard-double! p pt))
      (else
       (error "Unknown page template:" pt))))
   ((list? pt)
    (error "Full page template not yet supported:" pt))
   (else
    (error "Invalid page template:" pt)))
  unspecific)

