
(define (bidi! enable?)
  (if enable?
      (begin
	(set! *default-style* *bidi-style*)
	(default-paragraph-style/set-flow! 'rtl))
      (begin
	(set! *default-style* *normal-style*)
	(default-paragraph-style/set-flow! 'ltr))))
