
;
; ISO/IEC 10646-1:1993 CharSet Data
;


(define (make-cset . rest)
  rest)

(define (cset/empty)
  (make-cset))

(define (cset/whitespace)
  (make-cset #x0020				;SPACE
	     #x0009				;HORIZONTAL TAB
	     #x000A				;LINE FEED
	     #x000B				;VERTICAL TAB
	     #x000C				;FORM FEED
	     #x000D				;CARRIAGE RETURN
	     #x00A0				;NON-BREAKING SPACE
	     #x2028				;LINE SEPARATOR
	     #x2029))				;PARAGRAPH SEPARATOR

(define (cset/paragraph-boundaries)
  (make-cset #x000A				;LINE FEED
	     #x2029))				;PARAGRAPH SEPARATOR

(define (cset/empty? cset)
  (null? cset))

(define (cset/member? cset c)
  (memq c cset))

