
;
; Non-Spacing Mark Database
;
; Notes:
;
; 1. Data for Bengali, Gurmukhi, Gujarati, Oriya, Telugu, Kannada,
; Malayalam, and Lao scripts have not yet been entered.
;

(define *nsm-table-data*
  '(#x0300	;general diacritics
    #x0301
    #x0302
    #x0303
    #x0304
    #x0305
    #x0306
    #x0307
    #x0308
    #x0309
    #x030a
    #x030b
    #x030c
    #x030d
    #x030e
    #x030f
    #x0310
    #x0311
    #x0312
    #x0313
    #x0314
    #x0315
    #x0316
    #x0317
    #x0318
    #x0319
    #x031a
    #x031b
    #x031c
    #x031d
    #x031e
    #x031f
    #x0320
    #x0321
    #x0322
    #x0323
    #x0324
    #x0325
    #x0326
    #x0327
    #x0328
    #x0329
    #x032a
    #x032b
    #x032c
    #x032d
    #x032e
    #x032f
    #x0330
    #x0331
    #x0332
    #x0333
    #x0334
    #x0335
    #x0336
    #x0337
    #x0338
    #x0339
    #x033a
    #x033b
    #x033c
    #x033d
    #x033e
    #x033f
    #x0340
    #x0341
    #x0342
    #x0343
    #x0344
    #x0345
    #x0360
    #x0361
    #x0483	;cyrillic
    #x0484
    #x0485
    #x0486
    #x05b0	;hebrew
    #x05b1
    #x05b2
    #x05b3
    #x05b4
    #x05b5
    #x05b6
    #x05b7
    #x05b8
    #x05b9
    #x05bb
    #x05bc
    #x05bd
    #x05bf
    #x05c0
    #x05c1
    #x05c2
    #x064b	;arabic
    #x064c
    #x064d
    #x064e
    #x064f
    #x0650
    #x0651
    #x0652
    #x06d6
    #x06d7
    #x06d8
    #x06d9
    #x06da
    #x06db
    #x06dc
    #x06dd
    #x06de
    #x06df
    #x06e0
    #x06e1
    #x06e2
    #x06e3
    #x06e4
    #x06e7
    #x06e8
    #x06ea
    #x06eb
    #x06ec
    #x06ed
    #x0901	;devanagari
    #x0902
    #x0903
    #x093c
    #x093e
    #x093f
    #x0940
    #x0941
    #x0942
    #x0943
    #x0944
    #x0945
    #x0946
    #x0947
    #x0948
    #x0949
    #x094a
    #x094b
    #x094c
    #x094d
    #x0951
    #x0952
    #x0953
    #x0954
    #x0962
    #x0963
    #x0b82	;tamil
    #x0b83
    #x0bbe
    #x0bbf
    #x0bc0
    #x0bc1
    #x0bc2
    #x0bc6
    #x0bc7
    #x0bc8
    #x0bca
    #x0bcb
    #x0bcc
    #x0bcd
    #x0bd7
    #x0e31	;thai
    #x0e34
    #x0e35
    #x0e36
    #x0e37
    #x0e38
    #x0e39
    #x0e3a
    #x0e47
    #x0e48
    #x0e49
    #x0e4a
    #x0e4b
    #x0e4c
    #x0e4d
    #x0e4e
    #x20d0	;symbol diacritics
    #x20d1
    #x20d2
    #x20d3
    #x20d4
    #x20d5
    #x20d6
    #x20d7
    #x20d8
    #x20d9
    #x20da
    #x20db
    #x20dc
    #x20dd
    #x20de
    #x20df
    #x20e0
    #x20e1
    #x3099	;kana
    #x309a
    #xfb1e	;compatability
    #xfe20
    #xfe21
    #xfe22
    #xfe23))

(define *nsm-table*
  (let ((nt (make-hbb)))
    (letrec ((loop
	      (lambda (nl)
		(if (not (null? nl))
		    (let ((ne (car nl)))
		      (hbb/put! nt ne ne)
		      (loop (cdr nl)))))))
      (loop *nsm-table-data*)
      nt)))

(define nsm?
  (lambda (ce)
    (hbb/get *nsm-table* ce
	     (lambda (k1 k2)
	       (cond
		((< k1 k2) -1)
		((> k1 k2)  1)
		(else 0)))
	     (lambda (v)
	       v)
	     (lambda ()
	       #f))))
