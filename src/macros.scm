
;
; Macrology
;

(syntax-table-define system-global-syntax-table 'device/with-xfrm
  (macro (gd xfrm . body)
    `(DYNAMIC-WIND
       (LAMBDA ()
	 (DEVICE/PUSH-XFRM! ,gd ,xfrm))
       (LAMBDA ()
	 ,@body)
       (LAMBDA ()
	 (DEVICE/POP-XFRM! ,gd)))))

(syntax-table-define system-global-syntax-table 'device/with-origin
  (macro (gd origin . body)
    `(DYNAMIC-WIND
       (LAMBDA ()
	 (DEVICE/PUSH-XFRM! ,gd
	   (XFRM/TRANSLATE (MAKE-IDENTITY-XFRM) ,origin)))
       (LAMBDA ()
	 ,@body)
       (LAMBDA ()
	 (DEVICE/POP-XFRM! ,gd)))))
    
(syntax-table-define system-global-syntax-table 'device/with-scale
  (macro (gd sx sy . body)
    `(DYNAMIC-WIND
       (LAMBDA ()
	 (DEVICE/PUSH-XFRM! ,gd
	   (XFRM/SCALE (MAKE-IDENTITY-XFRM) ,sx ,sy)))
       (LAMBDA ()
	 ,@body)
       (LAMBDA ()
	 (DEVICE/POP-XFRM! ,gd)))))

