
(define *fatiha*
  #( #x0633	;seen
     #x0648	;wa
     #x0631	;reh
     #x0629	;ta marbuta
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0641	;feh
     #x0627	;alef
     #x062a	;teh
     #x062d	;hah
     #x0629	;ta marbuta
     #x2028	;-------------
     #x0628	;beh
     #x0633	;seen
     #x0645	;meem
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0644	;lam
     #x0647	;heh
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0631	;reh
     #x062d	;hah
     #x0645	;meem
     #x0627	;alef
     #x0646	;noon
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0631	;reh
     #x062d	;hah
     #x064a	;yeh
     #x0645	;meem
     #x2029     ;=============
   ))

(define *fatiha-voweled*
  #( #x0633	;seen
     #x064f	;damma
     #x0648	;wa
     #x0631	;reh
     #x064e	;fatha
     #x0629	;ta marbuta
     #x064f	;damma
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0652	;sukun
     #x0641	;feh
     #x064e	;fatha
     #x0627	;alef
     #x062a	;teh
     #x0650	;kasra
     #x062d	;hah
     #x064e	;fatha
     #x0629	;ta marbuta
     #x2028	;-------------
     #x0628	;beh
     #x0650	;kasra
     #x0633	;seen
     #x0652	;sukun
     #x0645	;meem
     #x0650	;kasra
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0644	;lam
     #x0651	;shadda
     #x0647	;heh
     #x0650	;kasra
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0631	;reh
     #x0651	;shadda
     #x064e	;fatha
     #x062d	;hah
     #x0652	;sukun
     #x0645	;meem
     #x064e	;fatha
     #x0627	;alef
     #x0646	;noon
     #x0650	;kasra
     #x0020	;
     #x0627	;alef (al-wasl)		;should be #x0671
     #x0644	;lam
     #x0631	;reh
     #x0651	;shadda
     #x064e	;fatha
     #x062d	;hah
     #x0650	;kasra
     #x064a	;yeh
     #x0645	;meem
     #x2029     ;=============
   ))

(with-output-to-file "fatiha.utf"
  (lambda ()
    (write-string (ucs->utf *fatiha*))))

(with-output-to-file "fatihav.utf"
  (lambda ()
    (write-string (ucs->utf *fatiha-voweled*))))
