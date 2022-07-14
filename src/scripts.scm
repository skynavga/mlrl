
;
; Pre-Defined Scripts (partial definitions only)
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(define *latin-script*
  '(latin 
     ((:type . :primary)
      (:independent-classes . (letter)))
     (letter
       (a b c d e f g h i j k l m n o p q r s t u v w x y z))))

(register-script *latin-script*)

(define *cyrillic-script*
  '(cyrillic
     ((:type . :primary)
      (:independent-classes . (letter)))
     (letter
       (a be ve ge de ie io zhe ze i short-i ka el em en o pe er es te u
	ef ha tse che sha shcha hard-sign yeru soft-sign e yu ya))))

(register-script *cyrillic-script*)

(define *greek-script*
  '(greek
     ((:type . :primary)
      (:independent-classes . (letter punctuation))
      (:bound-classes . (diacritic)))
     (letter
       (alpha beta gamma delta epsilon zeta eta theta iota kappa lamda
	mu nu xi omicron pi rho sigma tau upsilon phi chi psi omega))
     (punctuation
       (dexia-keraia aristeri-keraia erotimatiko))
     (diacritic
       (varia			; grave
	oxia			; acute
	dialytika		; diaeresis
	tonos			; vertical line above
	psili			; smooth breathing (comma above)
	dasia			; rough breathing (reverse comma above)
	perispomeni		; tilde
	ypogegrammeni))))	; subscript iota

(register-script *greek-script*)

