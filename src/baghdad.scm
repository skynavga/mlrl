
;
; Baghdad Font Metric Extensions
;

(define *baghdad-unicode-to-glyph-code-map*
  (lookup-table/compile
    'segment-single
    '((#x0020 #x0020 (- 1))	   ;space
      (#x0021 #x0029 (+ 127))	   ;ascii symbols and punctuation
      (#x002A #x002A (+ 149))	   ;asterisk
      (#x002B #x003F (+ 127))	   ;ascii symbols, punctuation, and digits
      (#x005B #x005F (+ 127))	   ;ascii symbols and punctuation
      (#x007B #x007D (+ 127))	   ;ascii symbols and punctuation
      (#x00AB #x00AB (- 20))	   ;mirrored left double guillemet
      (#x00BB #x00BB (- 48))	   ;mirrored right double guillemet
      (#x00D7 #x00D7 (- 46))	   ;multiplication sign
      (#x00F7 #x00F7 (- 93))	   ;division sign
      (#x0600 #x0652 (- 1377))	   ;primary arabic letters
      (#x0660 #x0669 (- 1456))	   ;arabic-indic digits
      (#x066A #x066A (- 1478))	   ;arabic percent sign
      (#x066B #x066C (- 1553))	   ;arabic decimal & thousands separators
      (#x066D #x066D (- 1454))	   ;arabic five pointed star
      (#x2026 #x2026 (- 8084)))))   ;horizontal ellipsis

;
; The following data was extracted from the Cursive Connection Subtable
; of the MORT sfnt table of the Baghdad TT GX font.
;

(define *baghdad-cursive-connection-map*
  (state-table/compile
    'contextual-substitution
    '( #xC1 .					; glyph class table
       ( 8 8 8 8 7 8 7 8 7 7 7 7 7 8 8 8
	 8 7 7 7 7 7 7 7 7 1 1 1 1 1 5 7
	 7 7 7 7 7 7 8 7 7 4 4 4 4 4 4 4 4 ))
    '(( 0  0  0  0  0  1  3  2  0 )		; state -> action table
      ( 0  0  0  0  0  1  3  2  0 )
      ( 0  0  0  0  1  1  6  5  4 )
      ( 0  0  0  0  7  8  3 10  9 )
      ( 0  0  0  0 11 12  3 14 13 )
      ( 0  0  0  0 15  8  3 10  9 ))
    0						; initial state
    '(( 0 ()       0   0)			; action table
      ( 2 ()       0   0)
      ( 3 (mark)   0   0)
      ( 5 (mark)   0   0)
      ( 0 ()       0  77)
      ( 4 (mark)   0  77)
      ( 5 (mark)  76   7)
      ( 3 ()       0   0)
      ( 2 ()     -74   0)
      ( 0 ()     -74  77)
      ( 4 (mark) -74  77)
      ( 4 ()       0   0)
      ( 2 ()      77   0)
      ( 0 ()      77  77)
      ( 4 (mark)  77  77)
      ( 5 ()       0   0))
    (/ 246 2)					;subst table offset
    '( #x0079					;subst tabl
       #xffff
       #x0002
       #xffff
       #x0003
       #x0004
       #x0005
       #x0006
       #x0007
       #xffff
       #xffff
       #xffff
       #xffff
       #x0008
       #x0009
       #x007e
       #x007b
       #x000b
       #x000c
       #x000d
       #x000e
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #x000f
       #x0010
       #x0011
       #x0012
       #x0013
       #x0014
       #x0015
       #xffff
       #x007b
       #x0016
       #x0017
       #xffff
       #x0018
       #xffff
       #x0019
       #x001a
       #x001b
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #x001c
       #x001d
       #xffff
       #xffff
       #xffff
       #xffff
       #x001e
       #x0040
       #x0041
       #x0042
       #x0043
       #x0044
       #x0045
       #x0046
       #x0047
       #x0048
       #x0049
       #x004a
       #x004b
       #x004c
       #x004d
       #xffff
       #x007c
       #x004e
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #x004f
       #x0050
       #x0051
       #x0052
       #x0053
       #x0054
       #x0055
       #x0056
       #x0057
       #x0058
       #x0059
       #x005f
       #x0060
       #x0061
       #x0062
       #x0063
       #x0064
       #x0065
       #x0066
       #x0067
       #x0068
       #x0069
       #x006a
       #x006b
       #x006c
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #xffff
       #x006d
       #x006e
       #x006f
       #x0070
       #x0071
       #x0072
       #x0073
       #x0074
       #x0075
       #x0076
       #xffff
       #x1000
       #x4001
       #x0000
       #x0002 )))

(define *baghdad-glyph-code-to-postscript-encoding-map*
  (lookup-table/compile
    'segment-single
    '((  2   9 (- 1))
      ( 13 254 (+ 1)))))

(define *baghdad-extended-metrics*
  `((pre-map . ,*baghdad-unicode-to-glyph-code-map*)
    (context-maps . (,*baghdad-cursive-connection-map*))
    (post-map . ,*baghdad-glyph-code-to-postscript-encoding-map*)))
