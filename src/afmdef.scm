
;
; AFM Object Definitions
;

(define *afm-cache* '())
(define *afm-cache-version* 1.0)
(define *afm-header-size* 12)
(define *afm-entry-size* 64)
(define *default-afm-cache-file-name* "./.afmcache")
(define *default-afm-directories* '("./afm/*.afm"))

(define-record afm-cache (file-name afm-table))

(define-record afm
  (font-name
   full-name
   family-name
   weight
   character-set
   encoding-scheme
   ascender
   descender
   cap-height
   x-height
   font-bbox
   underline-position
   underline-thickness
   italic-angle
   fixed-pitch?
   screen-font?
   screen-font-size
   first-glyph
   glyphs
   glyphs-loaded?
   glyph-table-offset
   ))

(define (make-afm)
  ((record-constructor (record-type afm))
    ""			;font-name
    ""			;full-name
    ""			;family-name
    ""			;weight
    ""			;character-set
    ""			;encoding-scheme
    0			;ascender
    0			;descender
    0			;cap-height
    0			;x-height
    (make-empty-rect)	;font-bbox
    0			;underline-position
    0			;underline-thickness
    0			;italic-angle
    '()			;fixed-pitch?
    '()			;screen-font?
    0			;screen-font-size
    '()			;first-glyph
    '()			;glyphs
    '()			;glyphs-loaded?
    '()))		;glyph-table-offset

(define-record afm-glyph (number name widths bbox vvector ligatures))

(define (make-afm-glyph)
  ((record-constructor (record-type afm-glyph))
     '() '() (make-vector 2) '() '() '()))
