
;
; Font Functions
;

#|
The following need to be rewritten rather badly; or, actually,
the caller of this needs to use a different function.  That is,
the following information can only be obtained from a font-instance,
and not a font, since the latter doesn't know what size or style
instance it is, and, therefore, can't determine its metrics.

(define (font/font-units-per-point f)
  (device-font/font-units-per-point (font/device-font f)))
  
(define (font/ascent f)
  (let ((s (/ (font-spec/size (font/font-spec f))
	      (font/font-units-per-point f))))
    (* (device-font/ascent (font/device-font f)) s)))


(define (font/descent f)
  (let ((s (/ (font-spec/size (font/font-spec f))
	       (font/font-units-per-point f))))
    (* (device-font/descent (font/device-font f)) s)))

(define (font/glyph-metrics f gn)
  (let ((s (/ (font-spec/size (font/font-spec f))
	      (font/font-units-per-point f)))
	(gm (postscript-font/glyph-metrics (font/device-font f) gn)))
    (make-glyph-metrics
      (point/scale (glyph-metrics/escapement gm) s s)
      (rect/scale (glyph-metrics/bbox gm) s s)
      (glyph-metrics/properties gm))))
|#

;
; Font Set Functions
;

;
; Font Catalog
;
