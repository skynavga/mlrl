
;
; Generic Graphics Device Interfaces
;

(declare (usual-integrations))

(define *device-type*
  (make-record-type "device" '(type system-device font-catalog)))

(define make-device
  (record-constructor *device-type*))

(define device/type
  (record-accessor *device-type* 'type))

(define device/set-type!
  (record-updater *device-type* 'type))

(define device/system-device
  (record-accessor *device-type* 'system-device))

(define device/set-system-device!
  (record-updater *device-type* 'system-device))

(define (device/open type . open-args)
  (case type
    ((postscript)
     (make-device 'postscript (apply ps/open open-args) '()))
    (else
     (error "Unknown graphics device type:" type))))

(define (device/dispatch gd . args)
  (case (device/type gd)
    ((postscript)
     (apply ps/dispatch (cons (device/system-device gd) args)))
    (else
     (error "No dispatcher available for:" (device/type gd)))))

(define (device/close gd)
  (device/dispatch gd 'close))

(define (device/push-xfrm! gd xfrm)
  (device/dispatch gd 'push-xfrm! xfrm))

(define (device/pop-xfrm! gd)
  (device/dispatch gd 'pop-xfrm!))

(define (device/erase gd)
  (device/clear-rectangle gd (device/bbox gd)))

(define (device/bbox gd)
  (device/dispatch gd 'bbox))

(define (device/set-point! gd pt)
  (device/dispatch gd 'set-point! pt))

(define (device/set-point-relative! gd pt)
  (device/dispatch gd 'set-point-relative! pt))

(define (device/set-color! gd r g b)
  (device/dispatch gd 'set-color! r g b))

(define (device/set-line-style! gd style)
  (device/dispatch gd 'set-line-style! style))

(define (device/draw-rectangle gd rect)
  (device/dispatch gd 'draw-rectangle rect))

(define (device/clear-rectangle gd rect)
  (device/dispatch gd 'clear-rectangle rect))

(define (device/draw-crosshair gd pt len)
  (device/dispatch gd 'draw-crosshair pt len))

(define (device/draw-grid gd rect grid-units)
  (device/dispatch gd 'draw-grid rect grid-units))

(define (device/set-font! gd font-instance)
  (device/dispatch gd 'set-font! font-instance))

(define (device/download-prologue gd)
  (device/dispatch gd 'download-prologue))

(define (device/download-epilogue gd)
  (device/dispatch gd 'download-epilogue))

(define (device/font-ascent gd font-instance)
  (device/dispatch gd 'font-ascent font-instance))

(define (device/font-descent gd font-instance)
  (device/dispatch gd 'font-descent font-instance))

(define (device/font-glyph-metrics gd fi gc #!optional gm)
  (if (default-object? gm)
      (set! gm (make-glyph-metrics)))
  (device/dispatch gd 'font-glyph-metrics fi gc gm))

(define (device/show-glyphs! gd glyphs #!optional origin)
  (if (default-object? origin)
      (device/dispatch gd 'show-glyphs! glyphs)
      (device/dispatch gd 'show-glyphs! glyphs origin)))

(define (device/show-glyph-string! gd glyph-string #!optional origin)
  (if (default-object? origin)
      (device/dispatch gd 'show-glyph-string! glyph-string)
      (device/dispatch gd 'show-glyph-string! glyph-string origin)))

;
; Add equipment to read extended metrics before creating font.
;

(define (device/load-font-catalog gd)
  (let ((fc (make-font-catalog)))
    (let load ((dfl (device/dispatch gd 'fonts)))
      (if (not (null? dfl))
	  (let ((f (apply make-font (cons gd (car dfl)))))
	    (font-catalog/extend! fc f)
	    (load (cdr dfl)))))
    fc))

(define (device/font-catalog gd)
  (let ((font-catalog (record-accessor *device-type* 'font-catalog))
	(set-font-catalog! (record-updater *device-type* 'font-catalog)))
    (or (font-catalog gd)
	(let ((fc (device/load-font-catalog gd)))
	  (set-font-catalog! gd fc)
	  fc))))

  