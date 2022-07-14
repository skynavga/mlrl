
;
; Content Stream
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(define-record content-stream
  ( first-content-unit			; head of CUs linked list
    last-content-unit			; tail of CUs linked list
    last-formatted-content-unit		; last formatted content unit
    first-column			; head of content stream's column list
    last-column				; tail of content stream's column list
  ))

(define (make-content-stream)
  ((record-constructor (record-type content-stream)) '() '() '() '() '()))

(define (content-stream/end-of-stream? cs)
  (let ((lfcu (content-stream/last-formatted-content-unit cs)))
    (and lfcu (null? (content-unit/next lfcu)))))

(define (content-stream/insert! cs new-cu #!optional cu)
  (if (default-object? cu)
      (set! cu (content-stream/last-content-unit cs)))
  (if (not (null? cu))
      (begin
	(content-unit/set-next! new-cu (content-unit/next cu))
	(content-unit/set-next! cu new-cu)))
  (content-unit/set-previous! new-cu cu)
  (if (null? (content-stream/first-content-unit cs))
      (content-stream/set-first-content-unit! cs new-cu))
  (let ((lcu (content-stream/last-content-unit cs)))
    (if (or (null? lcu)
	    (eq? cu lcu))
	(content-stream/set-last-content-unit! cs new-cu)))
  unspecific)

(define (content-stream/delete! cs cu)
  (let ((prev (content-unit/previous cu))
	(next (content-unit/next cu)))
    (if (not (null? prev))
	(content-unit/set-next! prev next))
    (if (eq? (content-stream/first-content-unit cs) cu)
	(content-stream/set-first-content-unit! cs next))
    (if (eq? (content-stream/last-content-unit cs) cu)
	(content-stream/set-last-content-unit! cs prev))))

;
; Format content stream into given column.  If this is the first
; column of this stream, then start from the beginning; otherwise,
; find last content unit of previous column and start where that
; one left off.
;
; Once finished formatting, add this column to this stream's list
; of columns and update the last column register.
;
; Note:  Why do I need a list of columns for this stream?  In order
; to find formatted content given some content interval.  Is this the
; right place to associate this data?
;
; A column formatting object serves to maintain state data which must
; persist across multiple calls to column-formatting/format; e.g., the
; active baseline point in the column.
;

(define (content-stream/format cs c)
  (let ((cf (make-column-formatting c)))
    ;
    ; flow content units starting with specific content unit into column
    ; until column formatter returns #f (in which case it hit the end of
    ; the column), or until we run out of content units to flow. record
    ; the last content unit formatted each time through the loop.
    ;
    (define (flow cu)
      (if (not (null? cu))
	  (begin
	    (if (not (column-formatting/format cf cu))
		(content-stream/set-last-formatted-content-unit! cs cu)
		(flow (content-unit/next cu))))))
    ;
    ; link column into content stream column list
    ;
    (if (null? (content-stream/first-column cs))
	(content-stream/set-first-column! cs c))
    (let ((lc (content-stream/last-column cs)))
      (column/set-previous-content! c lc)
      (if (not (null? lc))
	  (column/set-next-content! lc c))
      (content-stream/set-last-column! cs c))
    ;
    ; if previously flowed content, then start flowing where it left off
    ; otherwise, flow first content unit of stream
    ;
    (let ((lfcu (content-stream/last-formatted-content-unit cs)))
      (if (not (null? lfcu))
	  (flow lfcu)
	  (let ((fcu (content-stream/first-content-unit cs)))
	    (if (not (null? fcu))
		(flow fcu)))))))
