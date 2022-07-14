;
; TeX Style Paragraph Formatting
;
; Copyright 1991, Metis Technology, Inc.
; Redistribution and use in source and binary forms, with or without
; modification, are permitted without restriction.
;
; The following formatting algorithm was lifted directly from TeX Version 3.14.
; Although the basic algorithm is identical to Knuth's, the implementation is
; slightly different in order to eliminate a number of global variables
; employed by TeX.  The input to the line breaker is in the form of a glyph
; instruction stream containing box, glue, penalty, disc, and kern elements.
; Also, this implementation is not built for speed. For example, it employs
; floating point arithmetic rather than fixed point; furthermore, it uses an
; exact (but slow) rather than approximate (but fast) algorithm to compute
; badness.
;
; The structure of the main algorithm below is implemented using proper tail
; recursion which compiles into iterative behavior.
;
; Author: Glenn Adams <glenn@metis.com>
;

(declare (usual-integrations))

;
; Constants
;

(define *awful-badness*                 (-1+ (expt 2 30)))
(define *infinite-badness*              10000)
(define *infinite-penalty*              *infinite-badness*)
(define *eject-penalty*                 (- *infinite-penalty*))
(define *max-line*                      10000)

(define *very-loose-fit*                0)
(define *loose-fit*                     1)
(define *decent-fit*                    2)
(define *tight-fit*                     3)

;
; Default Parameters
;

(define *pretolerance*                  100)
(define *tolerance*                     200)

(define *line-penalty*                  10)
(define *hyphen-penalty*                50)
(define *explicit-hyphen-penalty*       50)

(define *double-hyphen-demerits*        10000)
(define *final-hyphen-demerits*         5000)
(define *adjacency-demerits*            10000)

(define *par-fill-skip*                 '(0. 1. fil    0. normal))
(define *left-skip*                     '(0. 0. normal 0. normal))
(define *right-skip*                    '(0. 0. normal 0. normal))

;
; Debugging
;

(define *debugging*                     '())
(define *deactivate-debugging*          '())
(define *tracing-paragraphs*            '())
(define *printed-position*              '())

;
; Object Definitions
;

(define-record passive
  ( position             ; glyph stream position corresponding to this break
    previous             ; previous passive node for optimal breakpoint
    serial               ; passive node creation count
  ))

(define-record active
  ( break                ; this breakpoint's passive node
    line                 ; line number of line after this breakpoint
    fitness              ; fitness class of line ending at this breakpoint
    hyphenated?          ; #t if node is bound to a discrectionary break
    demerits             ; total demerits of lines from para start to here
  ))

(define-record delta
  ( nominal              ; nominal measure
    stretch              ; stretch factor in points
    stretch-fil          ; stretch factor in fils
    stretch-fill         ; stretch factor in fills
    stretch-filll        ; stretch factor in fillls
    shrink               ; shrink factor
  ))

(define (make-delta)
  ((record-constructor (record-type delta)) 0 0 0 0 0 0))

(define-record break-state
  ( stream               ; glyph stream
    active               ; head of active breakpoint list
    passive              ; head of passive breakpoint list
    active-deltas        ; deltas from first active node to current position
    current-active-deltas; deltas from current active node to current position
    background-deltas    ; empty line deltas
    break-deltas         ; line break deltas
    minimum-demerits     ; minimum demerit of all classes
    minimal-demerits     ; minimal demerits for each class
    best-positions       ; positions at which minimal demerits occurred
    best-position-lines  ; lines in which minimal demerits occurred
    easy-line            ; equivalent break nodes after this line
    last-special-line    ; constant line width after this line
    first-length         ; length of lines up to and incl. last special line
    first-indent         ; indentation for first line(s)
    second-length        ; length of lines after last special line
    second-indent        ; indentation of lines after last special line
    paragraph-shape      ; vector of line margins/lengths for special shapes
    threshold            ; threshold for line badness
    passive-number       ; number of passive nodes created in this pass
    best-line            ; line number following last line of paragraph
  ))

(define (make-break-state)
  ((record-constructor (record-type break-state))
   '()                   ; stream
   '()                   ; active
   '()                   ; passive
   (make-delta)          ; active-deltas
   (make-delta)          ; current-active-deltas
   (make-delta)          ; background-deltas
   (make-delta)          ; break-deltas
   *awful-badness*       ; minimum-demerits
   (make-vector 4
     *awful-badness*)    ; minimal-demerits
   (make-vector 4 '())   ; best-positions
   (make-vector 4 '())   ; best-position-lines
   '()                   ; easy-line
   '()                   ; last-special-line
   '()                   ; first-length
   '()                   ; first-indent
   '()                   ; second-length
   '()                   ; second-indent
   '()                   ; paragraph-shape
   '()                   ; threshold
   0                     ; passive-number
   '()                   ; best-line
  ))

;
; Glue Specification
;

(define-record glue-spec (nominal stretch stretch-order shrink shrink-order)) 

(define (glue-spec/hash-mod gsk k)
  (remainder
   (inexact->exact
    (truncate
     (+ (* (list-ref gsk 0) 101.0)
        (* (list-ref gsk 1) 7201.0)
        (* (list-ref gsk 3) (* 7201.0 7201.0)))))
   k))

(define (glue-spec/equal? gs1 gs2)
  (and (= (list-ref gs1 0) (list-ref gs2 0))
       (= (list-ref gs1 1) (list-ref gs2 1))
       (eq? (list-ref gs1 2) (list-ref gs2 2))
       (= (list-ref gs1 3) (list-ref gs2 3))
       (eq? (list-ref gs1 4) (list-ref gs2 4))))

(define make-glue-spec-hash-table
  (hash-table/constructor
    glue-spec/hash-mod glue-spec/equal? cons #t car cdr set-cdr!))

(define *glue-spec-table* (make-glue-spec-hash-table))

(define (find-glue-spec nom str str-ord shr shr-ord)
  (let* ((gsk (list nom str str-ord shr shr-ord))
         (gs (hash-table/get *glue-spec-table* gsk '())))
    (if (null? gs)
        (begin
          (set! gs (apply make-glue-spec gsk))
          (hash-table/put! *glue-spec-table* gsk gs)))
    gs))

;
; Accessors for TeX Glyph Instructions
;

(define (box/measure gi)
  (caddr (gi/rands gi)))

(define (glue/nominal gi)
  (let ((gs (cadr (gi/rands gi))))
    (glue-spec/nominal gs)))

(define (glue/stretch gi)
  (let ((gs (cadr (gi/rands gi))))
    (glue-spec/stretch gs)))

(define (glue/stretch-order gi)
  (let ((gs (cadr (gi/rands gi))))
    (glue-spec/stretch-order gs)))

(define (glue/shrink gi)
  (let ((gs (cadr (gi/rands gi))))
    (glue-spec/shrink gs)))

(define (glue/shrink-order gi)
  (let ((gs (cadr (gi/rands gi))))
    (glue-spec/shrink-order gs)))

(define (penalty/penalty gi)
  (cadr (gi/rands gi)))

(define (kern/measure gi)
  (cadr (gi/rands gi)))

(define (disc/measure gi)
  (list-ref (gi/rands gi) 2))

(define (disc/pre-measure gi)
  (list-ref (gi/rands gi) 4))

(define (disc/post-measure gi)
  (list-ref (gi/rands gi) 6))

;
; Nodes on active & passive lists are composed with the following.
;

(define (make-node value)
  (cons value '()))

(define (free-node n)
  n ; ignore
  unspecific)

(define node/value car)
(define node/set-value! set-car!)
(define node/next cdr)
(define node/set-next! set-cdr!)

(define (make-active-node . rest)
  (make-node (apply make-active rest)))

(define (make-passive-node . rest)
  (make-node (apply make-passive rest)))

(define (make-delta-node)
  (make-node (make-delta)))

;
; The following functions depend on delta structures being
; represented as named records (i.e., vectors whose first element
; refers to a record description).
;

(define (delta/copy ds dr)
  (vector-set! dr 1 (vector-ref ds 1))
  (vector-set! dr 2 (vector-ref ds 2))
  (vector-set! dr 3 (vector-ref ds 3))
  (vector-set! dr 4 (vector-ref ds 4))
  (vector-set! dr 5 (vector-ref ds 5))
  (vector-set! dr 6 (vector-ref ds 6)))

(define (delta/add d1 d2 dr)
  (vector-set! dr 1 (+ (vector-ref d1 1) (vector-ref d2 1)))
  (vector-set! dr 2 (+ (vector-ref d1 2) (vector-ref d2 2)))
  (vector-set! dr 3 (+ (vector-ref d1 3) (vector-ref d2 3)))
  (vector-set! dr 4 (+ (vector-ref d1 4) (vector-ref d2 4)))
  (vector-set! dr 5 (+ (vector-ref d1 5) (vector-ref d2 5)))
  (vector-set! dr 6 (+ (vector-ref d1 6) (vector-ref d2 6))))
  
(define (delta/sub d1 d2 dr)
  (vector-set! dr 1 (- (vector-ref d1 1) (vector-ref d2 1)))
  (vector-set! dr 2 (- (vector-ref d1 2) (vector-ref d2 2)))
  (vector-set! dr 3 (- (vector-ref d1 3) (vector-ref d2 3)))
  (vector-set! dr 4 (- (vector-ref d1 4) (vector-ref d2 4)))
  (vector-set! dr 5 (- (vector-ref d1 5) (vector-ref d2 5)))
  (vector-set! dr 6 (- (vector-ref d1 6) (vector-ref d2 6))))

(define (delta/incr d di)
  (vector-set! d 1 (+ (vector-ref d 1) (vector-ref di 1)))
  (vector-set! d 2 (+ (vector-ref d 2) (vector-ref di 2)))
  (vector-set! d 3 (+ (vector-ref d 3) (vector-ref di 3)))
  (vector-set! d 4 (+ (vector-ref d 4) (vector-ref di 4)))
  (vector-set! d 5 (+ (vector-ref d 5) (vector-ref di 5)))
  (vector-set! d 6 (+ (vector-ref d 6) (vector-ref di 6))))
  
(define (delta/decr d dd)
  (vector-set! d 1 (+ (vector-ref d 1) (vector-ref dd 1)))
  (vector-set! d 2 (+ (vector-ref d 2) (vector-ref dd 2)))
  (vector-set! d 3 (+ (vector-ref d 3) (vector-ref dd 3)))
  (vector-set! d 4 (+ (vector-ref d 4) (vector-ref dd 4)))
  (vector-set! d 5 (+ (vector-ref d 5) (vector-ref dd 5)))
  (vector-set! d 6 (+ (vector-ref d 6) (vector-ref dd 6))))
  
;
; Compute background deltas for a paragraph's lines.
;

(define (tex/set-background! bs left-skip right-skip)
  (let ((bkd (break-state/background-deltas bs)))
    (delta/set-nominal! bkd
      (+ (glue-spec/nominal left-skip) (glue-spec/nominal right-skip)))
    (case (glue-spec/stretch-order left-skip)
      ((normal)
       (delta/set-stretch! bkd (glue-spec/stretch left-skip)))
      ((fil)
       (delta/set-stretch-fil! bkd (glue-spec/stretch left-skip)))
      ((fill)
       (delta/set-stretch-fill! bkd (glue-spec/stretch left-skip)))
      ((filll)
       (delta/set-stretch-filll! bkd (glue-spec/stretch left-skip))))
    (case (glue-spec/stretch-order right-skip)
      ((normal)
       (delta/set-stretch! bkd
         (+ (delta/stretch bkd) (glue-spec/stretch right-skip))))
      ((fil)
       (delta/set-stretch-fil! bkd
         (+ (delta/stretch-fil bkd) (glue-spec/stretch right-skip))))
      ((fill)
       (delta/set-stretch-fill! bkd
         (+ (delta/stretch-fill bkd) (glue-spec/stretch right-skip))))
      ((filll)
       (delta/set-stretch-filll! bkd
         (+ (delta/stretch-filll bkd) (glue-spec/stretch right-skip)))))
    (delta/set-shrink! bkd
      (+ (glue-spec/shrink left-skip) (glue-spec/shrink right-skip)))))

;
; Compute paragraph's line state parameters from geometry. 
;

(define (tex/set-geometry! bs hang-after hang-indent hsize par-shape looseness)
  (if (null? par-shape)
      (if (zero? hang-indent)
          (begin
            (break-state/set-last-special-line! bs 0)
            (break-state/set-second-length! bs hsize)
            (break-state/set-second-indent! bs 0))
          (begin
            (break-state/set-last-special-line! bs (abs hang-after))
            (if (negative? hang-after)
                (begin
                  (break-state/set-first-length! bs (- hsize (abs hang-indent)))
                  (break-state/set-first-indent! bs
                    (if (not (negative? hang-indent)) hang-indent 0))
                  (break-state/set-second-length! bs hsize)
                  (break-state/set-second-indent! bs 0))
                (begin
                  (break-state/set-first-length! bs hsize)
                  (break-state/set-first-indent! bs 0)
                  (break-state/set-second-length! bs (- hsize (abs hang-indent)))
                  (break-state/set-second-indent! bs hang-indent
                    (if (not (negative? hang-indent)) hang-indent 0))))))
      (let* ((ls (- (vector-length par-shape) 1))
             (ll (vector-ref par-shape ls)))
        (break-state/set-last-special-line! bs ls)
        (break-state/set-second-indent! bs (car ll))
        (break-state/set-second-length! bs (cdr ll))))
  (if (zero? looseness)
      (break-state/set-easy-line! bs (break-state/last-special-line bs))
      (break-state/set-easy-line! bs *max-line*)))

;
; Remove any glue at end of stream, then append a \parfillskip
; preceded by an infinite penalty (to prevent break before skip).
; The purpose of \parfillskip is to provide the glue needed to
; fill out the last line of the paragraph.
;

(define (tex/prepare-stream! tgs parfillskip)
  (define (tail)
    (let ((k (glyph-stream/fill tgs)))
      (if (zero? k)
          '()
          (glyph-stream/read-at tgs (-1+ k)))))
  (let ((gi (tail)))
    (if (eq? (gi/rator gi) 'box)
        (glyph-stream/write tgs
          (make-gi 'penalty (list '* *infinite-penalty*)))
        (if (not (eq? (gi/rator gi) 'glue))
            (glyph-stream/write tgs
              (make-gi 'penalty (list '* *infinite-penalty*)))
            (begin
              (gi/set-rator! gi 'penalty)
              (gi/set-rands! gi (list '* *infinite-penalty*)))))
    (glyph-stream/write tgs (make-gi 'glue (list '* parfillskip)))
    unspecific))

;
; Fixup input stream then establish in break state.
;

(define (tex/set-stream! bs tgs par-fill-skip)
  (tex/prepare-stream! tgs par-fill-skip)
  (break-state/set-stream! bs tgs))

;
; Compute badness using exact computation rather than approximation
; as done in TeX.  We aren't using scaled fixed point here.
;

(define (badness t s)
  (if (zero? t)
      0
      (if (not (positive? s))
          *infinite-badness*
          (let ((b (* 100 (expt (/ t s) 3))))
            (if (> b *infinite-badness*)
                *infinite-badness*
                (inexact->exact (truncate b)))))))

;
; Deactivate node r, absorbing adjacent delta nodes as necessary.
; Returns list containing next active node, next previous active node
; and next previous, previous active node values to be used in main
; loop of tex/try-break.
;

(define (tex/deactivate bs r pr ppr ad cad)
  (call/cc
    (lambda (exit)
      (let ((active (break-state/active bs)))
        (if *deactivate-debugging*
            (let ((pq (active/break (node/value r))))
              (format #t "~%**~A"
                      (if (null? pq) 0 (passive/serial (node/value pq))))))
        (if (eq? r (node/next active))
            (begin
              (set! r (node/next r))
              (if (delta? (node/value r))
                  (begin
                    (delta/incr ad (node/value r))
                    (delta/copy ad cad)
                    (set! r (node/next r))))
              (node/set-next! active r)
              (list r active))
            (begin
              (if (delta? (node/value pr))
                  (let ((nr (node/next r)))
                    (if (eq? nr (break-state/active bs))
                        (begin
                          (delta/decr cad (node/value pr))
                          (node/set-next! ppr '())
                          (exit (list (break-state/active bs) ppr)))
                        (if (delta? (node/value nr))
                            (begin
                              (delta/incr cad (node/value nr))
                              (delta/incr (node/value pr) (node/value nr))
                              (node/set-next! pr (node/next nr))
                              (exit (list (node/next nr) ppr)))))))
              (list (node/next r) r)))))))

    
;
; Consider whether the current position in the paragraph is a feasible
; breakpoint.  If it is, add it to the list of active breakpoints; otherwise,
; it fails to become a breakpoint.
;
; In the process of considering the current position as a feasible breakpoint,
; earlier breakpoints may be deactivated if they can no longer produce feasible
; breakpoints; i.e., because more measure is consumed between an earlier
; breakpoint and the current position than could possibly fit into a line
; containing the current position.  Another way to say this is that any
; active breakpoint which would produce an adjustment ratio at the current
; position less than -1 can no longer produce a feasible breakpoint, so it
; can be discarded.
;
; The current position is considered to be a feasible breakpoint if there
; exists an (already) active breakpoint for which the measure from that
; breakpoint to this position produces an adjustment ratio for the line
; measure under consideration that is greater than or equal to -1.0 and
; less than some maximum threshold value.
;
; The following implementation is already properly tail recursive.
;

(define (tex/try-break! bs threshold penalty discretionary? final-pass?)
  (call/cc
    (lambda (exit)
      (let ((tgs (break-state/stream bs))
            (active (break-state/active bs))
            (last-active (break-state/active bs))
            (ad (break-state/active-deltas bs))
            (cad (break-state/current-active-deltas bs))
            (bkd (break-state/background-deltas bs))
            (bd (break-state/break-deltas bs))
            (td (make-delta))
            (md (break-state/minimum-demerits bs))
            (mdv (break-state/minimal-demerits bs))
            (bpv (break-state/best-positions bs))
            (bplv (break-state/best-position-lines bs))
            (position)                  ; current position in glyph stream
            (rv)                        ; current node's data value
            (ppr '())                   ; next to last node (if last was delta)
            (line)                      ; current line class
            (line-length)               ; current line target length
            (old-line 0)                ; previous line class
            (no-break-yet? #t)          ; #t until break deltas initialized
            (artificial-demerits?)      ; for potential panic situations
            (node-stays-active?)        ; flag to prevent node deactivation
            (shortfall)                 ; missed line measure by this much
            (b)                         ; badness
            (class)                     ; fitness classification
            (d))                        ; demerits

        ;
        ; Validate penalty
        ;
        (if (>= (abs penalty) *infinite-penalty*)
            (if (positive? penalty)
                (exit unspecific)
                (set! penalty *eject-penalty*)))

        ;
        ; Initializations
        ;
        (if (glyph-stream/end-of-stream? tgs)
            (set! position '())
            (set! position (glyph-stream/position tgs)))
        (delta/copy ad cad)

        ;
        ; Inner loop of TeX line breaker.
        ;
        (let loop ((r (node/next (break-state/active bs)))
                   (pr (break-state/active bs)))
          (set! rv (node/value r))

          ;
          ; If this is a delta node, update current active measure & continue.
          ;
          (if (delta? rv)
              (begin
                (delta/incr cad rv)
                (loop (node/next r) r)))

          ;
          ; If a line number class has ended, create new active nodes for
          ; the best feasible breaks in that class; then return if current
          ; node is last node, otherwise compute new line measure.
          ;
          (set! line (active/line rv))
          (if (> line old-line)
              (begin
                (if (and (< md *awful-badness*)
                         (or (not (= old-line (break-state/easy-line bs)))
                             (eq? r last-active)))
                    (begin

                      ;
                      ; If break deltas haven't been computed yet, do so now.
                      ; The break delta for the current position is simply the
                      ; length of an empty line (background deltas) minus the
                      ; width of current position's node (e.g., the width of
                      ; a glue node) and the width of any immediately following
                      ; glue and kern nodes (separated by penalty nodes).
                      ;
                      ; During this process, we perform a look ahead on the
                      ; glyph stream in order to absorb glue and kern nodes.
                      ; The current position of the glyph stream is restored
                      ; after any lookahead.
                      ;
                      ; Break deltas are used to compute the width of new
                      ; delta nodes being inserted along with a new active
                      ; node.
                      ;
                      (if no-break-yet?
                          (begin
                            (set! no-break-yet? #f)
                            (delta/copy bkd bd)
                            (if discretionary?
                                (if (not (null? position))
                                    (begin
                                      (glyph-stream/set-position! tgs
                                        (-1+ (glyph-stream/position tgs)))
                                      (let ((gi (glyph-stream/read tgs)))
                                        (delta/set-nominal! bd
                                          (+ (delta/nominal bd)
                                             (- (disc/measure gi))
                                             (disc/pre-measure gi)
                                             (disc/post-measure gi)))))))
                            (let look-ahead ((gi (glyph-stream/read tgs)))
                              (if (not (glyph-stream/end-of-stream? tgs))
                                  (case (gi/rator gi)
                                    ((glue)
                                     (delta/set-nominal! bd
                                       (- (delta/nominal bd)
                                          (glue/nominal gi)))
                                     (case (glue/stretch-order gi)
                                       ((normal)
                                        (delta/set-stretch! bd
                                          (- (delta/stretch bd)
                                             (glue/stretch gi))))
                                       ((fil)
                                        (delta/set-stretch-fil! bd
                                          (- (delta/stretch-fil bd)
                                             (glue/stretch gi))))
                                       ((fill)
                                        (delta/set-stretch-fill! bd
                                          (- (delta/stretch-fill bd)
                                             (glue/stretch gi))))
                                       ((filll)
                                        (delta/set-stretch-filll! bd
                                          (- (delta/stretch-filll bd)
                                             (glue/stretch gi)))))
                                     (delta/set-shrink! bd
                                          (- (delta/shrink bd)
                                             (glue/shrink gi)))
                                     (look-ahead (glyph-stream/read tgs)))
                                    ((penalty)
                                     (look-ahead (glyph-stream/read tgs)))
                                    ((kern)
                                     (delta/set-nominal! bd
                                       (- (delta/nominal bd)
                                          (kern/measure gi))))
                                     (look-ahead (glyph-stream/read tgs)))))
                            (if (not (null? position))
                                (glyph-stream/set-position! tgs position))))

                      ;
                      ; Insert delta node if not at beginning of active
                      ; list.  If previous node is a delta, then convert it.
                      ;
                      (if (delta? (node/value pr))
                          (delta/decr (node/value pr) (delta/add cad bd td))
                          (if (eq? pr active)
                              (delta/copy bd ad)
                              (let ((q (make-delta-node)))
                                (node/set-next! q r)
                                (delta/sub bd cad (node/value q))
                                (node/set-next! pr q)
                                (set! ppr pr)
                                (set! pr q))))
                      (if (>= (abs *adjacency-demerits*) (- *awful-badness* md))
                          (set! md (-1+ *awful-badness*))
                          (set! md (+ md (abs *adjacency-demerits*))))
                      ;
                      ; Insert new active nodes for each fitness class which
                      ; has fewer or the same demerits as the global optimum
                      ; demerits.
                      ;
                      (let next-class ((k 0))
                        (if (< k 4)
                            (begin
                              (if (<= (vector-ref mdv k) md)
                                  (let* ((pn (1+ (break-state/passive-number bs)))
                                         (pq (make-passive-node
                                               (if (null? position)
                                                   (glyph-stream/position tgs)
                                                   (-1+ position))
                                               (vector-ref bpv k)
                                               pn))
                                         (aq (make-active-node
                                               pq
                                               (1+ (vector-ref bplv k))
                                               k
                                               discretionary?
                                               (vector-ref mdv k))))
                                    (break-state/set-passive-number! bs pn)
                                    (node/set-next! pq
                                      (break-state/passive bs))
                                    (break-state/set-passive! bs pq)
                                    (node/set-next! aq r)
                                    (node/set-next! pr aq)
                                    (set! pr aq)
                                    (if *tracing-paragraphs*
                                        (begin
                                          (format #t "~%@@~A: line ~A.~A"
                                                  (passive/serial (node/value pq))
                                                  (-1+ (active/line (node/value aq)))
                                                  k)
                                          (if (and discretionary?
                                                   (not (null? position)))
                                              (write-char #\-))
                                          (format #t " t=~A -> @@~A"
                                                  (active/demerits (node/value aq))
                                                  (if (null?
                                                        (passive/previous
                                                         (node/value pq)))
                                                      #\0
                                                      (passive/serial
                                                        (node/value
                                                          (passive/previous
                                                            (node/value pq))))))))))
                              (vector-set! mdv k *awful-badness*)
                              (next-class (1+ k)))))
                      (set! md *awful-badness*)
                      ;
                      ; If current active node is not at the end of the
                      ; active list, insert a delta node to follow new active
                      ; nodes which we just inserted.
                      ;
                      (if (not (eq? r last-active))
                          (let ((q (make-delta-node)))
                            (node/set-next! q r)
                            (delta/sub cad bd (node/value q))
                            (node/set-next! pr q)
                            (set! ppr pr)
                            (set! pr q)))))

                ;
                ; If we've exhausted active node list, then update state
                ; data and exit.  N.B. this is the only way out of the inner loop.
                ;
                (if (eq? r last-active)
                    (begin
                      (break-state/set-minimum-demerits! bs md)
                      (exit unspecific)))

                ;
                ; There are more active nodes, start a new line class.
                ;
                (if (> line (break-state/easy-line bs))
                    (begin
                      (set! line-length (break-state/second-length bs))
                      (set! old-line (-1+ *max-line*)))
                    (begin
                      (set! old-line line)
                      (if (> line (break-state/last-special-line bs))
                          (set! line-length (break-state/second-length bs))
                          (if (null? (break-state/paragraph-shape bs))
                              (set! line-length (break-state/first-length bs))
                              (set! line-length
                                (vector-ref (break-state/paragraph-shape bs)
                                            line))))))))

          ;
          ; Compute badness and fitness class for current position w.r.t.
          ; current active node under consideration.
          ;
          (set! artificial-demerits? #f)
          (set! shortfall (- line-length (delta/nominal cad)))
          (if (positive? shortfall)
              (begin
                (if (or (not (zero? (delta/stretch-fil cad)))
                        (not (zero? (delta/stretch-fill cad)))
                        (not (zero? (delta/stretch-filll cad))))
                    (set! b 0)
                    (set! b (badness shortfall (delta/stretch cad))))
                (if (> b 12)
                    (if (> b 99)
                        (set! class *very-loose-fit*)
                        (set! class *loose-fit*))
                    (set! class *decent-fit*)))
              (begin
                (if (> (- shortfall) (delta/shrink cad))
                    (set! b (+ *infinite-badness* 1))
                    (set! b (badness (- shortfall) (delta/shrink cad))))
                (set! class (if (> b 12) *tight-fit* *decent-fit*))))

          
          ;
          ; Consider badness of current position w.r.t. active node
          ; under consideration.  If active node can no longer serve
          ; as a potential boundary, deactivate it.  If badness exceeds
          ; threshold and we aren't forcing this breakpoint, then
          ; continue with inner loop.
          ;
          (if (or (> b *infinite-badness*)
                  (= penalty *eject-penalty*))
              (begin
                (if (and final-pass?
                         (= (break-state/minimum-demerits bs) *awful-badness*)
                         (eq? (node/next r) last-active)
                         (eq? pr active))
                    (set! artificial-demerits? #t)
                    (if (> b threshold)
                        (apply loop (tex/deactivate bs r pr ppr ad cad))))
                (set! node-stays-active? #f))
              (begin
                (if (> b threshold)
                    (loop (node/next r) r))
                (set! node-stays-active? #t)))

          ;
          ; Compute demerits for feasible breakpoint.
          ;
          (if artificial-demerits?
              (set! d 0)
              (begin
                (set! d (+ b *line-penalty*))
                (if (>= (abs d) 10000)
                    (set! d 100000000)
                    (set! d (* d d)))
                (if (not (zero? penalty))
                    (if (positive? penalty)
                        (set! d (+ d (* penalty penalty)))
                        (if (> penalty *eject-penalty*)
                            (set! d (- d (* penalty penalty))))))
                (if (and discretionary?
                         (active/hyphenated? rv))
                    (if (not (null? position))
                        (set! d (+ d *double-hyphen-demerits*))
                        (set! d (+ d *final-hyphen-demerits*))))
                (if (> (abs (- class (active/fitness rv))) 1)
                    (set! d (+ d *adjacency-demerits*)))))

          ;
          ; Print new feasible break if tracing paragraphs.
          ;
          (if *tracing-paragraphs*
              (begin
                (format #t "~%@")
                (if (null? position)
                    (format #t "\\par")
                    (let ((gi (glyph-stream/read-at tgs (-1+ position))))
                      (case (gi/rator gi)
                        ((penalty) (format #t "\\penalty"))
                        ((disc) (format #t "\\discretionary"))
                        ((kern) (format #t "\\kern")))))
                (format #t " via @@~A b=~A p=~A d=~A"
                        (if (null? (active/break rv))
                            #\0
                            (passive/serial (node/value (active/break rv))))
                        (if (> b *infinite-badness*) #\* b)
                        penalty
                        (if artificial-demerits? #\* d))))

          ;
          ; Update minimal demerits and optimal break positions and
          ; break position lines for each fitness class; also update
          ; global optimum demerits.
          ;
          (set! d (+ d (active/demerits rv))) ; total from start of paragraph
          (if (<= d (vector-ref mdv class))
              (begin
                (vector-set! mdv class (inexact->exact (truncate d)))
                (vector-set! bpv class (active/break rv))
                (vector-set! bplv class line)
                (if (< d md)
                    (set! md d))))

          ;
          ; If active node under consideration will be staying active,
          ; then continue with inner loop; otherwise, deactivate the
          ; current active node, then continue inner loop.
          ;
          (if node-stays-active?
              (loop (node/next r) r))
          (apply loop (tex/deactivate bs r pr ppr ad cad)))))))

;
; Try final break at end of paragraph.  Return active node representing
; optimum break path or '() if no such break was found.
;

(define (tex/find-final-break bs threshold looseness final-pass?)
  (tex/try-break! bs threshold *eject-penalty* #t final-pass?)
  (let ((active (break-state/active bs))
        (fewest-demerits *awful-badness*)
        (best-bet '()))
    (if (eq? (node/next active) active)
        '()
        (begin
          (let loop ((r (node/next active)))
            (if (not (eq? r active))
                (let ((rv (node/value r)))
                  (if (active? rv)
                      (let ((demerits (active/demerits rv)))
                        (if (< demerits fewest-demerits)
                            (begin
                              (set! fewest-demerits demerits)
                              (set! best-bet r)))))
                  (loop (node/next r)))))
          (if (zero? looseness)
              (node/value best-bet)
              (let ((actual-looseness 0))
                (let loop ((r (node/next active)))
                  (let ((rv (node/value r)))
                    (if (active? rv)
                        (let ((dl (- (active/line rv) (break-state/best-line bs))))
                          (if (or (and (< dl actual-looseness)
                                       (<= looseness dl))
                                  (and (> dl actual-looseness)
                                       (>= looseness dl)))
                              (begin
                                (set! best-bet r)
                                (set! actual-looseness dl)
                                (set! fewest-demerits (active/demerits rv)))
                              (let ((demerits (active/demerits rv)))
                                (if (and (= dl actual-looseness)
                                         (< demerits fewest-demerits))
                                    (begin
                                      (set! best-bet r)
                                      (set! fewest-demerits demerits)))))
                          (loop (node/next r))))))
                (break-state/set-best-line! bs (active/line (node/value best-bet)))
                (and (or (= actual-looseness looseness) final-pass?)
                     (node/value best-bet))))))))

(define (tex/find-optimal-breaks bs threshold looseness second-pass? final-pass?)
  second-pass? ; ignore
  (let ((tgs (break-state/stream bs))
        (active (break-state/active bs))
        (ad (break-state/active-deltas bs)))
    ;
    ; Try break at current stream position.
    ;
    (glyph-stream/rewind tgs)
    (let next-item ((pgi '()) (gi (glyph-stream/read tgs)))
      (if (and (not (glyph-stream/end-of-stream? tgs))
               (not (eq? (node/next active) active)))
          (begin
            (case (gi/rator gi)
              ((box)
               (delta/set-nominal! ad
                 (+ (delta/nominal ad) (box/measure gi))))
              ((glue)
               (if (and pgi (eq? (gi/rator pgi) 'box))
                   (tex/try-break! bs threshold 0 #f final-pass?))
               (delta/set-nominal! ad
                 (+ (delta/nominal ad) (glue/nominal gi)))
               (case (glue/stretch-order gi)
                 ((normal)
                  (delta/set-stretch! ad
                    (+ (delta/stretch ad) (glue/stretch gi))))
                 ((fil)
                  (delta/set-stretch-fil! ad
                    (+ (delta/stretch-fil ad) (glue/stretch gi))))
                 ((fill)
                  (delta/set-stretch-fill! ad
                    (+ (delta/stretch-fill ad) (glue/stretch gi))))
                 ((filll)
                  (delta/set-filll! ad
                    (+ (delta/filll ad) (glue/stretch gi)))))
               (delta/set-shrink! ad (+ (delta/shrink ad) (glue/shrink gi))))
              ((penalty)
               (let ((penalty (penalty/penalty gi)))
                 (if (and (negative? penalty)
                          (< penalty (- *infinite-penalty*)))
                     (set! penalty *eject-penalty*))
                 (if (< penalty *infinite-penalty*)
                     (tex/try-break! bs threshold penalty #f final-pass?))))
              ((disc)
               (if (zero? (disc/pre-measure gi))
                   (tex/try-break! bs
                     threshold *explicit-hyphen-penalty* #t final-pass?)
                   (begin
                     (delta/set-nominal! ad
                       (+ (delta/nominal ad) (disc/pre-measure gi)))
                     (tex/try-break! bs threshold *hyphen-penalty* #t final-pass?)
                     (delta/set-nominal! ad
                       (- (delta/nominal ad) (disc/pre-measure gi)))))
               (delta/set-nominal! ad
                 (+ (delta/nominal ad) (disc/measure gi)))))
            (next-item gi (glyph-stream/read tgs)))))
    ;
    ; If at end of paragraph, try final break.
    ;
    (and (glyph-stream/end-of-stream? tgs)
         (tex/find-final-break bs threshold looseness final-pass?))))

(define (tex/find-breaks bs first-line pretolerance tolerance emerg-stretch looseness)
  (let ((threshold pretolerance) (second-pass?) (final-pass?))
    (if (not (negative? threshold))
        (begin
          (if *tracing-paragraphs*
              (format #t "~%@firstpass"))
          (set! second-pass? #f)
          (set! final-pass? #f))
        (begin
          (set! threshold tolerance)
          (set! second-pass? #t)
          (set! final-pass? (not (positive? emerg-stretch)))))
    ;
    ; Create head of active list.
    ;
    (break-state/set-active! bs
      (make-active-node '* *max-line* '() #t 0))
    ;
    ; Multi-pass loop
    ;
    (let loop ()
      (if (> threshold *infinite-badness*)
          (set! threshold *infinite-badness*))
      (let ((q (make-active-node '() first-line *decent-fit* #f 0)))
        (node/set-next! (break-state/active bs) q)
        (node/set-next! q (break-state/active bs))
        (delta/copy (break-state/background-deltas bs)
                    (break-state/active-deltas bs)))
      (break-state/set-passive-number! bs 0)
      (let ((best-bet (tex/find-optimal-breaks bs
                        threshold looseness second-pass? final-pass?)))
        (or best-bet
            (begin
              (node/set-next! (break-state/active bs) (break-state/active bs))
              (break-state/set-passive! bs '())
              (if (not second-pass?)
                  (begin
                    (if *tracing-paragraphs*
                        (format #t "~%@secondpass"))
                    (set! threshold tolerance)
                    (set! second-pass? #t)
                    (set! final-pass? (not (positive? emerg-stretch))))
                  (let ((bkd (break-state/background-deltas bs)))
                    (if *tracing-paragraphs*
                        (format #t "~%@emergencypass"))
                    (delta/set-stretch! bkd
                      (+ (delta/stretch bkd) emerg-stretch))
                    (set! final-pass? #t)))
              (loop)))))))

(define (tex/set-breaks bs best-bet) bs best-bet)

(define (tex/line-break tgs first-line par-fill-skip left-skip right-skip
                        hang-after hang-indent hsize paragraph-shape looseness
                        pretolerance tolerance emerg-stretch)
  (let ((bs (make-break-state)))
    (tex/set-stream! bs tgs par-fill-skip)
    (tex/set-background! bs left-skip right-skip)
    (tex/set-geometry! bs
      hang-after hang-indent hsize paragraph-shape looseness)
    (tex/set-breaks bs
      (tex/find-breaks bs first-line pretolerance tolerance emerg-stretch looseness))))

(define (tex/default-line-break tgs hsize)
  (tex/line-break
    tgs
    1
    (apply find-glue-spec *par-fill-skip*)
    (apply find-glue-spec *left-skip*)
    (apply find-glue-spec *right-skip*)
    0
    0
    hsize
    '()
    0
    *pretolerance*
    *tolerance*
    20.0))

#|
(define (tex/get-breaks active)
  (let loop ((pq (active/break active)) (bl '()))
    (if (null? pq)
        bl
        (let ((pv (node/value pq)))
          (loop (passive/previous pv)
                (cons (cons (passive/serial pv) (passive/position pv)) bl))))))

(define (tex/show-breaks active)
  (let loop ((bl (tex/get-breaks active)))
    (if (not (null? bl))
        (let ((b (car bl)))
          (format #t "~%@@~A ~A" (car b) (cdr b))
          (loop (cdr bl))))))
|#
