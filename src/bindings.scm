
;
; Binding Utilities
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;

(declare (usual-integrations))

(define make-bindings
  (lambda () (make-cell (make-1d-table))))

(define bindings/empty?
  (lambda (b) (or (null? b) (null? (1d-table/alist (cell-contents b))))))

(define bindings/bound?
  (lambda (b k)
    (call-with-current-continuation
     (lambda (exit)
       (1d-table/lookup (cell-contents b) k
         (lambda (v) v (exit #t))
	 (lambda ( ) (exit #f)))))))

(define bindings/extend!
  (lambda (b k v)
    (1d-table/put! (cell-contents b) k v)))

(define bindings/lookup
  (lambda (b k)
    (call-with-current-continuation
     (lambda (exit)
       (1d-table/lookup (cell-contents b) k
         (lambda (v) (exit v))
	 (lambda ( ) (error "Key is unbound:" k)))))))

(define bindings/assign!
  (lambda (b k v)
    (if (not (bindings/bound? b k))
	(error "Key is unbound:" k)
	(bindings/extend! b k v))))

(define bindings/unassign!
  (lambda (b k)
    (1d-table/remove! (cell-contents b) k)))

(define bindings/for-each
  (lambda (b proc)
    (letrec ((loop
	      (lambda (rest)
		(if (not (null? rest))
		    (begin
		      (let ((p (car rest)))
			(proc (car p) (cdr p)))
		      (loop (cdr rest)))))))
      (loop (1d-table/alist (cell-contents b))))))

(define bindings/entries-list
  (lambda (b)
    (1d-table/alist (cell-contents b))))