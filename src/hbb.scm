
;
; Half-Balanced Binary Tree
;
; See "A new class of balanced search trees:  half-balanced binary
; search trees," by H. J. Olivie, RAIRO Theor. Infor., (16), 1982,
; pp. 51-71.
;
; Copyright 1993 Metis Technology, Inc.  All rights reserved.
;
;

;
; The public functions supported by this module are:
;
;   make-hbb
;
;     Return a new HBB tree.
; 
;   hbb/get hbb key #!optional compare if-found if-not-found
;
;     Lookup value of key in hbb tree.  If found, invoke if-found
;     with the value as a single argument; otherwise, invoke if-not-found
;     with no arguments.  The key may be a number, a symbol, a string, or
;     any other object.  In the first three cases, the compare argument
;     need not be supplied; in case key is not a number, symbol, or string,
;     compare should be supplied as a procedure of two arguments
;     representing two keys to being compared.  The comparsion procedure
;     should return -1 if the first key precedes the second key, 1 if the
;     first key follows the second key, or 0 if the keys are eqv?.  If
;     one of the continuation procedures, if-found or if-not-found, is not
;     supplied, then hbb/lookup's own continuation is used instead.
;
;   hbb/put! hbb key value #!optional compare
;
;     Insert key with the specified into the HBB tree.  If key already exists,
;     then its previous value is replaced with the new value.  Keys are
;     compared using the compare procedure as described above.  The return
;     value is unspecified.
;
;   hbb/clear! hbb key #!optional compare
;
;     Delete key and its associated value from the HBB tree.  If key is
;     not present in the tree, then do nothing.  Keys are compared using
;     the compare procedure as described above.  The return value is
;     unspecified.
;
;   hbb/adjust! hbb adjust #!optional key1 key2 compare if-merged
;
;     Adjust the keys in the HBB tree, calling the procedure adjust
;     for each key.  The adjust procedure should take an existing key
;     and return an adjusted key.  If key1 is supplied, then adjust only
;     those keys that equal or follow key1; if key2 is supplied,
;     then adjust only those keys from key1 (inclusive) to key2
;     (exclusive).  Keys are compared using the compare procedure as
;     described above.  The return value is unspecified.
;
;     Adjustments to keys must be either monotonically increasing or
;     monotonically decreasing.  Failure to observe this restriction
;     may invalidate the tree's invariants.  If some key would be
;     merged into an adjacent key (either preceding or following),
;     then the procedure if-merged, if supplied, is invoked with the
;     merging key and value, and the key and value of the adjacent key as
;     its arguments.  This procedure should return a new value which
;     will replace the value of the adjacent key.  If no if-merged
;     procedure is specified, then the value of the key being merged will
;     be discarded, with no changes to the adjacent key and value.
;
;   hbb/for-each hbb proc #!optional key1 key2 compare
;
;     Enumerate all key/value pairs of the HBB tree, calling proc
;     for each pair.  If key1 is supplied, then enumerate only
;     those keys that are equal to or follow key1; if key2 is supplied,
;     then enumerate only those keys from key1 (inclusive) to key2
;     (exclusive).  Keys are compared using the compare procedure as
;     described above.  The return value is unspecified.
;
;

;
; An HBB tree, and the left and right branches of an HBB node are
; represented as cells containing nodes rather than the nodes themselves.
; This allows the tree to be restructured in a way which would be
; much more messy were it not for the use of cells.
;

(declare (usual-integrations))

(define-record hbb-node (left right height shortest-path key value))

(define *hbb-leaf-node* (make-hbb-node '() '() 0 0 '() '()))

(define *hbb-max-height* 64)

(define hbb-node/out-of-balance?
  (lambda (n)
    (let ((ln (cell-contents (hbb-node/left n)))
	  (rn (cell-contents (hbb-node/right n))))
      (or (not (= (hbb-node/shortest-path n)
		  (1+ (min (hbb-node/shortest-path ln)
			   (hbb-node/shortest-path rn)))))
	  (not (= (hbb-node/height n)
		  (1+ (max (hbb-node/height ln)
			   (hbb-node/height rn)))))))))

(define hbb-node/update!
  (lambda (n)
    (let ((ln (cell-contents (hbb-node/left n)))
	  (rn (cell-contents (hbb-node/right n))))
      (hbb-node/set-height! n
	(1+ (max (hbb-node/height ln)
		 (hbb-node/height rn))))
      (hbb-node/set-shortest-path! n
	(1+ (min (hbb-node/shortest-path ln)
		 (hbb-node/shortest-path rn)))))))

(define hbb-node/rotate!
  (lambda (nc dir)
    (let ((n (cell-contents nc)))
      (case dir
	((left)
	 (let ((nn (cell-contents (hbb-node/right n))))
	   (set-cell-contents!
	     (hbb-node/right n)
	     (cell-contents (hbb-node/left nn)))
	   (set-cell-contents! (hbb-node/left nn) n)
	   (hbb-node/update! n)
	   (hbb-node/update! nn)
	   (set-cell-contents! nc nn)))
	((right)
	 (let ((nn (cell-contents (hbb-node/left n))))
	   (set-cell-contents!
	     (hbb-node/left n)
	     (cell-contents (hbb-node/right nn)))
	   (set-cell-contents! (hbb-node/right nn) n)
	   (hbb-node/update! n)
	   (hbb-node/update! nn)
	   (set-cell-contents! nc nn)))))))

(define make-hbb
  (lambda () (make-cell *hbb-leaf-node*)))

(define hbb/default-compare
  (lambda (key)
    (cond
     ((number? key)
      (lambda (k1 k2)
	(cond
	 ((< k1 k2) -1)
	 ((> k1 k2)  1)
	 (else 0))))
     ((symbol? key)
      (lambda (k1 k2)
	(cond
	 ((string<? (symbol->string k1) (symbol->string k2)) -1)
	 ((string>? (symbol->string k1) (symbol->string k2))  1)
	 (else 0))))
     ((string? key)
      (lambda (k1 k2)
	(cond
	 ((string<? k1 k2) -1)
	 ((string>? k1 k2)  1)
	 (else 0))))
     (else
      (error "Can't determine default compare function for key:" key)))))

(define hbb/get
  (lambda (hbb key #!optional compare if-found if-not-found)
    (call-with-current-continuation
     (lambda (exit)
       (if (default-object? compare)
	   (set! compare (hbb/default-compare key)))
       (if (default-object? if-found)
	   (set! if-found exit))
       (if (default-object? if-not-found)
	   (set! if-not-found (lambda () (exit '()))))
       (letrec ((loop
		 (lambda (n)
		   (if (eq? n *hbb-leaf-node*)
		       (if-not-found)
		       (let ((d (compare key (hbb-node/key n))))
			 (cond 
			  ((< d 0)
			   (loop (cell-contents (hbb-node/left n))))
			  ((> d 0)
			   (loop (cell-contents (hbb-node/right n))))
			  (else
			    (if-found
			      (hbb-node/value n)))))))))
	 (loop (cell-contents hbb)))))))

(define hbb/descend-recording
  (lambda (hbb key compare nodes nodedirs nnodes if-found if-not-found)
    (letrec ((loop
	      (lambda (nc)
		(let ((n (cell-contents nc)))
		  (if (eq? n *hbb-leaf-node*)
		      (if-not-found nc)
		      (let ((d (compare key (hbb-node/key n)))
			    (k (cell-contents nnodes)))
			(vector-set! nodes k nc)
			(cond 
			 ((< d 0)
			  (vector-set! nodedirs k 'left)
			  (set-cell-contents! nnodes (+ k 1))
			  (loop (hbb-node/left n)))
			 ((> d 0)
			  (vector-set! nodedirs k 'right)
			  (set-cell-contents! nnodes (+ k 1))
			  (loop (hbb-node/right n)))
			 (else
			  (if-found nc)))))))))
      (loop hbb))))
	     
(define hbb/put-restructure!
  (lambda (nc dir)
    (let ((n (cell-contents nc)))
      (case dir
	((left)
	 (let* ((n1 (cell-contents (hbb-node/left n)))
		(ln1 (cell-contents (hbb-node/left n1)))
		(rn1 (cell-contents (hbb-node/right n1))))
	   (if (> (hbb-node/height ln1)
		  (hbb-node/height rn1))
	       (hbb-node/rotate! nc 'right)
	       (begin
		 (hbb-node/rotate! (hbb-node/left n) 'left)
		 (hbb-node/rotate! nc 'right)))))
	((right)
	 (let* ((n1 (cell-contents (hbb-node/right n)))
		(ln1 (cell-contents (hbb-node/left n1)))
		(rn1 (cell-contents (hbb-node/right n1))))
	   (if (> (hbb-node/height rn1)
		  (hbb-node/height ln1))
	       (hbb-node/rotate! nc 'left)
	       (begin
		 (hbb-node/rotate! (hbb-node/right n) 'right)
		 (hbb-node/rotate! nc 'left)))))))))

(define hbb/put!
  (lambda (hbb key value #!optional compare)
    (call-with-current-continuation
     (lambda (exit)
       (if (default-object? compare)
	   (set! compare (hbb/default-compare key)))
       (let ((nodes (make-vector *hbb-max-height*))
	     (nodedirs (make-vector *hbb-max-height*))
	     (nnodes (make-cell 0)))
	 (letrec ((restore!
		   (lambda (k)
		     (if (not (zero? k))
			 (let* ((k (- k 1))
				(n (cell-contents (vector-ref nodes k))))
			   (if (hbb-node/out-of-balance? n)
			       (begin
				 (hbb-node/update! n)
				 (if (> (hbb-node/height n)
					(* 2 (hbb-node/shortest-path n)))
				     (hbb/put-restructure!
				       (vector-ref nodes k)
				       (vector-ref nodedirs k)))))
			   (restore! k))))))
	   (hbb/descend-recording hbb key compare nodes nodedirs nnodes
	     (lambda (nc)
	       (exit (hbb-node/set-value! (cell-contents nc) value)))
	     (lambda (nc)
	       (set-cell-contents! nc
	         (make-hbb-node
		  (make-cell *hbb-leaf-node*)
		  (make-cell *hbb-leaf-node*) 1 1 key value))
	       (exit (restore! (cell-contents nnodes)))))))))))

(define hbb/clear-restructure!
  (lambda (nc dir)
    (let ((n (cell-contents nc)))
      (case dir
	((left)
	 (let* ((n1 (cell-contents (hbb-node/right n)))
		(ln1 (cell-contents (hbb-node/left n1)))
		(rn1 (cell-contents (hbb-node/right n1))))
	   (cond					  
	    ((> (hbb-node/height rn1) (hbb-node/height ln1))      ; case 1
	     (hbb-node/rotate! nc 'left))	                  
	    ((< (hbb-node/height rn1) (hbb-node/height ln1))      ; case 2
	     (hbb-node/rotate! (hbb-node/right n) 'right)
	     (hbb-node/rotate! nc 'left))
	    (else
	     (let* ((n2 ln1)
		    (ln2 (cell-contents (hbb-node/left n2)))
		    (rn2 (cell-contents (hbb-node/right n2))))
	       (if (odd? (hbb-node/height n2))	  		  ; case 3
		   (hbb-node/rotate! nc 'left)
		   (let ((ln (cell-contents
			      (hbb-node/left n)))
			 (rn (cell-contents
			      (hbb-node/right n))))
		     (cond
		      ((>= (hbb-node/height rn2)  		  ; case 4
			   (hbb-node/height ln2))
			(hbb-node/rotate!
			  (hbb-node/right n) 'right)
			(hbb-node/rotate! nc 'left))
		      ((=  (hbb-node/height ln)   		  ; case 5
			   (hbb-node/height ln2))
			(hbb-node/rotate! nc 'left))
		      (else			  		  ; case 6
			(hbb-node/rotate!
			  (hbb-node/left rn) 'right)
			(hbb-node/rotate!
			  (hbb-node/right n) 'right)
			(hbb-node/rotate! nc 'left))))))))))
	((right)
	 (let* ((n1 (cell-contents (hbb-node/left n)))
		(ln1 (cell-contents (hbb-node/left n1)))
		(rn1 (cell-contents (hbb-node/right n1))))
	   (cond					  
	    ((> (hbb-node/height ln1) (hbb-node/height rn1))	  ; case 1
	     (hbb-node/rotate! nc 'right))	  
	    ((< (hbb-node/height ln1) (hbb-node/height rn1))	  ; case 2
	     (hbb-node/rotate! (hbb-node/left n) 'left)
	     (hbb-node/rotate! nc 'right))
	    (else
	     (let* ((n2 rn1)
		    (ln2 (cell-contents (hbb-node/left n2)))
		    (rn2 (cell-contents (hbb-node/right n2))))
	       (if (odd? (hbb-node/height n2))	  		  ; case 3
		   (hbb-node/rotate! nc 'right)
		   (let ((ln (cell-contents
			      (hbb-node/left n)))
			 (rn (cell-contents
			      (hbb-node/right n))))
		     (cond
		      ((>= (hbb-node/height ln2)  		  ; case 4
			   (hbb-node/height rn2))
			(hbb-node/rotate!
			  (hbb-node/left n) 'left)
			(hbb-node/rotate! nc 'right))
		      ((=  (hbb-node/height rn)   		  ; case 5
			   (hbb-node/height rn2))
			(hbb-node/rotate! nc 'right))
		      (else			  		  ; case 6
			(hbb-node/rotate!
			  (hbb-node/right ln) 'left)
			(hbb-node/rotate!
			  (hbb-node/left n) 'left)
			(hbb-node/rotate!
			  nc 'right))))))))))))))

(define hbb/clear-replace!
  (lambda (rc n)
    (let* ((r (cell-contents rc))
	   (lr (cell-contents (hbb-node/left r)))
	   (rr (cell-contents (hbb-node/right r))))
      (if (eq? lr *hbb-leaf-node*)
	  (begin
	    (hbb-node/set-key! n (hbb-node/key r))
	    (hbb-node/set-value! n (hbb-node/value r))
	    (set-cell-contents! rc rr))
	  (begin
	    (hbb/clear-replace! (hbb-node/left r) n)
	    (if (hbb-node/out-of-balance? r)
		(begin
		  (hbb-node/update! r)
		  (if (> (hbb-node/height r)
			 (* 2 (hbb-node/shortest-path r)))
		      (hbb/clear-restructure! rc 'left)))))))))

(define hbb/clear-delete!
  (lambda (nc)
    (let* ((n (cell-contents nc))
	   (ln (cell-contents (hbb-node/left n)))
	   (rn (cell-contents (hbb-node/right n))))
      (cond
       ((eq? rn *hbb-leaf-node*) (set-cell-contents! nc ln))
       ((eq? ln *hbb-leaf-node*) (set-cell-contents! nc rn))
       (else
	(hbb/clear-replace! (hbb-node/right n) n)
	(if (hbb-node/out-of-balance? n)
	    (begin
	      (hbb-node/update! n)
	      (if (> (hbb-node/height n)
		     (* 2 (hbb-node/shortest-path n)))
		  (hbb/clear-restructure! nc 'right)))))))))

(define hbb/clear!
  (lambda (hbb key #!optional compare)
    (call-with-current-continuation
     (lambda (exit)
       (if (default-object? compare)
	   (set! compare (hbb/default-compare key)))
       (let ((nodes (make-vector *hbb-max-height*))
	     (nodedirs (make-vector *hbb-max-height*))
	     (nnodes (make-cell 0)))
	 (letrec ((restore!
		   (lambda (k)
		     (if (not (zero? k))
			 (let* ((k (- k 1))
				(n (cell-contents (vector-ref nodes k))))
			   (if (hbb-node/out-of-balance? n)
			       (begin
				 (hbb-node/update! n)
				 (if (> (hbb-node/height n)
					(* 2 (hbb-node/shortest-path n)))
				     (hbb/clear-restructure!
				       (vector-ref nodes k)
				       (vector-ref nodedirs k)))))
			   (restore! k))))))
	   (hbb/descend-recording hbb key compare nodes nodedirs nnodes
	     (lambda (nc)
	       (hbb/clear-delete! nc)
	       (exit (restore! (cell-contents nnodes))))
	     (lambda (nc)
	       (exit (set-cell-contents! nc (cell-contents nc)))))))))))

(define hbb/for-each
  (lambda (hbb proc #!optional key1 key2 compare)
    (if (and (not (default-object? key1))
	     (default-object? compare))
	(set! compare (hbb/default-compare key1)))
    (letrec ((loop
	      (lambda (n)
               (let ((key (hbb-node/key n)))
		 (if (not (eq? n *hbb-leaf-node*))
		     (let ((c1 (or (default-object? key1)
				   (>= (compare key key1) 0)))
			   (c2 (or (default-object? key2)
				   (< (compare key key2) 0))))
		       (if c1
			   (loop (cell-contents (hbb-node/left n))))
		       (if (and c1 c2)
			   (proc key (hbb-node/value n)))
		       (if c2
			   (loop (cell-contents (hbb-node/right n))))))))))
      (loop (cell-contents hbb)))))


;
; Debugging
;
#|
(define hbb-node/dump
  (lambda (n level label formatter)
    (format #t "~%~A~A: ~A"
	    (make-string (* 2 level) #\space)
	    label
	    (formatter (hbb-node/key n) (hbb-node/value n)))))

(define hbb/dump
  (lambda (hbb formatter)
    (letrec ((loop
	      (lambda (n k label)
		(if (not (eq? n *hbb-leaf-node*))
		    (begin
		      (loop (cell-contents (hbb-node/left n)) (1+ k) "L")
		      (hbb-node/dump n k label formatter)
		      (loop (cell-contents (hbb-node/right n)) (1+ k) "R"))))))
      (loop (cell-contents hbb) 0 "T"))))
|#
