
;
; Graphics Primitives
;

(declare (usual-integrations))

(define-record point (x y))

(define (point/copy pt #!optional npt)
  (if (default-object? npt)
      (make-point (point/x pt) (point/y pt))
      (begin
	(point/set-x! npt (point/x pt))
	(point/set-y! npt (point/y pt))
	npt)))

(define (point/add pt deltas)
  (make-point (+ (point/x pt) (point/x deltas))
	      (+ (point/y pt) (point/y deltas))))

(define (point/difference pt1 pt2)
  (make-point (- (point/x pt1) (point/x pt2))
	      (- (point/y pt1) (point/y pt2))))

(define (point/left? p1 p2)
  (< (point/x p1) (point/x p2)))

(define (point/right? p1 p2)
  (> (point/x p1) (point/x p2)))

(define (point/above? p1 p2)
  (> (point/y p1) (point/y p2)))

(define (point/below? p1 p2)
  (< (point/y p1) (point/y p2)))

(define (point/scale p sx sy)
  (make-point (* (point/x p) sx) (* (point/y p) sy)))

(define-record rect (ll ur))

(define (make-empty-rect)
  (make-rect (make-point 0 0) (make-point 0 0)))

(define (rect/copy rs #!optional rd)
  (if (default-object? rd)
      (set! rd (make-rect (make-point 0 0) (make-point 0 0))))
  (let ((lls (rect/ll rs))
	(lld (rect/ll rd))
	(urs (rect/ur rs))
	(urd (rect/ur rd)))
    (point/set-x! lld (point/x lls))
    (point/set-y! lld (point/y lls))
    (point/set-x! urd (point/x urs))
    (point/set-y! urd (point/y urs))
    rd))

(define (rect->list r)
  (let ((ll (rect/ll r))
	(ur (rect/ur r)))
    (list (point/x ll) (point/y ll) (point/x ur) (point/y ur))))

(define (list->rect rl)
  (make-rect (make-point (list-ref rl 0) (list-ref rl 1))
	     (make-point (list-ref rl 2) (list-ref rl 3))))

(define (rect/origin-x r)
  (point/x (rect/ll r)))

(define (rect/origin-y r)
  (point/y (rect/ll r)))

(define (rect/width r)
  (- (point/x (rect/ur r)) (point/x (rect/ll r))))

(define (rect/height r)
  (- (point/y (rect/ur r)) (point/y (rect/ll r))))

(define (rect/empty? r)
  (and (zero? (rect/width r))
       (zero? (rect/height r))))

(define (rect/translate r ll)
  (let ((w (rect/width r))
	(h (rect/height r)))
    (make-rect (point/copy ll)
	       (point/add ll (make-point w h)))))

(define (rect/scale r sx sy)
  (make-rect (point/scale (rect/ll r) sx sy)
	     (point/scale (rect/ur r) sx sy)))

(define (rect/subset-of? r1 r2)
  (let ((ll1 (rect/ll r1))
	(ll2 (rect/ll r2))
	(ur1 (rect/ur r1))
	(ur2 (rect/ur r2)))
    (and (not (point/left? ll1 ll2))
	 (not (point/right? ur1 ur2))
	 (not (point/above? ur1 ur2))
	 (not (point/below? ll1 ll2))
	 #t)))

(define (rect/union r1 r2 #!optional rd)
  (if (default-object? rd)
      (set! rd (make-empty-rect)))
  (let ((ll1 (rect/ll r1))
	(ll2 (rect/ll r2))
	(lld (rect/ll rd))
	(ur1 (rect/ur r1))
	(ur2 (rect/ur r2))
	(urd (rect/ur rd)))
    (point/set-x! lld (min (point/x ll1) (point/x ll2)))
    (point/set-y! lld (min (point/y ll1) (point/y ll2)))
    (point/set-x! urd (max (point/x ur1) (point/x ur2)))
    (point/set-y! urd (max (point/y ur1) (point/y ur2)))
    rd))

(define-record xfrm (a b c d tx ty))

(define (make-identity-xfrm)
  (make-xfrm 1 0 0 1 0 0))

(define (xfrm/translate x pt)
  (make-xfrm (xfrm/a x)
	     (xfrm/b x)
	     (xfrm/c x)
	     (xfrm/d x)
	     (+ (xfrm/tx x) (point/x pt))
	     (+ (xfrm/ty x) (point/y pt))))

(define (xfrm/scale x sx sy)
  (make-xfrm (* (xfrm/a x) sx)
	     (xfrm/b x)
	     (xfrm/c x)
	     (* (xfrm/d x) sy)
	     (xfrm/tx x)
	     (xfrm/ty x)))

(define (xfrm/rotate x r)
  (let ((cr (cos r))
	(sr (sin r)))
    (make-xfrm (* (xfrm/a x) cr)
	       (* (xfrm/b x) sr)
	       (* (xfrm/c x) (- sr))
	       (* (xfrm/d x) cr)
	       (xfrm/tx x)
	       (xfrm/ty x))))

(define (xfrm/concat x0 x1)
  (make-xfrm (+ (* (xfrm/a  x0) (xfrm/a x1))
		(* (xfrm/b  x0) (xfrm/b x1)))
	     (+ (* (xfrm/a  x0) (xfrm/b x1))
		(* (xfrm/b  x0) (xfrm/d x1)))
	     (+ (* (xfrm/c  x0) (xfrm/d x1))
		(* (xfrm/d  x0) (xfrm/c x1)))
	     (+ (* (xfrm/c  x0) (xfrm/b x1))
		(* (xfrm/d  x0) (xfrm/d x1)))
	     (+ (* (xfrm/tx x0) (xfrm/a x1))
		(* (xfrm/ty x0) (xfrm/c x1))
		(* (xfrm/tx x0) 1))
	     (+ (* (xfrm/tx x0) (xfrm/b x1))
		(* (xfrm/ty x0) (xfrm/d x1))
		(* (xfrm/ty x1) 1))))

