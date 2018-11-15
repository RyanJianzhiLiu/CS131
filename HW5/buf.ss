(load "expr-compare1.ss")
(define xbnds '((a x) (b y) (c z)))
(define Xbnds '((A X) (B Y) (C Z)))
(define xbody '(+ a c))
(define Xbody '(+ A C))

(define dicts (find-alias xbnds Xbnds empty empty))
(define xdict (car dicts))
(define Xdict (cdr dicts))

(define uni_xbnds (unify_bnds xbnds xdict))
(define uni_Xbnds (unify_bnds Xbnds Xdict))
(define uni_xbody (unify xbody xdict))
(define uni_Xbody (unify Xbody Xdict))
(define diff_bnds (expr-compare uni_xbnds uni_Xbnds))
(define diff_body (expr-compare uni_xbody uni_Xbody))
(define diff `(let ,diff_bnds ,diff_body))

(define x `(let ,xbnds ,xbody))
(define X `(let ,Xbnds ,Xbody))
(define diff0 (expr-compare x X))

(equal? diff diff0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "expr-compare2.ss")
(define x '(let ((c 1)) (let ((a c)) a)))
(define y '(let ((d 1)) (let ((a d)) a)))
(define dicts (find-bnd-alias (cadr x) (cadr y) empty empty))

(define xbnds (cadr x))
(define ybnds (cadr y))
(define xbody (caddr x))
(define ybody (caddr y))

(define uni_xbnds (unify_bnds xbnds (car dicts) empty))
(define uni_ybnds (unify_bnds ybnds (cdr dicts) empty))
(define uni_xbody (unify xbody (car dicts)))
(define uni_ybody (unify ybody (cdr dicts)))

(define bnd_cmp uni_xbnds uni_ybnds)
(expr-cmp uni_xbody uni_ybody (car dicts) (cdr dicts))
(equal? (expr-compare x y) '(let ((c!d 1)) (let ((a c!d)) a)))

(define f '(lambda (x y) (+ x y)))
(define F '(lambda (X Y) (+ X Y)))
(define dicts (find-alias (cadr f) (cadr F) empty empty))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test-expr-x '((lambda (a b) (f a b)) 1 2))
(define test-expr-y '((lambda (a c) (f c a)) 1 2))
(test-expr-compare test-expr-x test-expr-y)

(let ((f (lambda (x) (* x x)))
      (a 2) (x 3) (y 5))
  (let ((a ((lambda (f x y) (- (+ (f x) (f y)) a)) f x y))
	(b (- (f (+ x y)) a)))
    (if (= a b) '? (- a b))
  )
)
