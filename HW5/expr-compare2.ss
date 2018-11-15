(define (lambda-cmp x y) #f)

(define (expr-compare x y) (expr-cmp x y empty empty))

(define (expr-cmp x y xdict ydict)
  (cond
   [(equal? x y) x]
   [(and (equal? x #t) (equal? y #f)) '%]
   [(and (equal? x #f) (equal? y #t)) '(not %)]
   [(and (list? x) (list? y) (= (length x) (length y)))
    (if (equal? (car x) (car y))
	;if heads of x,y equal
	(cond
	 [(equal? (car x) 'quote) (quote-cmp x y)]
	 [(equal? (car x) 'lambda) (lambda-cmp x y)]
	 [(equal? (car x) 'let) (let-cmp x y xdict ydict)]
	 [else (cons (car x) (expr-cmp (cdr x) (cdr y) xdict ydict))]
	)
	;if heads of x,y not equal
	(if (or (spf (car x)) (spf (car y)))
	    ;if either head of x or y is special form
	    `(if % ,x ,y)
	    ;if neither head of x nor y is special form
	    (cons
	     (expr-cmp (car x) (car y) xdict ydict)
	     (expr-cmp (cdr x) (cdr y) xdict ydict)
	    )
	 )
    )
   ]
   [else `(if % ,x ,y)]
  )
)

(define (spf x)
  (or
   (equal? x 'quote)
   (equal? x 'lambda)
   (equal? x 'let)
   (equal? x 'if)
  )
)

(define (quote-cmp x y)
  (if (equal? x y)
      (cadr x)
      `(if % ,x ,y)
  )
)

(define (let-cmp x y xdict ydict)
  ;cadr x -> bindings
  ;caddr x -> body
  (if (equal? (length (cadr x)) (length (cadr y)))
      (let [(dicts (find-alias (cadr x) (cadr y) xdict ydict))
	    (xbnds (cadr x)) (ybnds (cadr y))
	    (xbody (caddr x)) (ybody (caddr y))]
	;TODO
	(let [(uni_xbnds (unify_bnds xbnds (car dicts) xdict))
	      (uni_ybnds (unify_bnds ybnds (cdr dicts) ydict))
	      (uni_xbody (unify xbody (car dicts)))
	      (uni_ybody (unify ybody (cdr dicts)))]
	  `( let
	     ,(expr-cmp uni_xbnds uni_ybnds (car dicts) (cdr dicts))
	     ,(expr-cmp uni_xbody uni_ybody (car dicts) (cdr dicts))
	   )
	)
      )
      `(if % ,x ,y)
  )
)
(define (con! s1 s2)
  (string->symbol
   (string-append (symbol->string s1) "!" (symbol->string s2)))
)
(define (lookup dict key)
  (if (null? dict)
      key
      (if (equal? key (caar dict))
	  (cdar dict)
	  (lookup (cdr dict) key)
      )
  )
)
(define (find-alias xbnds ybnds xdict ydict)
  (if (null? xbnds)
      (cons xdict ydict)
      (let [(xid (caar xbnds)) (yid (caar ybnds))]
	(if (equal? xid yid)
	    (find-alias (cdr xbnds) (cdr ybnds) xdict ydict)
	    (let [(alias (con! xid yid))]
	      (let [(new_xdict (cons (cons xid alias) xdict))
		    (new_ydict (cons (cons yid alias) ydict))]
		(find-alias (cdr xbnds) (cdr ybnds) new_xdict new_ydict)
	       )
	     )
	 )
       )
  )
)

(define (unify symbols dict)
  (if (list? symbols)
      (if (null? symbols)
	  empty
	  (let [(new_symbol (lookup dict (car symbols)))]
	    (cons new_symbol (unify (cdr symbols) dict))
	  )
      )
      (lookup dict symbols)
  )
)

(define (unify_bnds bnds dict old_dict)
  (if (null? bnds)
      empty
      (let [(id (caar bnds)) (val (cadar bnds))]
	(cons
	 (cons (unify id dict) (list (unify val old_dict)))
	 (unify_bnds (cdr bnds) dict old_dict)
	)
      )
  )
)
