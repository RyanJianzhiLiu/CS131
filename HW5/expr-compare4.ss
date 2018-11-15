(define (expr-compare x y) (expr-cmp x y empty empty))

(define (expr-cmp x y xdict ydict)
  (cond
   [(and (null? x) (null? y)) empty]
   [(and (not (list? x)) (not (list? y)) (equal? x y)) x]
   [(and (equal? x #t) (equal? y #f)) '%]
   [(and (equal? x #f) (equal? y #t)) '(not %)]
   [(and (list? x) (list? y) (= (length x) (length y)))
    (if (equal? (car x) (car y))
	;if heads of x,y equal
	(cond
	 [(equal? (car x) 'quote) (quote-cmp x y)]
	 [(equal? (car x) 'lambda) (lambda-cmp x y xdict ydict)]
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

(define (lambda-cmp x y xdict ydict)
  ;x: lambda argv-list body
  ;cadr x -> argvs
  ;caddr x -> body
  ;cdddr x -> vals (maybe empty)
  (if (equal? (length (cadr x)) (length (cadr y)))
      (let [(dicts (find-alias (cadr x) (cadr y) empty empty))
            (xargs (cadr x)) (yargs (cadr y))
            (xbody (caddr x)) (ybody (caddr y))]
        (let [(uni_xargs (unify xargs (car dicts)))
              (uni_yargs (unify yargs (cdr dicts)))
              (uni_xbody (unify xbody (append (car dicts) xdict)))
              (uni_ybody (unify ybody (append (cdr dicts) ydict)))]
	  `( lambda
	     ,(expr-cmp uni_xargs uni_yargs
			(append (car dicts) xdict) (append (cdr dicts) ydict))
	     ,(expr-cmp uni_xbody uni_ybody
			(append (car dicts) xdict) (append (cdr dicts) ydict))
	   )
        )
      )
      `(if % ,x ,y)
  )
)

(define (find-alias xargs yargs xdict ydict)
  (if (null? xargs)
      (cons xdict ydict)
      (let [(xid (car xargs)) (yid (car yargs))]
	(if (equal? xid yid)
	    (find-alias (cdr xargs) (cdr yargs) xdict ydict)
	    (let [(alias (con! xid yid))]
	      (let [(new_xdict (cons (cons xid alias) xdict))
		    (new_ydict (cons (cons yid alias) ydict))]
		(find-alias (cdr xargs) (cdr yargs) new_xdict new_ydict)
	      )
	    )
	)
      )
  )
)

(define (let-cmp x y xdict ydict)
  ;x: let binding-list body
  ;cadr x -> bindings
  ;caddr x -> body
  (if (equal? (length (cadr x)) (length (cadr y)))
      (let [(xids-vals (split_bnds (cadr x) empty empty))
	    (yids-vals (split_bnds (cadr y) empty empty))]
	(let [(dicts (find-alias (car xids-vals) (car yids-vals) empty empty))
	      (xbody (caddr x)) (ybody (caddr y))]
	)
      )
      (let [(dicts (find-bnd-alias (cadr x) (cadr y) empty empty))
	    (xbnds (cadr x)) (ybnds (cadr y))
	    (xbody (caddr x)) (ybody (caddr y))]
	(let [(uni_xbnds (unify_bnds xbnds (car dicts) xdict))
	      (uni_ybnds (unify_bnds ybnds (cdr dicts) ydict))
	      (uni_xbody (unify xbody (append (car dicts) xdict)))
	      (uni_ybody (unify ybody (append (cdr dicts) ydict)))]
	  `( let
	     ,(expr-cmp uni_xbnds uni_ybnds
			(append (car dicts) xdict) (append (cdr dicts) ydict))
	     ,(expr-cmp uni_xbody uni_ybody
			(append (car dicts) xdict) (append (cdr dicts) ydict))
	   )
	)
      )
      `(if % ,x ,y)
  )
)

(define (split_bnds bnds id_acc val_acc)
  (if (null? bnds)
      (cons id_acc val_acc)
      (let [(new_id_acc (append id_acc `(,(caar bnds))))
	    (new_val_acc (append val_acc `(,(cadar bnds))))]
	(split_bnds (cdr bnds) new_id_acc new_val_acc)
      )
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
(define (find-bnd-alias xbnds ybnds xdict ydict)
  (if (null? xbnds)
      (cons xdict ydict)
      (let [(xid (caar xbnds)) (yid (caar ybnds))]
	(if (equal? xid yid)
	    (find-bnd-alias (cdr xbnds) (cdr ybnds) xdict ydict)
	    (let [(alias (con! xid yid))]
	      (let [(new_xdict (cons (cons xid alias) xdict))
		    (new_ydict (cons (cons yid alias) ydict))]
		(find-bnd-alias (cdr xbnds) (cdr ybnds) new_xdict new_ydict)
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
