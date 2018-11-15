(define (lambda-cmp x y) #f)
(define (let-cmp x y) #f)

(define (expr-compare x y)
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
	 [(equal? (car x) 'let) (let-cmp x y)]
	 [else (cons (car x) (expr-compare (cdr x) (cdr y)))]
	)
	;if heads of x,y not equal
	(if (or (spf (car x)) (spf (car y)))
	    ;if either head of x or y is special form
	    `(if % ,x ,y)
	    ;if neither head of x nor y is special form
	    (cons
	     (expr-compare (car x) (car y))
	     (expr-compare (cdr x) (cdr y))
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
