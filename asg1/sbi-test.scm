#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Author: Adam Henry, adlhenry@ucsc.edu"
;; $Id: sbi.scm,v 1.1 2014-11-03 11:16:32-08 - - $

;; Stdin atom
(define *stdin* (current-input-port))

;; Stderr atom
(define *stderr* (current-error-port))

;; Program name atom
(define *run-file*
	(let-values
		(((dirpath basepath root?)
			(split-path (find-system-path 'run-file))))
		(path->string basepath)
	)
)

;; Warning display function
(define (die list)
	(for-each (lambda (item) (display item *stderr*)) list)
	(newline *stderr*)
	(exit 1)
)

;; Usage warning function
(define (usage-exit)
	(die `("Usage: " ,*run-file* " filename"))
)

;; Define the program counter
(define PC 1)

;; Define statement list size
(define size 0)

;; Define statement list hash
(define stmt-list (make-hash))

;; Define label line-number hash
(define label-linenr (make-hash))

;; Define label table hash
(define label-table (make-hash))

;; Define symbol table hash
(define symbol-table (make-hash))

;; Initialize built-in symbols
(hash-set! symbol-table '+
	(lambda args
		(if (null? (cdr args))
			(+ (car args))
			(+ (car args) (cadr args))
		)
	)
)
(hash-set! symbol-table '-
	(lambda args
		(if (null? (cdr args))
			(- (car args))
			(- (car args) (cadr args))
		)
	)
)
(hash-set! symbol-table '* (lambda (x y) (* x y)))
(hash-set! symbol-table '/ (lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
(hash-set! symbol-table '% (lambda (x y) (modulo x y)))
(hash-set! symbol-table '^ (lambda (x y) (expt x y)))
(hash-set! symbol-table '= (lambda (x y) (= x y)))
(hash-set! symbol-table '< (lambda (x y) (< x y)))
(hash-set! symbol-table '> (lambda (x y) (> x y)))
(hash-set! symbol-table '<> (lambda (x y) (not (= x y))))
(hash-set! symbol-table '>= (lambda (x y) (>= x y)))
(hash-set! symbol-table '<= (lambda (x y) (<= x y)))
(hash-set! symbol-table 'abs (lambda (x) (abs x)))
(hash-set! symbol-table 'acos (lambda (x) (acos x)))
(hash-set! symbol-table 'asin (lambda (x) (asin x)))
(hash-set! symbol-table 'atan (lambda (x) (atan x)))
(hash-set! symbol-table 'cos (lambda (x) (cos x)))
(hash-set! symbol-table 'sin (lambda (x) (sin x)))
(hash-set! symbol-table 'tan (lambda (x) (tan x)))
(hash-set! symbol-table 'exp (lambda (x) (exp x)))
(hash-set! symbol-table 'floor (lambda (x) (floor x)))
(hash-set! symbol-table 'log (lambda (x) (log (+ x 0.0))))
(hash-set! symbol-table 'round (lambda (x) (round x)))
(hash-set! symbol-table 'sqrt (lambda (x) (sqrt x)))
(hash-set! symbol-table 'ceil (lambda (x) (ceiling x)))
(hash-set! symbol-table 'trunc (lambda (x) (truncate x)))
(hash-set! symbol-table 'log10 (lambda (x) (/ (log (+ x 0.0)) (log 10))))
(hash-set! symbol-table 'log2 (lambda (x) (/ (log (+ x 0.0)) (log 2))))
(hash-set! symbol-table 'pi pi)
(hash-set! symbol-table 'e (exp 1))

;; Program file read function
(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		(if (not (input-port? inputfile))
			(die `(,*run-file* ": " ,filename ": open failed"))
			(let ((program (read inputfile)))
				(close-input-port inputfile)
				program
			)
		)
	)
)

;; Statement label hash function
(define (label-hash label stmt)
	(if (null? stmt)
		(hash-set! label-table label stmt)
		(hash-set! label-table label (car stmt))
	)
)

;; Program load function
(define (load-program program)
	(let ((index 0))
		(map (lambda (line)
			(let ((stmt (cdr line)))
				(if (null? stmt)
					stmt
					(let ((stmt-val (car stmt)))
						(set! index (+ index 1))
						(hash-set! stmt-list index stmt-val)
						(if (symbol? stmt-val)
							(let ((label (car stmt)))
								(hash-set! label-linenr label index)
								(label-hash label (cdr stmt))
							)
							stmt-val
						)
					)
				)
			))
			program
		)
		(set! size index)
	)
)

;; Expression evaluate function
(define (eval-expr expr)
	(if (or (null? expr) (number? expr) (symbol? expr))
		(if (symbol? expr)
			(hash-ref symbol-table expr)
			expr
		)
		(if (null? (cddr expr))
			(let ((op (car expr))
				(expr1 (cadr expr)))
				((hash-ref symbol-table op) (eval-expr expr1))
			)
			(let ((op (car expr))
				(expr1 (cadr expr))
				(expr2 (caddr expr)))
				((hash-ref symbol-table op) (eval-expr expr1) (eval-expr expr2))
			)
		)
	)
)

;; Dim subroutine
(define (dim-stmt array-expr)
	(let* ((symbol (caar array-expr))
		(expr (cadar array-expr))
		(size (round (eval-expr expr))))
		(if (< size 0)
			(die `(,*run-file* ": dim: " ,symbol " negative array size"))
			(let ((vec (make-vector size)))
				(hash-set! symbol-table symbol
					(lambda args
						(if (null? (cdr args))
							(vector-ref vec (- (car args) 1))
							(vector-set! vec (- (car args) 1) (cadr args))
						)
					)
				)
			)
		)
	)
)

;; Let subroutine
(define (let-stmt mem-expr)
	(let ((symbol (car mem-expr)) (expr (cadr mem-expr)))
		(if (pair? symbol)
			(let ((array (hash-ref symbol-table (car symbol)))
				(index (eval-expr (cadr symbol))))
				(array index (eval-expr expr))
			)
			(hash-set! symbol-table symbol (eval-expr expr))
		)
	)
)

;; Goto subroutine
(define (goto-stmt label)
	(if (hash-has-key? label-table (car label))
		(set! PC (hash-ref label-linenr (car label)))
		(die `(,*run-file* ": goto: " ,label " undefined"))
	)
)

;; If subroutine
(define (if-stmt op-label)
	(let ((expr (car op-label)) (label (cadr op-label)))
		(if (eval-expr expr)
			(if (hash-has-key? label-table label)
				(set! PC (hash-ref label-linenr label))
				(die `(,*run-file* ": if: " ,label " undefined"))
			)
			expr
		)
	)
)

;; Print subroutine
(define (print-stmt printable)
	(if (null? printable)
		(printf "~n")
		(let ((expr (car printable)) (rest (cdr printable)))
			(if (string? expr)
				(printf "~a" expr)
				(printf "~a" (eval-expr expr))
			)
			(print-stmt rest)
		)
	)
)

;; Input subroutine
(define (input-stmt memory)
	(let ((incount 0))
		(map (lambda (var)
			(let ((val (read *stdin*)))
				(cond
					[(number? val)
					(hash-set! symbol-table var val)
					(set! incount (+ incount 1))]
					[(eof-object? val) (set! incount (- 1))]
					[else (die `(,*run-file* ": input: " ,val " non-numeric"))]
				)
			))
			memory
		)
		(hash-set! symbol-table 'inputcount incount)
	)
)

;; Statement execute function
(define (exe-statement stmt)
	(if (null? stmt)
		stmt
		(let ((stmt-type (car stmt))
			(args (cdr stmt)))
			(cond
				[(equal? stmt-type 'dim) (dim-stmt args)]
				[(equal? stmt-type 'let) (let-stmt args)]
				[(equal? stmt-type 'goto) (goto-stmt args)]
				[(equal? stmt-type 'if) (if-stmt args)]
				[(equal? stmt-type 'print) (print-stmt args)]
				[(equal? stmt-type 'input) (input-stmt args)]
			)
		)
	)
)

;; Program execute function
(define (run-program)
	(let ((prgm-counter PC))
		(if (> PC size)
			PC
			(let ((stmt-val (hash-ref stmt-list PC)))
				(if (symbol? stmt-val)
					(let* ((label stmt-val) (stmt (hash-ref label-table label)))
						(if (null? stmt)
							stmt
							(exe-statement stmt)
						)
					)
					(exe-statement stmt-val)
				)
				(if (= PC prgm-counter)
					(set! PC (+ PC 1))
					PC
				)
				(run-program)
			)
		)
	)
)

;; Program main function
(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
			(program (readlist-from-inputfile sbprogfile)))
			(load-program program)
			(run-program)
		)
	)
)

;; Call main function
(main (vector->list (current-command-line-arguments)))