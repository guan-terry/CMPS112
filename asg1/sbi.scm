#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;; $Id: sbi.scm,v 1.4 2018-04-11 16:31:36-07 - - $
;; AUTHORS:
;; Terry Guan 
;; CruzID: teguan
;; Email: teguan@ucsc.edu
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "~n")
    (map (lambda (line)
	(if (null? (cdr line))
	 (printf "~s~n" (cdr line))
	(printf "~s~n" (cdr line)
	))) 
    program)
    (printf "~n"))

;----------------------------------------------------------------------
;-------------------------------My functions---------------------------
;----------------------------------------------------------------------

;;TODO!!!!!!1
;;Function tables and variables tables are combined to be 
;;identfier tables
(define *label-table* (make-hash))
(define *function-table* (make-hash))
(define *variable-table* (make-hash))

(define (function-get key)
        (hash-ref *function-table* key))
(define (function-put! key value)
        (hash-set! *function-table* key value))


(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
	(atan 	 ,(lambda (x) (atan x)))
	(asin 	 ,(lambda (x) (asin x)))
	(acos 	 ,(lambda (x) (acos x)))
	(cos 	 ,(lambda (x) (cos x)))
	(sin 	 ,(lambda (x) (sin x)))
	(tan	 ,(lambda (x) (tan x)))
	(abs	 ,(lambda (x) (abs x)))
	(round 	 ,(lambda (x) (round x)))
	(=	 ,(lambda (x y) (= x y)))
	(<>	 ,(lambda (x y) (not (= x y))))
	(/ 	 ,(lambda (x y) (/ x y)))
        (+       ,(lambda (x y) (+ x y)))
	(- 	 ,(lambda (x y) (- x y)))
	(*	 ,(lambda (x y) (* x y)))
        (^       ,(lambda (x y) (expt x y)))
        (ceil    ,(lambda (x) (ceiling x)))
        (exp     ,(lambda (x) (exp x)))
        (floor   ,(lambda (x) (floor x)))
        (log     ,(lambda (x) (log x)))
	(sqrt    ,(lambda (x) (sqrt x)))
	(>	 ,(lambda (x y) (> x y)))
	(>=	 ,(lambda (x y) (>= x y)))
	(<	 ,(lambda (x y) (< x y)))
	(<=	 ,(lambda (x y) (<= x y)))
	(e	  2.718281828459045235360287471352662497757247093)
	(pi	  3.141592653589793238462643383279502884197169399)
     )
)
;;Takes in a list of expressions
;;and evalutes it to determine what to print	 
(define (printing line)
;;	(printf "~s~n" line)
;;	(printf "cdr is: ~s~n" (cdr line))
;;	(printf "caar is:  ~s~n" (caar line))
;;	(when (> (length line) 2) (printf "~a" (eval-expr (car line))) (printing (cdr line)))
	(cond
		[(null? line) (printf "~n")]
;;		[(integer? (car line)) (printf "~s~n" (car line))]
	;;	[(= (length line)2) (printf "~a~a~n"(eval-expr (car line)) (eval-expr (cadr line)))]
		[(and (pair? (car line)) (vector? (eval-expr(caar line)))) 
			(printf "~a~n" (vector-ref(eval-expr(caar line)) (-(eval-expr (cadar line)) 1)))]
		[(>= (length line)2) (printf "~a"(eval-expr (car line))) (printing(cdr line))]
		[(and (integer? (car line)) (= (length line) 1)) (printf "~a~n" (car line))]
		[(and (string? (car line)) (= (length line) 1)) (printf "~a~n" (car line))]
		[(and (pair? (car line))(vector? (eval-expr(caar line))))
			 (printf "~a~n"(vector-ref (eval-expr(caar line)) (- (eval-expr (cadar line)) 1)))]
		[(= (length line)1) (printf"~a~n" (eval-expr (car line)))]
		
	)
;;	(printf "got through the printing stage~n")
)

(define (eval-let line)
;;	(printf "Line is: ~s key is: ~s and value is ~s~n" line (eval-expr(car line)) (eval-expr(cadr line)))
;;	;;if car line is a list 
;;	;;and if 
	(if (pair? (car line))  
		(cond 
			[(vector? (eval-expr(caar line)))
			;; (printf "~s~n" (vector? (eval-expr(caar line))))]
			 (vector-set! (eval-expr(caar line)) (-(eval-expr (cadar line)) 1) (eval-expr (cadr line)))]
		)
		#f
	)
	(function-put! (car line) (eval-expr (cadr line)))
)

(define (eval-dim line) 
;;	(printf "Name is:~s Size is:~s~n" (car line) (eval-expr (cadr line)))
	(function-put! (car line) (make-vector (eval-expr(cadr line))))
;;	(printf "is (car line) a vector?: ~s~n" (vector? (eval-expr(car line))))
)

;;evalutes expressions
(define (eval-expr expr)
;;(printf "expression is ~s~n" expr)
	
	(cond
		[(string? expr) expr]	
		[(or (integer? expr) (rational? expr)) expr]
		[(symbol? expr) (hash-ref *function-table* expr #f)]
		;;						(1st element) (2nd element) (3rd element)
		[(and (= (length expr) 2) (eq? (car expr) 'log)) ((eval-expr(car expr))(+ (eval-expr(cadr expr))0.0) )]
	;;	[(and (= (length expr) 3) (eq? (car expr) '/) (= (eval-expr(caddr expr)) 0)) "NaN"]
		[(= (length expr) 3) ((hash-ref *function-table* (car expr)) (eval-expr(cadr expr)) (eval-expr(caddr expr)))]
		[(and (= (length expr) 2) (eq? (car expr) '+)) (+ (eval-expr(cadr expr)) 0.0)]
		[(and (= (length expr) 2) (eq? (car expr) '-)) (- 0.0 (eval-expr(cadr expr)))]
		[(and (hash-has-key? *function-table* (car expr)) (vector?(function-get(car expr))))
			(vector-ref(function-get(car expr))
		;;		(if (symbol? (cadr expr)) (cadr expr)
							(- (eval-expr(cadr expr)) 1))
		]	
		[(= (length expr) 2) ((hash-ref *function-table* (car expr)) (eval-expr(cadr expr))) ]
	)
;;	(printf "got through expr~n")
)
	
	
(define (eval-goto line)
;;	(printf "goto the line: ~s~n"line )
	(interpret-prgm (hash-ref *label-table* line))
)

(define (eval-if line)
;;	(printf "line is :~s~n" line)
;;	(printf "input count is now ~s~n" (function-get 'inputcount))
;;	(printf "(-1) is evaluated into ~s~n" (eval-expr (caddar line)))
;;	(printf "inputcount is evaluated into~s~n" (eval-expr (cadar line)))
	(cond 
		[((hash-ref *function-table* (caar line)) (eval-expr(cadar line)) (eval-expr(caddar line)))
		 (interpret-prgm (hash-ref *label-table* (cadr line)))]
	)
)


(define (read-inputs line)
;;	(printf "line is ~s~n" line)
	(when (not (null? (car line)))
		(function-put! (car line) void)
;;			(printf "before read ~n")
			(let ((expr (read)))
;;				(printf "~s~n" expr)
				(cond 
					[(eof-object? expr) (function-put! 'inputcount -1)];; (printf "this ran~n")]
					[(integer? expr) (function-put! 'inputcount (+ (function-get 'inputcount) 1))
							(function-put! (car line) expr)];; (printf "File being inputted~s~n" (car line))]
					[else (printf "this is an invalid input~n")(read-inputs line)]
				)
;;				(printf "Expr is represented as ~s~n" expr)
			)
		)
		(when (and (not (null? (cdr line))) (> (function-get 'inputcount) 0)) (read-inputs (cdr line)))
)

(define (eval-input line)
;;i	(printf "~s~n" line)
;;	(when (hash-has-key? *function-table* 'inputcount) (read-inputs line))
	(function-put! 'inputcount 0)
;;	(printf "input count resetted?~s~n" (function-get 'inputcount))
	(read-inputs line) 
)

(define (parse-line line)
	;;(printf "~s~n" line)
;;	(printfnput count is now ~s~n" (function-get 'inputcount))
	(cond
		[(null? (cdr line)) (void)]
		[(symbol? (cadr line)) (parse-line (cdr line))]
		[(eq? (caadr line) 'print) (printing (cdadr line))]
		[(eq? (caadr line) 'let) (eval-let (cdadr line))]
		[(eq? (caadr line) 'dim) (eval-dim (cadadr line))]
		[(eq? (caadr line) 'goto) (eval-goto (cadadr line))]	
		[(eq? (caadr line) 'if) (eval-if (cdadr line))]
;;		[(and (eq? (caadr line) 'input) (hash-has-key? *function-table* 'inputcount)) (read-inputs (cdadr line))]
;;		[(and (eq? (caadr line) 'input) (hash-has-key? *function-table* 'inputcount)(=(function-get 'inputcount) -1)) (eval-input line)]
		[(eq? (caadr line) 'input) (eval-input(cdadr line))]
	)
;;	(printf "got through the parse-line stage~n")
)

(define (interpret-prgm file) 
	(when (null? file) (exit))
;;	(printf "file sent to parse-line is: ~s~n" (car file))
	(parse-line (car file))
;;	(printf "finished parse-line~n")	
	(when (not (null? file))
		(interpret-prgm (cdr file)))	
)

; the file and puts it in  a list
;;it then proceeds to put any labels into a hashtable
(define (hashing file)

;;	(printf "~s~n" (car file))
	(when (not (null? file))
;		(printf "~s~n" (car file)) 
		(when (and (not (null? (cdar file))))
			(when (symbol? (cadar file))
;;				(printf "put into *label-table* ~s~n" (cadar file))
				(hash-set! *label-table* (cadar file) file)))
;;		(when (and (= (length file) 1) (not (null? (car file))))
;;			(when (symbol? (cadar file))
;				(printf "put into *lable-table* ~s~n" (cadar file))
;;				(hash-set! *label-table* (cadar file) (file))))
			
	(hashing (cdr file)))
)



(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
	;reads program form input file
          (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile))) 
		(hashing program)
		(interpret-prgm program)
	;;	(write-program-by-line sbprogfile program)	

;;		(printf "got here~n")
		)))

;(when (terminal-port? *stdin*)
     (main (vector->list (current-command-line-arguments)))
