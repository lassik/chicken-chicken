(import (scheme base) (scheme char) (scheme file) (scheme write)
        (scheme process-context))

(current-output-port (current-error-port))

(define disassemble-at-start? #f)
(define disassemble-at-each-step? #f)

(define (make-vm program)
  (let ((v (vector-append (vector 0 "") (list->vector program) (vector 0))))
    (vector 2 (vector-length v) v)))

(define vm (make-parameter (make-vm '())))

(define (load-program program) (vm (make-vm program)))

(define (pc) (vector-ref (vm) 0))
(define (set-pc! new-pc) (vector-set! (vm) 0 new-pc))

(define (stack-depth) (vector-ref (vm) 1))
(define (set-stack-depth! new-depth) (vector-set! (vm) 1 new-depth))

(define (valid-address? i)
  (and (exact-integer? i) (>= i 0) (< i (stack-depth))))

(define (valid-address i)
  (if (valid-address? i) i (error "The chicken has escaped the coop" i)))

(define (stack-ref i)
  (vector-ref (vector-ref (vm) 2) (valid-address i)))

(define (stack-set! i value)
  (vector-set! (vector-ref (vm) 2) (valid-address i) value))

(define (make-room-for n)
  (let ((old-room (vector-length (vector-ref (vm) 2))))
    (when (< old-room n)
      (let loop ((new-room 64))
        (if (< new-room n)
            (loop (* new-room 2))
            (set! (vector-ref (vm) 2)
              (vector-append (vector-ref (vm) 2)
                             (make-vector (- new-room old-room) 0))))))))

(define (push x)
  (let* ((i (stack-depth))
         (n (+ i 1)))
    (make-room-for n)
    (set-stack-depth! n)
    (stack-set! i x)))

(define (pop)
  (let* ((n (- (stack-depth) 1))
         (x (stack-ref n)))
    (set-stack-depth! n)
    x))

(define (pop2)
  (let* ((b (pop))
         (a (pop)))
    (list a b)))

(define (input) (stack-ref 1))
(define (set-input! string) (stack-set! 1 string))

(define (fetch-opcode)
  (let ((opcode (stack-ref (pc))))
    (unless (and (exact-integer? opcode) (>= opcode 0))
      (error "Bad opcode" opcode))
    (set-pc! (+ (pc) 1))
    opcode))

(define num-special-ops 10)
(define special-ops (make-vector num-special-ops #f))
(define special-op-names (make-vector num-special-ops #f))

(define-syntax define-special-op
  (syntax-rules ()
    ((_ (opcode chicken-name name) body ...)
     (begin (vector-set! special-ops opcode (lambda () body ...))
            (vector-set! special-op-names opcode
                         (string-append (symbol->string 'chicken-name)
                                        " (" (symbol->string 'name) ")"))))))

(define-special-op (0 axe exit)
  (eof-object))

(define-special-op (1 chicken chicken)
  (push "chicken"))

(define-special-op (2 add add)
  (push (apply (lambda (a b)
                 (cond ((and (number? a) (number? b))
                        (+ a b))
                       ((and (number? a) (string? b))
                        (string-append (number->string a) b))
                       ((and (string? a) (number? b))
                        (string-append a (number->string b)))
                       ((and (string? a) (string? b))
                        (string-append a b))
                       (else
                        (error "Cannot add" a b))))
               (pop2))))

(define-special-op (3 fox subtract)
  (push (apply - (pop2))))

(define-special-op (4 rooster multiply)
  (push (apply * (pop2))))

(define-special-op (5 compare compare)
  (push (apply (lambda (a b) (if (equal? a b) 1 0))
               (pop2))))

(define-special-op (6 pick load)
  (push (if (= 1 (fetch-opcode))
            (stack-ref (pop))
            (stack-ref (pop)))))

(define-special-op (7 peck store)
  (apply (lambda (value address) (stack-set! address value))
         (pop2)))

(define-special-op (8 fr jump)
  (apply (lambda (condition offset)
           (unless (equal? condition 0)
             (set-pc! (+ (pc) offset))))
         (pop2)))

(define-special-op (9 BBQ char)
  (let ((i (pop)))
    (cond ((not (exact-integer? i))
           (error "BBQ non-integer" i))
          ((< i 0)
           (error "BBQ negative" i))
          ((> i 127)
           (error "BBQ bigger than ASCII" i))
          (else
           (push (string (integer->char i)))))))

(define (opcode-procedure opcode)
  (if (< opcode num-special-ops)
      (vector-ref special-ops opcode)
      (lambda () (push (- opcode num-special-ops)))))

(define (opcode-name opcode)
  (if (< opcode num-special-ops)
      (vector-ref special-op-names opcode)
      (string-append "push " (number->string (- opcode num-special-ops)))))

(define (display-opcode at opcode)
  (display "@")
  (write at)
  (display ": ")
  (display (opcode-name opcode))
  (newline))

(define (disassemble-program)
  (display "Disassembly starts\n")
  (let loop ((at 2))
    (when (valid-address? at)
      (display-opcode at (stack-ref at))
      (loop (+ at 1))))
  (display "Disassembly ends\n\n"))

(define (run)
  (let* ((at (pc)) (opcode (fetch-opcode)))
    (when disassemble-at-each-step? (display-opcode at opcode))
    ((opcode-procedure opcode))))

(define (run-program)
  (unless (eof-object? (run)) (run-program)))

(define (run-step n)
  (when (> n 0)
    (unless (eof-object? (run))
      (run-step (- n 1)))))

(define (read-program-as-chickens)
  (define (line-add line word)
    (if (null? word) line
        (let ((word (list->string (reverse word))))
          (if (equal? "chicken" word)
              (cons 'chicken line)
              (error "Not chicken" word)))))
  (define (lines-add lines line)
    (cons line lines))
  (let loop ((lines '()) (line '()) (word '()))
    (let ((char (read-char)))
      (cond ((eof-object? char)
             (reverse (lines-add lines (line-add line word))))
            ((eqv? #\newline char)
             (loop (lines-add lines (line-add line word)) '() '()))
            ((char-whitespace? char)
             (loop lines (line-add line word) '()))
            ((char-alphabetic? char)
             (loop lines line (cons char word)))
            (else
             (error "Not chicken or whitespace" char))))))

(define (read-program)
  (map length (read-program-as-chickens)))

(define (main args)
  (let-values
      (((source-file initial-input)
        (case (length args)
          ((1) (values (car args) ""))
          ((2) (values (car args) (cadr args)))
          (else (display "Usage: chicken-chicken source-file [input]\n")
                (exit #f)))))
    (load-program (with-input-from-file source-file read-program))
    (when disassemble-at-start? (disassemble-program))
    (set-input! (or (string->number initial-input) initial-input))
    (run-program)
    (display (pop))
    (newline)))

(main (cdr (command-line)))
