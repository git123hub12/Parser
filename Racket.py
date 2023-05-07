
#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define scanner
(lexer
   ["read"  ( cons `(read , "read") (scanner input-port))] 
   ["write"  ( cons `(write, "write") (scanner input-port))]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
   
    (cons `(ID ,(string->symbol lexeme))
          (scanner input-port))]
   [#\( 
    (cons `(LP,(string->symbol lexeme))
          (scanner input-port))]
   [#\)
    (cons `(RP ,(string->symbol lexeme))
          (scanner input-port))]

   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    (cons `(INT ,(string->number lexeme))
          (scanner input-port))]
   
   [#\/ (cons `(div ,(string->symbol lexeme))(scanner input-port))] 
   [#\+ (cons `(add ,(string->symbol lexeme))(scanner input-port))] 
   [#\-(cons `(sub ,(string->symbol lexeme))(scanner input-port))]
   [#\* (cons `(mult ,(string->symbol lexeme))(scanner input-port))]

  [":="  ( cons `(assign, ":=")(scanner input-port))] 
  
    
      ["$$"  `((EOF, "$$"))]
   [whitespace  (scanner input-port)]
   
   [(eof)
    '()]))
(define Files (scanner(open-input-file "input01.txt")))
    Files
;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;;parser

(define (match sign scan)
  (cond
  [(equal? sign (first (first scan))) 
      (rest scan)]
     [else (error "error")]))

(define (program scanner)
  (cond 
    [(or (equal? (first(first scanner)) `ID)
          (equal? (first(first scanner)) `read)
          (equal? (first(first scanner)) `write)
          (equal? (first(first scanner)) `EOF))
          (match `EOF (stmt_list scanner))]
    [else (error "error with the program")]))
     
 
(define (stmt_list scanner)
  ( cond
   [(or (equal? (first(first scanner)) `ID)
        (equal? (first(first scanner)) `read)
        (equal? (first(first scanner)) `write))
        (stmt_list (stmt scanner))]
   [(equal? (first(first scanner)) `EOF) scanner]

    [else (error "error")]))

(define (stmt scanner)
  (cond
    [(equal? (first(first scanner)) `ID)
     (expr (match `assign (match `ID scanner)))]
    [(equal? (first(first scanner)) `read)
    (match `ID (match `read (scanner)))]
    [(equal? (first(first scanner)) `write)
    (expr (match `write scanner))]
    [else (error "error")]))

(define (expr scanner)
  (cond
    [(or (equal? (first(first scanner)) `ID)
        (equal? (first(first scanner)) `INT)
        (equal? (first(first scanner)) `LP))
     (term_tail (term scanner))]
   [else (error "error")]))


(define (term_tail scanner) (displayln scanner)(displayln (first(first scanner)))
  (cond
    [(or (equal? (first(first scanner)) `add)  
        (equal? (first(first scanner)) `sub))
     (term_tail( term ( add_opp scanner)))]
    
     [(or ( equal? (first(first scanner)) `RP)
          (equal? (first(first scanner)) `ID)
          (equal? (first(first scanner)) `read)
          (equal? (first(first scanner)) `write)
          (equal? (first(first scanner)) `EOF))
          scanner]
     [else (error "error")]))

(define (term scanner) 
  (cond
    [(or (equal? (first(first scanner)) `ID)
       (equal? (first(first scanner)) `INT)
      (equal? (first(first scanner)) `LP))
     (factor_tail( factor scanner))]
     [else (error "error")]))


(define (factor_tail scanner)
  (cond
    [(or (equal? (first(first scanner))`mult)
        (equal? (first(first scanner)) `div))
     (factor_tail( factor ( mult_opp scanner)))]
    
     [(or ( equal? (first(first scanner)) `RP)
          ( equal? (first(first scanner)) `add)
          ( equal? (first(first scanner)) `sub)
          (equal? (first(first scanner)) `ID)
          (equal? (first(first scanner)) `read)
          equal? (first(first scanner)) `write)
          (equal? (first(first scanner)) `EOF)
          scanner]
     [else (error "error")]))

(define (factor scanner) 
  (cond
     [ ( equal? (first(first scanner)) `ID)
          (match `ID scanner)]
    
     [(equal? (first(first scanner)) `INT)
          (match `INT scanner)]
      [(equal? (first(first scanner)) `LP)
      (match  `RP (expr(match `LP scanner)))]
        [else (error "error")]))

 (define (add_opp scanner)
 (cond
    [( equal? (first(first scanner)) `add)
          (match `add scanner)]
    [ ( equal? (first(first scanner)) `sub)
          (match `sub scanner)]
    [else (error "error")]))

 (define (mult_opp scanner)
   (cond
      [ ( equal? (first(first scanner)) `mult)
          (match `mult scanner)]
    [ ( equal? (first(first scanner)) `div)
          (match `div scanner)]
    [else (error "error")]))

(program (scanner (open-input-file "input01.txt")))

;(define tokenlist (scanner (open-input-file "input01.txt")))






;////////////////////////////
;  sources
;https://matt.might.net/articles/lexers-in-racket/
;the book page 76-77
;https://docs.racket-lang.org/guide/io-patterns.html
;https://docs.racket-lang.org/reference/if.html
;https://stackoverflow.com/questions/9857380/create-files-through-racket
;https://www.youtube.com/watch?v=crCHdXccRlY
