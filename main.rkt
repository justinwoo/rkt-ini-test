#lang curly-fn racket/base

(require megaparsack
         megaparsack/text
         data/applicative
         data/functor
         data/monad
         data/either
         racket/bool
         racket/string
         racket/function)

(define skip-ws
  (or/p (many+/p space/p)
        void/p))

(define (lexeme p)
  (do [x <- p]
      skip-ws
      (pure x)))

(define section-name
  (lexeme
   (do (char/p #\[)
       [cs <- (many+/p (satisfy/p #{not (equal? % #\])}))]
       (char/p #\])
       (pure (list->string cs)))))

(define field
  (lexeme
   (do [xs <- (many+/p (satisfy/p #{not (or (equal? % #\=)
                                            (equal? % #\[))}))]
       (char/p #\=)
       [ys <- (many+/p (satisfy/p #{not (equal? % #\newline)}))]
       (pure (cons (list->string xs)
                   (list->string ys))))))

(define section
  (lexeme
   (do [name <- section-name]
       [fields <- (many/p field)]
       (pure (cons name
                   (make-immutable-hash fields))))))

(define document
  (do skip-ws
      [xs <- (many/p section)]
      (pure (make-immutable-hash xs))))

(define string-value 'string)
(define integer-value 'number)
(define boolean-value 'boolean)
(define string-array-value 'string-array)

(define (read-field-from-field-spec schema value)
  (case schema
    ['string (pure value)]
    ['number (let ([x (string->number value 10 'number-or-false)])
                  (if (false? x)
                      (failure (string-append "Couldn't convert to integer: " value))
                      (pure x)))]
    ['boolean (case (string-downcase value)
                [("false") (pure #f)]
                [("true") (pure #t)]
                [else (failure (string-append "Couldn't convert to boolean: " value))])]
    ['string-array (pure (string-split value))]
    [else (failure (string-append "Unknown read schema: " (symbol->string schema)))]))

(define (generic-inner-hash-operation f level-name parent-name)
  (lambda (schema section)
    (define (list-either-to-either-list item acc)
      (do [xs <- acc]
          [x <- item]
          (pure (cons x xs))))
    (map make-immutable-hash
         (foldl list-either-to-either-list (success '())
                (hash-map schema
                          (lambda (key val)
                            (let ([x (hash-ref section key #f)])
                              (if (false? x)
                                  (failure (string-append "Could not find " level-name " " key " in " parent-name))
                                  (map #{cons key %} (f val x))))))))))

(define read-fields-from-section-spec
  (generic-inner-hash-operation read-field-from-field-spec
                                "field"
                                "section"))

(define read-sections-from-document-spec
  (generic-inner-hash-operation read-fields-from-section-spec
                                "section"
                                "document"))

(define (parse-ini-document schema s)
  (do [parsed <- (parse-string document s)]
      (read-sections-from-document-spec schema parsed)))

;; test
(read-fields-from-section-spec
 '#hash(("apple" . number))
 '#hash(("apple" . "banana")))
;; (failure "Couldn't convert to integer: banana")

(read-fields-from-section-spec
 '#hash(("apple" . string))
 '#hash(("apple" . "banana")))
;; (success '#hash(("apple" . "banana")))

(read-sections-from-document-spec
 '#hash(["apple" . #hash(["kiwi" . string] ["grape" . number])])
 '#hash(["apple" . #hash(["kiwi" . "watermelon"])]))
;; (failure "Could not find field grape in section")

(read-sections-from-document-spec
 '#hash(["apple" . #hash(["kiwi" . string] ["grape" . number])])
 '#hash(["apple" . #hash(["kiwi" . "watermelon"] ["grape" . "123"])]))
;; (success '#hash(("apple" . #hash(("kiwi" . "watermelon") ("grape" . 123)))))

(parse-string section-name "[sdafs]    ")
(parse-string field "asdfsadf=field\n")
(parse-string section "[apple]kiwi=watermelon\ncherry=persimmon\n[banana]")
(parse-string document "[apple]\nkiwi=watermelon\ncherry=persimmon\n[banana]\ngrape=durian\n[cherry]")
;; (success "sdafs")
;; (success '("asdfsadf" . "field"))
;; (success '("apple" . #hash(("cherry" . "persimmon") ("kiwi" . "watermelon"))))
;; (success
;;   '#hash(("cherry" . #hash())
;;         ("apple" . #hash(("cherry" . "persimmon") ("kiwi" . "watermelon")))
;;         ("banana" . #hash(("grape" . "durian")))))

(read-field-from-field-spec 'string "apple")
(read-field-from-field-spec 'number "apple")
(read-field-from-field-spec 'number "10")
(read-field-from-field-spec 'boolean "apple")
(read-field-from-field-spec 'boolean "true")
(read-field-from-field-spec 'boolean "false")
(read-field-from-field-spec 'unknown "apple")
;; (pure "apple")
;; (failure "Couldn't convert to integer: apple")
;; (pure 10)
;; (failure "Couldn't convert to boolean: apple")
;; (pure #t)
;; (pure #f)
;; (failure "Unknown read schema: unknown")

(parse-ini-document
 '#hash(["apple" . #hash(["kiwi" . string] ["grape" . number])])
 "[apple]\ngrape=123")
(parse-ini-document
 '#hash(["apple" . #hash(["kiwi" . string] ["grape" . number])])
 "[apple]\nkiwi=watermelon\ngrape=123")
;; (failure "Could not find field kiwi in section")
;; (success '#hash(("apple" . #hash(("kiwi" . "watermelon") ("grape" . 123)))))

