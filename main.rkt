#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         racket/string
         rx)

(provide language-tag-match
         language-tag?
         normal-use?
         private-use?
         grandfathered?
         language-part?
         language-script-part?
         language-region-part?
         language-variant-part?
         language-extension-part?
         language-private-use-part?)

(define group-sep-char "-")

(define (apply-prefix prefix lst)
  (map (λ (name) (string-append prefix name)) lst))

(define bcp47-grandfathered-irregular
  (rx/or-group "en-GB-oed"
               "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak" "i-klingon"
               "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay" "i-tsu"
               "sgn-BE-FR" "sgn-BE-NL" "sgn-CH-DE"))

(define bcp47-grandfathered-regular
  (rx/or-group "art-lojban"
               "cel-gaulish"
               "no-bok" "no-nyn"
               "zh-guoyu" "zh-hakka" "zh-min" "zh-min-nan" "zh-xiang"))

(define bcp47-tl-grandfathered
  (rx/or-group bcp47-grandfathered-irregular bcp47-grandfathered-regular))

(define bcp47-language-part
  (rx/or-group
   (rx/and-group (rx/repeat rx/match-alpha #:lower 2 #:upper 3)
                 (rx/and-group group-sep-char
                               (rx/and-group (rx/repeat rx/match-alpha
                                                        #:lower 3 #:upper 3)
                                             (rx/repeat (rx/and-group group-sep-char
                                                                      (rx/repeat rx/match-alpha
                                                                                 #:lower 3 #:upper 3))
                                                        #:upper 2))
                               #:repeat 'optional))
   (rx/repeat rx/match-alpha #:lower 4 #:upper 4)
   (rx/repeat rx/match-alpha #:lower 5 #:upper 8)))

(define bcp47-script-part
  (rx/group (rx/repeat rx/match-alpha #:lower 4 #:upper 4)))

(define bcp47-script
  (rx/and-group group-sep-char
                bcp47-script-part
                #:repeat 'optional))

(define bcp47-region-part
  (rx/or-group (rx/repeat rx/match-alpha #:lower 2 #:upper 2)
               (rx/repeat rx/match-digit #:lower 3 #:upper 3)))

(define bcp47-region
  (rx/and-group group-sep-char
                bcp47-region-part
                #:repeat 'optional))

(define bcp47-variant-part
  (rx/or-group (rx/repeat rx/match-alnum #:lower 5 #:upper 8)
               (rx/and rx/match-digit
                       (rx/repeat rx/match-alnum #:lower 3 #:upper 3))))

(define bcp47-variant
  (rx/group
   (rx/and-group group-sep-char
                 bcp47-variant-part
                 #:repeat 'zero-or-more)))

(define bcp47-extension-part
  (rx/and-group (rx/match rx/range-digit
                          (rx/range #\A #\W)
                          (rx/range #\Y #\Z)
                          (rx/range #\a #\w)
                          (rx/range #\y #\z))
                (rx/and-group "-"
                              (rx/repeat rx/match-alnum
                                         #:lower 2 #:upper 8)
                              #:repeat 'one-or-more)))

(define bcp47-extension
  (rx/group
   (rx/and-group group-sep-char
                 bcp47-extension-part
                 #:repeat 'zero-or-more)))

(define bcp47-private-use-part
  (rx/and-group "x"
                (rx/and-group group-sep-char
                              (rx/repeat rx/match-alnum
                                         #:lower 1 #:upper 8)
                              #:repeat 'one-or-more)))

(define bcp47-private-use
  (rx/and-group group-sep-char bcp47-private-use-part #:repeat 'optional))

(define bcp47-tl-language
  (rx/and-group bcp47-language-part
                bcp47-script
                bcp47-region
                bcp47-variant
                bcp47-extension
                bcp47-private-use))

(define bcp47-tl-private-use
  (rx/and-group "x"
                (rx/and-group group-sep-char
                              (rx/repeat rx/match-alnum
                                         #:lower 1 #:upper 8)
                              #:repeat 'one-or-more)))

(define bcp47-pattern
  (pregexp
   (rx/string-exactly
    (rx/or-group
     bcp47-tl-grandfathered
     bcp47-tl-language
     bcp47-tl-private-use))))

;; -------------------------------------------------------------------------------------------------

(define/contract (normal-use? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (normal-use? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-tl-language)) val)))))

(define/contract (private-use? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (private-use? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-tl-private-use)) val)))))

(define/contract (grandfathered? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (grandfathered? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-tl-grandfathered)) val)))))

(define/contract (language-part? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-part? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-language-part)) val)))))

(define/contract (language-script-part? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-script-part? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-script-part)) val)))))

(define/contract (language-region-part? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-region-part? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-region-part)) val)))))

(define/contract (language-variant-part? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-variant-part? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-variant-part)) val)))))

(define/contract (language-extension-part? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-extension-part? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-extension-part)) val)))))

(define/contract (language-private-use-part? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-private-use-part? (symbol->string val))
      (not (false? (regexp-match (pregexp (rx/string-exactly bcp47-private-use-part)) val)))))

;; -------------------------------------------------------------------------------------------------

(define/contract (language-tag? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-tag? (symbol->string val))
      (not (false? (regexp-match bcp47-pattern val)))))

(define (language-property matches group name (skip 0))
  (let ((value (vector-ref matches group)))
    (if (non-empty-string? value)
        (list (cons name (if (> skip 0) (substring value 1) value)))
        '())))

(define (language-properties matches)
  (append (language-property matches 6 'language)
          (language-property matches 12 'script)
          (language-property matches 14 'region)
          (language-property matches 15 'variant 1)
          (language-property matches 18 'extension 1)
          (language-property matches 23 'private-use)
          ))

(define (language-tag-match val)
  (let ((match-list (regexp-match bcp47-pattern val)))
    (if match-list
        (let ((matches (list->vector match-list)))
          ;; debug: (displayln (map cons (range (length match-list)) match-list))
          (cond
            ((vector-ref matches 3) (list 'grandfathered-i (vector-ref matches 3)))
            ((vector-ref matches 4) (list 'grandfathered-r (vector-ref matches 4)))
            ((vector-ref matches 5) (list 'lang (vector-ref matches 5) (language-properties matches)))
            ((vector-ref matches 25) (list 'private-use (vector-ref matches 25)))
            (else '(error))))
        #f)))

;; -------------------------------------------------------------------------------------------------
;; In-Module Tests
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  (provide language-tag-match-test-suite
           language-tag-predicate-test-suite)

  ;; -----------------------------------------------------------------------------------------------
  ;; Test Suite(s)
  ;; -----------------------------------------------------------------------------------------------

  (define language-tag-match-test-suite
    (test-suite
     "Module langtag > function language-tag-match"

     (test-case
         "grandfathered - irregular"
       (check-equal? (language-tag-match "i-klingon")
                     '(grandfathered-i "i-klingon")))

     (test-case
         "grandfathered - regular"
       (check-equal? (language-tag-match "zh-min")
                     '(grandfathered-r "zh-min")))

     (test-case
         "language, 2-char"
       (check-equal? (language-tag-match "en")
                     '(lang "en" ((language . "en")))))

     (test-case
         "language w/region"
       (check-equal? (language-tag-match "en-US")
                     '(lang "en-US" ((language . "en") (region . "US")))))

     (test-case
         "language w/script"
       (check-equal? (language-tag-match "sr-Latn")
                     '(lang "sr-Latn" ((language . "sr") (script . "Latn")))))

     (test-case
         "language w/script+region"
       (check-equal? (language-tag-match "sr-Latn-RS")
                     '(lang "sr-Latn-RS" ((language . "sr") (script . "Latn") (region . "RS")))))

     (test-case
         "language w/numeric region"
       (check-equal? (language-tag-match "es-419")
                     '(lang "es-419" ((language . "es") (region . "419")))))

     (test-case
         "language w/variant"
       (check-equal? (language-tag-match "sl-nedis")
                     '(lang "sl-nedis" ((language . "sl") (variant . "nedis")))))

     (test-case
         "language w/region+variant"
       (check-equal? (language-tag-match "de-CH-1996")
                     '(lang "de-CH-1996" ((language . "de") (region . "CH") (variant . "1996")))))

     (test-case
         "language, extended"
       (check-equal? (language-tag-match "zh-cmn")
                     '(lang "zh-cmn" ((language . "zh-cmn"))))
       (check-equal? (language-tag-match "cmn") '(lang "cmn" ((language . "cmn")))))

     (test-case
         "private-use"
       (check-equal? (language-tag-match "x-abcde")
                     '(private-use "x-abcde")))

     (test-case
         "language w/script"
       (check-equal? (language-tag-match "en-Latn")
                     '(lang "en-Latn" ((language . "en") (script . "Latn")))))

     (test-case
         "language w/region+variant"
       (check-equal? (language-tag-match "en-US-boont")
                     '(lang "en-US-boont" ((language . "en") (region . "US") (variant . "boont")))))

     (test-case
         "language w/region+variants"
       (check-equal? (language-tag-match "en-US-boont-boons")
                     '(lang "en-US-boont-boons" ((language . "en") (region . "US") (variant . "boont-boons")))))

     (test-case
         "language w/extension"
       (check-equal? (language-tag-match "de-a-value")
                     '(lang "de-a-value" ((language . "de") (extension . "a-value")))))

     (test-case
         "language w/extension+private"
       (check-equal? (language-tag-match "en-a-bbb-x-a-ccc")
                     '(lang "en-a-bbb-x-a-ccc" ((language . "en") (extension . "a-bbb") (private-use . "x-a-ccc")))))

     (test-case
         "language w/extension"
       (check-equal? (language-tag-match "en-a-bbb-b-ccc")
                     '(lang "en-a-bbb-b-ccc" ((language . "en") (extension . "a-bbb-b-ccc")))))

     ))

  (define language-tag-predicate-test-suite
    (test-suite
     "Module langtag > predicates"

     (for-each (λ (v) (test-case (format "function language-tag? > ~s" v) (check-true (language-tag? v))))
               '("en-GB-oed"
                 "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak" "i-klingon"
                 "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay" "i-tsu"
                 "sgn-BE-FR" "sgn-BE-NL" "sgn-CH-DE"
                 ;; -------------------------------------------------------------------
                 "art-lojban"
                 "cel-gaulish"
                 "no-bok" "no-nyn"
                 "zh-guoyu" "zh-hakka" "zh-min" "zh-min-nan" "zh-xiang"
                 ;; -------------------------------------------------------------------
                 "en" "en-US" "en-Latn" "en-Latn-US" "sr-Latn" "sr-Latn-RS" "zh-cmn"
                 "es-419"
                 "sl-nedis" "de-CH-1996" "en-US-boont"
                 "de-a-value" "en-a-bbb-x-a-ccc" "en-a-bbb-b-ccc"
                 ;; -------------------------------------------------------------------
                 "x-abcde"))

     (for-each (λ (v) (test-case (format "function grandfathered? > ~s" v) (check-true (grandfathered? v))))
               '("en-GB-oed"
                 "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak" "i-klingon"
                 "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay" "i-tsu"
                 "sgn-BE-FR" "sgn-BE-NL" "sgn-CH-DE"
                 ;; -------------------------------------------------------------------
                 "art-lojban"
                 "cel-gaulish"
                 "no-bok" "no-nyn"
                 "zh-guoyu" "zh-hakka" "zh-min" "zh-min-nan" "zh-xiang"))

     (for-each (λ (v) (test-case (format "function private-use? > ~s" v)
                        (check-true (private-use? v))))
               '("x-private"))

     (for-each (λ (v) (test-case (format "function language? > ~s" v)
                        (check-true (normal-use? v))))
               '("en" "en-US" "en-Latn" "en-Latn-US" "sr-Latn" "sr-Latn-RS" "zh-cmn"
                 "es-419"
                 "sl-nedis" "de-CH-1996" "en-US-boont"
                 "de-a-value" "en-a-bbb-x-a-ccc" "en-a-bbb-b-ccc"))

     (for-each (λ (v) (test-case (format "function language-part? > ~s" v)
                        (check-true (language-part? v))))
               '("en" "sr" "zh-cmn"))

     (for-each (λ (v) (test-case (format "function language-script-part? > ~s" v)
                        (check-true (language-script-part? v))))
               '("Latn"))

     (for-each (λ (v) (test-case (format "function language-region-part? > ~s" v)
                        (check-true (language-region-part? v))))
               '("US" "RS" "CH" "419"))

     (for-each (λ (v) (test-case (format "function language-variant-part? > ~s" v)
                        (check-true (language-variant-part? v))))
               '("nedis" "boont" "1996"))

     ))

  ;; -----------------------------------------------------------------------------------------------
  ;; Test Runner
  ;; -----------------------------------------------------------------------------------------------

  (run-tests language-tag-match-test-suite)
  (run-tests language-tag-predicate-test-suite))
