#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         racket/string
         rx)

(provide language-tag-match
         language-tag?)

(define group-sep-char "-")

(define (apply-prefix prefix lst)
  (map (λ (name) (string-append prefix name)) lst))

(define bcp47-grandfathered-group-1
  (rx/or-group "en-GB-oed"
               "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak" "i-klingon"
               "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay" "i-tsu"
               "sgn-BE-FR" "sgn-BE-NL" "sgn-CH-DE"))

(define bcp47-grandfathered-group-2
  (rx/or-group "art-lojban"
               "cel-gaulish"
               "no-bok" "no-nyn"
               "zh-guoyu" "zh-hakka" "zh-min" "zh-min-nan" "zh-xiang"))

(define bcp47-language
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

(define bcp47-script
  (rx/and-group group-sep-char
                (rx/group (rx/repeat rx/match-alpha
                                     #:lower 4 #:upper 4))
                #:repeat 'optional))

(define bcp47-region
  (rx/and-group group-sep-char
                (rx/or-group (rx/repeat rx/match-alpha
                                        #:lower 2 #:upper 2)
                             (rx/repeat rx/match-alnum
                                        #:lower 3 #:upper 3))
                #:repeat 'optional))

;; Hard to capture as it's a repeating pattern.
(define bcp47-variant
  (rx/group
   (rx/and-group group-sep-char
                 (rx/or-group (rx/repeat rx/match-alnum
                                         #:lower 5 #:upper 8)
                              (rx/and rx/match-digit
                                      (rx/repeat rx/match-alnum
                                                 #:lower 3 #:upper 3)))
                 #:repeat 'zero-or-more)))

;; Hard to capture as it's a repeating pattern.
(define bcp47-extension
  (rx/group
   (rx/and-group group-sep-char
                 (rx/and-group (rx/match rx/range-digit
                                         (rx/range #\A #\W)
                                         (rx/range #\Y #\Z)
                                         (rx/range #\a #\w)
                                         (rx/range #\y #\z))
                               (rx/and-group "-"
                                             (rx/repeat rx/match-alnum
                                                        #:lower 2 #:upper 8)
                                             #:repeat 'one-or-more))
                 #:repeat 'zero-or-more)))

(define bcp47-private-use
  (rx/and-group group-sep-char
                (rx/and-group "x"
                              (rx/and-group group-sep-char
                                            (rx/repeat rx/match-alnum
                                                       #:lower 1 #:upper 8)
                                            #:repeat 'one-or-more))
                #:repeat 'optional))

(define bcp47-private-use-1
  (rx/and-group "x"
                (rx/and-group group-sep-char
                              (rx/repeat rx/match-alnum
                                         #:lower 1 #:upper 8)
                              #:repeat 'one-or-more)))

(define bcp47-pattern
  (pregexp
   (rx/string-exactly
    (rx/or-group                                ;; group 2
     (rx/or-group bcp47-grandfathered-group-1   ;; group 3
                  bcp47-grandfathered-group-2)  ;; group 4
     (rx/and-group bcp47-language               ;; group 5 -> 6,9
                   bcp47-script                 ;; group 12
                   bcp47-region                 ;; group 14
                   bcp47-variant                ;; group 15
                   bcp47-extension              ;; group 18
                   bcp47-private-use)           ;; group 23
     bcp47-private-use-1))))                    ;; group 25

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
            ((vector-ref matches 3) (list 'grandfathered-1 (vector-ref matches 3)))
            ((vector-ref matches 4) (list 'grandfathered-2 (vector-ref matches 4)))
            ((vector-ref matches 5) (list 'lang (vector-ref matches 5) (language-properties matches)))
            ((vector-ref matches 25) (list 'private-use (vector-ref matches 25)))
            (else '(error))))
        #f)))

(define/contract (language-tag? val)
  (-> (or/c symbol? string?) boolean?)
  (if (symbol? val)
      (language-tag? (symbol->string val))
      (not (false? (regexp-match bcp47-pattern val)))))

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
         "grandfathered group 1"
       (check-equal? (language-tag-match "i-klingon")
                     '(grandfathered-1 "i-klingon")))

     (test-case
         "grandfathered group 2"
       (check-equal? (language-tag-match "zh-min")
                     '(grandfathered-2 "zh-min")))

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
     "Module langtag > function language-tag?"

     (for-each (λ (v) (test-case (format "value: ~s" v) (check-true (language-tag? v))))
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
                 "x-abcde"))))

  ;; -----------------------------------------------------------------------------------------------
  ;; Test Runner
  ;; -----------------------------------------------------------------------------------------------

  (run-tests language-tag-match-test-suite)
  (run-tests language-tag-predicate-test-suite))
