#lang info
(define collection "langtag")
(define deps '("base" "rx"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib"))
(define scribblings '(("scribblings/langtag.scrbl" ())))
(define pkg-desc "BCP-47 compliant language tag.")
(define version "1.0")
(define pkg-authors '(johnstonskj))
(define license 'Apache-2.0)
