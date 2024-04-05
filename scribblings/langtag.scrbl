#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          langtag
          (for-label racket/base
                     langtag))

@;{============================================================================}

@(define example-eval (make-base-eval '(require langtag)))

@;{============================================================================}

@title[#:version  "0.1.0"]{BCP-47 compliant language tag}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]
@defmodule[langtag]

@defproc[#:kind "predicate"
         (language-tag?
          [val (or/c symbol? string?)])
         boolean?]{
TBD

@examples[#:eval example-eval
(require langtag)

(language-tag? "en")
(language-tag? "en-US")
(language-tag? "en-US-boont")
(language-tag? "en-Latn-US")
(language-tag? "i-klingon")
(language-tag? "x-private")
]
}

@defproc[(language-tag-match
          [val (or/c symbol? string?)])
         (list symbol? string? (or/c (listof (cons/c symbol? string?)) none/c))]{
TBD

@examples[#:eval example-eval
(require langtag)

(language-tag-match "en")
(language-tag-match "en-US")
(language-tag-match "en-US-boont")
(language-tag-match "en-Latn-US")
(language-tag-match "i-klingon")
(language-tag-match "x-private")
]
}

@section[]{Appendix: Definition}

The syntax of the language tag, from [RFC5646], in ABNF [RFC5234] is:

@verbatim|{
 Language-Tag  = langtag             ; normal language tags
               / privateuse          ; private use tag
               / grandfathered       ; grandfathered tags

 langtag       = language
                 ["-" script]
                 ["-" region]
                 *("-" variant)
                 *("-" extension)
                 ["-" privateuse]

 language      = 2*3ALPHA            ; shortest ISO 639 code
                 ["-" extlang]       ; sometimes followed by
                                     ; extended language subtags
               / 4ALPHA              ; or reserved for future use
               / 5*8ALPHA            ; or registered language subtag

 extlang       = 3ALPHA              ; selected ISO 639 codes
                 *2("-" 3ALPHA)      ; permanently reserved

 script        = 4ALPHA              ; ISO 15924 code

 region        = 2ALPHA              ; ISO 3166-1 code
               / 3DIGIT              ; UN M.49 code

 variant       = 5*8alphanum         ; registered variants
               / (DIGIT 3alphanum)

 extension     = singleton 1*("-" (2*8alphanum))

                                     ; Single alphanumerics
                                     ; "x" reserved for private use
 singleton     = DIGIT               ; 0 - 9
               / %x41-57             ; A - W
               / %x59-5A             ; Y - Z
               / %x61-77             ; a - w
               / %x79-7A             ; y - z

 privateuse    = "x" 1*("-" (1*8alphanum))

 grandfathered = irregular           ; non-redundant tags registered
               / regular             ; during the RFC 3066 era

 irregular     = "en-GB-oed"         ; irregular tags do not match
               / "i-ami"             ; the 'langtag' production and
               / "i-bnn"             ; would not otherwise be
               / "i-default"         ; considered 'well-formed'
               / "i-enochian"        ; These tags are all valid,
               / "i-hak"             ; but most are deprecated
               / "i-klingon"         ; in favor of more modern
               / "i-lux"             ; subtags or subtag
               / "i-mingo"           ; combination
               / "i-navajo"
               / "i-pwn"
               / "i-tao"
               / "i-tay"
               / "i-tsu"
               / "sgn-BE-FR"
               / "sgn-BE-NL"
               / "sgn-CH-DE"

 regular       = "art-lojban"        ; these tags match the 'langtag'
               / "cel-gaulish"       ; production, but their subtags
               / "no-bok"            ; are not extended language
               / "no-nyn"            ; or variant subtags: their meaning
               / "zh-guoyu"          ; is defined by their registration
               / "zh-hakka"          ; and all of these are deprecated
               / "zh-min"            ; in favor of a more modern
               / "zh-min-nan"        ; subtag or sequence of subtags
               / "zh-xiang"

 alphanum      = (ALPHA / DIGIT)     ; letters and numbers
}|
