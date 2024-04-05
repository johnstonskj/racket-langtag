# Racket Package langtag

BCP-47 compliant language tag

[![raco pkg install langtag](https://img.shields.io/badge/raco%20pkg%20install-langtag-blue.svg)](http://pkgs.racket-lang.org/package/langtag)
[![Documentation](https://img.shields.io/badge/raco%20docs-langtag-blue.svg)](http://docs.racket-lang.org/langtag/index.html)
[![Racket](https://github.com/johnstonskj/racket-langtag/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-langtag/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-langtag.svg?style=flat-square)](https://github.com/johnstonskj/racket-langtag/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-langtag.svg)](https://github.com/johnstonskj/racket-langtag/stargazers)

## Example

```racket
(require langtag)

(language-tag? "en")          ;; => #t
(language-tag? "en-US")       ;; => #t
(language-tag? "en-US-boont") ;; => #t
(language-tag? "en-Latn-US")  ;; => #t
(language-tag? "i-klingon")   ;; => #t
(language-tag? "x-private")   ;; => #t
```

## Changes

TBD
