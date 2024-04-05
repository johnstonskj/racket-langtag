# Racket Package langtag

This package provides predicates that determines whether a given string is a valid *Language Tag* as defined
by RFC5646 and used across HTTP, HTML, XML, RDF, and much more.

[![raco pkg install langtag](https://img.shields.io/badge/raco%20pkg%20install-langtag-blue.svg)](http://pkgs.racket-lang.org/package/langtag)
[![Documentation](https://img.shields.io/badge/raco%20docs-langtag-blue.svg)](http://docs.racket-lang.org/langtag/index.html)
[![Racket](https://github.com/johnstonskj/racket-langtag/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-langtag/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-langtag.svg?style=flat-square)](https://github.com/johnstonskj/racket-langtag/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-langtag.svg)](https://github.com/johnstonskj/racket-langtag/stargazers)

## Example

TBD

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

**Version 1.0**

Initial release:

* Feature: a set of usable predicates for all aspects of the language tag.
* Feature: partial matcher that will break down most language tags.
  * Work to do to split variant and extensions into lists.
