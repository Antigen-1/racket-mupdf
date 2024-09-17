#lang racket/base
(provide (struct-out exn:fail:mupdf))
(struct exn:fail:mupdf exn:fail ()
  #:constructor-name make-exn/mupdf)
