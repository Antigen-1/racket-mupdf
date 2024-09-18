#lang scribble/manual
@require[@for-label[racket-mupdf
                    racket/base
                    racket/draw
                    racket/class]]

@title{racket-mupdf}
@author{zhanghao}

@defmodule[racket-mupdf]

Racket version of MuPDF C API.

@section{Exceptions}

@defstruct[(exn:fail:mupdf exn:fail) () #:constructor-name make-exn/mupdf]

@section{Functions}

Currently this library only supports reading from PDF files. Allocated objects are automatically deallocated in this library.

@defproc[#:kind "constructor"
         (make-context)
         mupdf-context?]
@defproc[#:kind "constructor"
         (open-document (ctx mupdf-context?) (file path-string?))
         mupdf-document?]
@defproc[#:kind "constructor"
         (extract-pixmap (doc mupdf-document?) (pg-num exact-nonnegative-integer?) (ctm mupdf-matrix?))
         mupdf-pixmap?]{
If the document has @racket[pg-num] or fewer pages, then the @racket[exn:fail:contract] exception is raised.
}
@defproc[#:kind "constructor"
         (make-matrix (zoom1 flonum?) (zoom2 flonum?) (rotate flonum?))
         mupdf-matrix?]
@defproc[#:kind "accessor"
         (document-count-pages (doc mupdf-document?))
         exact-nonnegative-integer?]
@defproc[#:kind "transformer"
         (pixmap->bitmap (pix mupdf-pixmap?))
         (is-a/c bitmap%)]
@defproc[#:kind "predicate"
         (mupdf-context? (value any/c))
         boolean?]
@defproc[#:kind "predicate"
         (mupdf-document? (value any/c))
         boolean?]
@defproc[#:kind "predicate"
         (mupdf-pixmap? (value any/c))
         boolean?]
@defproc[#:kind "predicate"
         (mupdf-matrix? (value any/c))
         boolean?]
