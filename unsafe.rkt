#lang racket/base
(require ffi/unsafe ffi/unsafe/define ffi/unsafe/alloc
         racket/runtime-path
         "exn.rkt")
(provide make-context
         open-document
         document-count-pages
         make-matrix
         extract-pixmap
         pixmap-has-alpha?
         pixmap-offsets
         pixmap->bytes
         )

(define-runtime-path libpdf "libpdf.so")

(define pdf-lib (ffi-lib libpdf))
(define-ffi-definer define-pdf-lib pdf-lib)

(define _fz_context-pointer (_cpointer/null 'fz_context))
(define _fz_document-pointer (_cpointer/null 'fz_document))
(define _fz_pixmap-pointer (_cpointer/null 'fz_pixmap))

(define-cstruct _fz_matrix ([a _float] [b _float] [c _float] [d _float] [e _float] [f _float]))
(define-cstruct _offset ([x _int] [y _int]))

(define (check-pointer ptr str)
  (if ptr
      ptr
      (raise (make-exn/mupdf str (current-continuation-marks)))))
(define (check-exit-code code str)
  (if (= -1 code)
      (raise (make-exn/mupdf str (current-continuation-marks)))
      code))

(define-pdf-lib mupdf_new_context
  (_fun -> (r : _fz_context-pointer) ->
        (check-pointer r "Fail to create a context")))
(define-pdf-lib mupdf_register_type_handlers
  (_fun (c : _fz_context-pointer) -> (r : _int)
        -> (let ()
             (check-exit-code r "Fail to register type handlers")
             c)))
(define-pdf-lib mupdf_drop_context
  (_fun _fz_context-pointer -> _void))
(define-pdf-lib mupdf_open_document
  (_fun _fz_context-pointer _file -> (r : _fz_document-pointer)
        -> (check-pointer r "Fail to open the document")))
(define-pdf-lib mupdf_drop_document
  (_fun _fz_context-pointer _fz_document-pointer
        -> _void))
(define-pdf-lib mupdf_count_pages
  (_fun _fz_context-pointer _fz_document-pointer
        -> (r : _int)
        -> (check-exit-code r "Fail to count pages")))
(define-pdf-lib mupdf_new_matrix
  (_fun _float _float _float -> _fz_matrix))
(define-pdf-lib mupdf_new_rgb_pixmap_from_page_number
  (_fun _fz_context-pointer _fz_document-pointer _int _fz_matrix
        -> (r : _fz_pixmap-pointer)
        -> (check-pointer r "Fail to create a pixmap")))
(define-pdf-lib mupdf_drop_pixmap
  (_fun _fz_context-pointer _fz_pixmap-pointer
        -> _void))
(define-pdf-lib mupdf_pixmap_alphap
  (_fun _fz_pixmap-pointer -> _stdbool))
(define-pdf-lib mupdf_pixmap_offset
  (_fun _fz_pixmap-pointer -> _offset))
(define-pdf-lib mupdf_extract_bytes_from_pixmap
  (_fun _fz_pixmap-pointer -> _bytes))

(define make-context
  ((allocator mupdf_drop_context) (compose1 mupdf_register_type_handlers mupdf_new_context)))
(define (open-document ctx file)
  (((allocator (lambda (doc) (mupdf_drop_document ctx doc)))
    (lambda (f) (mupdf_open_document ctx f)))
   file))
(define (document-count-pages ctx doc)
  (mupdf_count_pages ctx doc))
(define (make-matrix zm1 zm2 rt)
  (mupdf_new_matrix zm1 zm2 rt))
(define (extract-pixmap ctx doc pgn mtx)
  (((allocator (lambda (pix) (mupdf_drop_pixmap ctx pix)))
    (lambda (pn) (mupdf_new_rgb_pixmap_from_page_number ctx doc pn mtx)))
   pgn))
(define (pixmap-has-alpha? pix)
  (mupdf_pixmap_alphap pix))
(define (pixmap-offsets pix)
  (let ((ofs (mupdf_pixmap_offset pix)))
    (values
     (offset-x ofs)
     (offset-y ofs))))
(define pixmap->bytes
  ((allocator free) mupdf_extract_bytes_from_pixmap))
