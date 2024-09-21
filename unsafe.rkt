#lang racket/base
(require ffi/unsafe ffi/unsafe/define ffi/unsafe/alloc ffi/vector
         racket/runtime-path racket/draw racket/class
         (except-in racket/contract ->)
         (rename-in racket/contract (-> n:->))
         "exn.rkt")
(provide make-context
         (contract-out
          (make-matrix (n:-> flonum? flonum? flonum? any))
          (document-count-pages (n:-> mupdf-document? any))
          (open-document (n:-> mupdf-context? (n:-> path-string? any)))
          (extract-pixmap (->i ((doc mupdf-document?)
                                (ctx fz_matrix?))
                               ()
                               (_ (doc)
                                  (->i ((page-num exact-nonnegative-integer?))
                                       ()
                                       #:pre (page-num) (> (document-count-pages doc) page-num)
                                       any))))
          (pixmap->bitmap (->* (mupdf-pixmap?) (#:alpha? boolean?) any)))
         mupdf-context?
         mupdf-document?
         mupdf-pixmap?
         (rename-out (fz_matrix? mupdf-matrix?)))

(define-runtime-path libpdf "libpdf.so")

(define pdf-lib (ffi-lib libpdf))
(define-ffi-definer define-pdf-lib pdf-lib)

(define _fz_context-pointer (_cpointer/null 'fz_context))
(define _fz_document-pointer (_cpointer/null 'fz_document))
(define _fz_pixmap-pointer (_cpointer/null 'fz_pixmap))

(define-cstruct _fz_matrix ([a _float] [b _float] [c _float] [d _float] [e _float] [f _float]))

(struct mupdf-context (ctx))
(struct mupdf-document mupdf-context (doc))
(struct mupdf-pixmap mupdf-context (pix))

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
(define-pdf-lib mupdf_pixmap_alpha_p
  (_fun _fz_pixmap-pointer -> _bool))
(define-pdf-lib mupdf_pixmap_w
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_h
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_x
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_y
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_n
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_s
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_stride
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_xres
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_yres
  (_fun _fz_pixmap-pointer -> _int))
(define-pdf-lib mupdf_pixmap_samples
  ;; _bytes cannot obtain all bytes it contains
  (_fun _fz_pixmap-pointer -> _pointer))

(define make-context
  ((allocator (compose1 mupdf_drop_context mupdf-context-ctx))
   (compose1 mupdf-context mupdf_register_type_handlers mupdf_new_context)))
(define (open-document ctx)
  ((allocator (lambda (doc) (mupdf_drop_document (mupdf-context-ctx doc) (mupdf-document-doc doc))))
   (lambda (f)
     (define c (mupdf-context-ctx ctx))
     (mupdf-document c (mupdf_open_document c f)))))
(define (document-count-pages doc)
  (mupdf_count_pages (mupdf-context-ctx doc) (mupdf-document-doc doc)))
(define (make-matrix zm1 zm2 rt)
  (mupdf_new_matrix zm1 zm2 rt))
(define (extract-pixmap doc mtx)
  ((allocator (lambda (pix) (mupdf_drop_pixmap (mupdf-context-ctx pix) (mupdf-pixmap-pix pix))))
   (lambda (pn)
     (define c (mupdf-context-ctx doc))
     (define d (mupdf-document-doc doc))
     (mupdf-pixmap c (mupdf_new_rgb_pixmap_from_page_number c d pn mtx)))))
(define (pixmap->bitmap wp #:alpha? (a? #t))
  (let* ((pix (mupdf-pixmap-pix wp))

         (w (mupdf_pixmap_w pix))
         (h (mupdf_pixmap_h pix))
         (n (mupdf_pixmap_n pix))
         (x (mupdf_pixmap_x pix))
         (y (mupdf_pixmap_y pix))
         (stride (mupdf_pixmap_stride pix))
         (alpha? (and a? (mupdf_pixmap_alpha_p pix)))
         (samples (mupdf_pixmap_samples pix))

         (unit-len 4)
         (bts (make-bytes (* unit-len w h)))
         (bmp (make-object bitmap% (+ x w) (+ y h) #f alpha?)))
    (for ((i (in-range 0 h)))
      (define sample-base (* i stride))
      (define result-base (* i unit-len w))
      (for ((p (in-range 0 w)))
        (define sample-rgb-base (+ sample-base (* p n)))
        (define result-alpha-base (+ result-base (* p unit-len)))
        (bytes-set! bts result-alpha-base
                    (if alpha? (ptr-ref samples _byte (+ sample-rgb-base n -1)) 255))
        (memcpy (u8vector->cpointer bts) (+ result-alpha-base 1)
                samples sample-rgb-base 3 _byte)))
    (send bmp set-argb-pixels x y w h bts)
    bmp))
