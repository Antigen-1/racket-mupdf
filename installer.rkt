#lang racket/base
(require racket/system)
(provide installer)

(define (installer _ root)
  (parameterize ((current-directory root))
    (let ((ret (system "cc -lm -lmupdf -fPIC -shared pdf.c -o libpdf.so" #:set-pwd? #t)))
      (if ret (void) (raise-user-error "Fail to build the shared object.")))))
