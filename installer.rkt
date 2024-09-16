#lang racket/base
(require racket/system)
(provide installer)

(define (installer _ root)
  (parameterize ((current-directory root))
    (let ((ret (system* "cc" "-lmupdf" "-shared" "-fPIC" "-o" "libpdf.so" "pdf.c" #:set-pwd? #t)))
      (if ret (void) (raise-user-error "Fail to build the shared object.")))))
