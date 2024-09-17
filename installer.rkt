#lang racket/base
(require racket/system)
(provide installer)

(define (installer _ root)
  (let ((ret (system* "cc" "-lm" "-lmupdf" "-shared" "-fPIC" "-o"
                      (build-path root "libpdf.so")
                      (build-path root "pdf.c"))))
    (if ret (void) (raise-user-error "Fail to build the shared object."))))
