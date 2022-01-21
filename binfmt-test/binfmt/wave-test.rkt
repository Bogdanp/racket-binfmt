#lang racket/base

(require binfmt/runtime
         rackunit
         "common.rkt"
         "wave.b")

;; https://freesound.org/people/-zin-/sounds/32158/
(define r
  (call-with-input-file "wave.wav"
    (parameterize ([current-endianness 'little])
      wave)))
(define fmt (ref 'fmt_1 r))
(define data (ref 'data_1 r))

(check-equal? (ref 'num-channels_1 fmt) 2)
(check-equal? (ref 'sample-rate_1 fmt) 44100)
(check-equal? (ref 'bits-per-sample_1 fmt) 16)
(check-equal? (ref 'chunksize_1 data) 907200)
