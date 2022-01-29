#lang racket/base

(require racket/port
         rackunit
         "kafka.b")

(define (kstring s)
  (define bs (string->bytes/utf-8 s))
  `((StringLength_1 . ,(bytes-length bs))
    (StringData_1 . ,(bytes->list bs))))

(define a-request-header
  `((APIKey_1 . 3)
    (APIVersion_1 . 1)
    (CorrelationID_1 . 0)
    (ClientID_1 . ,(kstring "example"))))

(define a-request-header-bs
  (call-with-output-bytes
   (lambda (out)
     (un-RequestHeader a-request-header out))))

(check-equal?
 (call-with-input-bytes a-request-header-bs RequestHeader)
 a-request-header)

(define a-metadata-request
  `((ArrayLen_1 . 3)
    (Topic_1 . (,(kstring "topic-1")
                ,(kstring "topic-2")
                ,(kstring "topic-3")))))

(define a-metadata-request-bs
  (call-with-output-bytes
   (lambda (out)
     (un-MetadataRequest a-metadata-request out))))

(check-equal?
 (call-with-input-bytes a-metadata-request-bs MetadataRequest)
 a-metadata-request)
