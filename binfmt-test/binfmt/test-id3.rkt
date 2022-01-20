#lang racket/base

(require binfmt/runtime
         rackunit
         "common.rkt"
         "id3v1.b")

(define (ref-bytes id lst)
  (apply bytes (ref id lst)))

(define t
  (call-with-input-file "creativecommonssong.mp3"
    (lambda (in)
      (file-position in eof)
      (file-position in (- (file-position in) 128))
      (parameterize ([current-endianness 'big])
        (id3 in)))))

(check-equal? (ref-bytes 'title_1 t)   #"Creative Commons Song         ")
(check-equal? (ref-bytes 'artist_1 t)  #"Improbulus                    ")
(check-equal? (ref-bytes 'album_1 t)   #"N/A                           ")
(check-equal? (ref-bytes 'year_1 t)    #"2005")
(check-equal? (ref-bytes 'comment_1 t) #"Take on O Mio Babbino Caro!   ")
(check-equal? (ref       'genre_1 t)   103)
