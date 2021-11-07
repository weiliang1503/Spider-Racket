#lang racket

(require net/url)
(require racket/gui)
(require json)
(require racket/date)
(require racket/draw)

(date-display-format 'iso-8601)

(define spider% (class object%
                  (field (running? #f)
                         (cards   '())
                         (pic-urls '())
                         (pic-names'()))
                  (init-field working-gauge)
                  (super-new)

                  (define (init-url uid offset)
                    (string->url (format "https://api.vc.bilibili.com/dynamic_svr/v1/dynamic_svr/space_history?host_uid=~A&need_top=1&offset_dynamic_id=~A" uid offset)))

                  (define/public (init-cards uid)
                    (define offset "")
                    (let loop ((p 1))

                      (define u (init-url uid offset))
                      (define-values (status headers in)
                        (http-sendrecv/url u))

                      (define page (hash-ref (string->jsexpr
                                              (port->string in))
                                             'data))

                      (set! offset (hash-ref page 'next_offset))

                      (cond [(= (hash-ref page 'has_more) 1)
                             (displayln (format "Solving Page ~A, Next Offset is ~A" p offset))
                             (set! cards (append cards (hash-ref page 'cards)))
                             (loop (+ p 1))]
                            [else (displayln "Done!")])))

                  (define/public (init-pic-urls)
                    (for ((card cards))

                      (define card-info (string->jsexpr (hash-ref card 'card)))

                      (define date-base (date->string
                                         (seconds->date
                                          (hash-ref (hash-ref card 'desc) 'timestamp)) #t))

                      (set! date-base (regexp-replace* ":" date-base "-"))

                      (cond [(hash-has-key? card-info 'item)

                             (define card-useful-info (hash-ref card-info 'item))
                             (cond [(hash-has-key? card-useful-info 'pictures)

                                    (define pic-infos (hash-ref card-useful-info 'pictures))
                                    (define n 0)

                                    (for ((pic-info pic-infos))

                                      (define current-pic-url (hash-ref pic-info 'img_src))
                                      (set! pic-urls (append pic-urls (list current-pic-url)))

                                      (define picture-type (car (regexp-match "\\.(jpg|png|gif)$" current-pic-url)))
                                      (define current-pic-name (string-append date-base "-" (number->string n) picture-type))

                                      (set! pic-names (append pic-names (list current-pic-name)))


                                      (set! n (+ n 1))
                                      )])]
                            [else #f])))

                  (define (download-pic pic-url pic-path pic-s)
                    (define pic-u (string->url pic-url))
                    (define pic-p (string->path pic-path))
                    (define pic-n (file-name-from-path (url->path pic-u)))

                    (define out-put-p (build-path pic-p pic-s))

                    (define-values (status headers in)
                      (http-sendrecv/url pic-u))
                    (let ((out (open-output-file out-put-p)))
                      (display (port->bytes in) out))
                    (displayln (format "~A Done!" pic-n)))

                  (define/public (download-urls path)
                    (send working-gauge set-range (length pic-urls))
                    (define r 0)
                    (map thread-wait (for/list ((pic-url pic-urls) (pic-save pic-names))
                                       (thread (lambda ()
                                                 (download-pic pic-url path pic-save)
                                                 (set! r (+ 1 r))
                                                 (send working-gauge set-value r)
                                                 ))))
                    (displayln "All Pictures Downloaded"))

                  ))

(define frame (new frame%
                   (label "Spider for Bilibili")
                   (width 500)))

(define bitmap (make-bitmap 450 206))

(send bitmap load-file "logo.png")

(define msg (new message%
                 (label bitmap)
                 (parent frame)))

(define vbox (new vertical-pane%
                  (parent frame)
                  (vert-margin 20)
                  (horiz-margin 20)
                  (spacing 30)))

(define uid-field (new text-field%
                       (label "UID")
                       (init-value "439916362")
                       (parent vbox)))

(define gauge (new gauge%
                   (parent vbox)
                   (label "DOWNLOADING ")
                   (range 100)))

(define spider (new spider% (working-gauge gauge)))

(define hbox (new horizontal-pane%
                  (parent vbox)
                  (spacing 10)))

(define path-field (new text-field%
                        (label "SAVE PATH")
                        (init-value "./")
                        (parent hbox)))

(define start-button (new button%
                          (label "START")
                          (parent hbox)
                          (callback (lambda (button event)
                                      (if (get-field running? spider)
                                          (displayln "Already Running")
                                          (thread (lambda ()
                                                    (set-field! running? spider #t)
                                                    (send spider init-cards (send uid-field get-value))
                                                    (send spider init-pic-urls)
                                                    (send spider download-urls (send path-field get-value))
                                                    (set-field! running? spider #f))))))
                                      ))

(send frame show #t)
;; (send sp init-cards 439916362)

;; (send sp init-pic-urls)
;; (for ((i (get-field pic-names sp)))
;;   (displayln i))

;; (send sp download-urls "./help")
