#lang racket

; working through http://docs.racket-lang.org/more/index.html

(require xml net/url)

(define (go)
  'yep-it-works)

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread
      (lambda ()
        (handle in out)
        (close-input-port in)
        (close-output-port out))))
  ; watcher thread enforces 10 sec timeout
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    ; Match the first line to get the request
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ; discard rest of the header up to blank line
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; dispatch/routing
    (let ([xexpr (dispatch (list-ref req 1))])
      ; send reply
      (display "HTTP/1.1 200 OK\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))
