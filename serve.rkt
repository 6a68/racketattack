#lang racket

; working through http://docs.racket-lang.org/more/index.html

(require xml net/url)

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

; heh. this guide dates from 2003, when html was an xml variant.
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

; note, some properties of net/url used to route endpoints:
;   > (define u (string->url "http://localhost:8080/foo/bar?x=bye"))
;   > (url-path u)
;   '(#<path/param> #<path/param>)
;   > (map path/param-path (url-path u))
;   '("foo" "bar")
;   > (url-query u)
;   '((x . "bye")) 
(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; call handler
      (h (url-query url))
      ; no handler found? return this funny-shaped error page in lisp form.
      ; ugh, tragically, return a 200 with "error" in the title, sigh
      `(html (head (title "Error"))
            (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

; register one sample route
(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))
