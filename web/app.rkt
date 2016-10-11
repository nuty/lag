#lang racket

(require web-server/servlet
         db
         xml
         web-server/servlet-env
         web-server/templates)

(define select-sql-text "SELECT * FROM company_score ORDER BY id DESC LIMIT 50")

(define sqlite-conn
  (virtual-connection
   (connection-pool
    (λ() (sqlite3-connect  #:database "db/lagou.db" )))))

(define (render-template collections)
    (include-template "template/index.html"))


(define (response-to ret)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    empty
    (list (string->bytes/utf-8 ret))))


(define (query-all)
  (query-rows sqlite-conn select-sql-text))

(define (index req)
  (define all-data
    (map (lambda (row)
         (vector->list row))
       (query-all)))
  (response-to (render-template all-data)))


(serve/servlet index
  #:port 8000
  #:extra-files-paths (list (build-path "web/static"))
  #:servlet-path "/index"
  #:stateless? #t)
