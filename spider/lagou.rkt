#lang racket
(require net/url)
(require db)
(require (planet neil/html-parsing:3:0))
(require (planet clements/sxml2:1))

(define base-url "http://www.lagou.com/gongsi/interviewExperiences.html?companyId=")
(define range-start 1501)
(define range-end 2000)
(define name-partern "//a/@title/text()") ;name 名称
(define count-partern "//*[@id='interview_container']/div[1]/span/text()") ;count 评论人数
(define score-partern "//*[@id='interview_container']/div[2]/div[1]/div/span[2]/text()");score 综合得分
(define corrating-partern "//*[@id='interview_container']/div[2]/div[1]/ul/li[1]/span[2]/text()");corrating 相符
(define interviewer-partern "//*[@id='interview_container']/div[2]/div[1]/ul/li[2]/span[2]/text()");interviewer 面试官
(define env-partern "//*[@id='interview_container']/div[2]/div[1]/ul/li[3]/span[2]/text()");env 环境
(define dis-partern "//*[@id='container_right']/div[1]/p/text()");dis 描述

(define create-table-text "create table company_score(
                          id integer,
                          name varchar(20),
                          count varchar(10),
                          score varchar(10),
                          corrating varchar(10),
                          interviewer varchar(10),
                          env varchar(10),
                          dis text(1000))")

(define sqlite-conn
  (virtual-connection
   (connection-pool
    (λ() (sqlite3-connect  #:database "db/lagou.db" )))))

(define (create-table)
  (query-exec sqlite-conn create-table-text))


(define (urlopen url)
  (call/input-url (string->url url)
   (curry get-pure-port #:redirections 5)
   port->string))

(define (html-data url)
  (html->xexp (urlopen url)))

(define (extract-data html partern)
  ((sxpath partern) html))

(define (main start end)
   (for ([num (in-list (range start end))])
    (define html-rsp (html-data (string-append base-url (format "~a" num))))
     (define raw-name (extract-data  html-rsp name-partern))
     (define raw-dis (extract-data  html-rsp dis-partern))
      (define rv
        (list
          num
          (list-ref raw-name 1)                     
          (list-ref (if (null?  (extract-data html-rsp count-partern))
                    '("(" "0" ")")
                    (extract-data html-rsp count-partern)) 1)
          (list-ref
           (if (null?
                (extract-data html-rsp score-partern)) '("0.0")
                    (extract-data html-rsp score-partern)) 0)
          (list-ref
           (if (null?
                (extract-data html-rsp corrating-partern)) '("0.0")
                    (extract-data html-rsp corrating-partern)) 0)
          (list-ref
           (if (null? (extract-data html-rsp interviewer-partern))
                                 '("0.0")
                                 (extract-data html-rsp interviewer-partern)) 0)
          (list-ref
           (if (null? (extract-data html-rsp env-partern))
                                 '("0.0") (extract-data html-rsp env-partern)) 0)
          (list-ref
           (if (null? raw-dis) '("") raw-dis) 0)))

     (cond [(not (equal? (list-ref rv 1) "使用腾讯QQ帐号登录"))
            (query-exec sqlite-conn "insert into company_score values ($1, $2, $3, $4, $5, $6, $7, $8)"
             (list-ref rv 0)
              (list-ref rv 1)
               (list-ref rv 2)
                (list-ref rv 3)
                 (list-ref rv 4)
                  (list-ref rv 5)
                   (list-ref rv 6)
                    (list-ref rv 7))
                     (displayln rv)]
           [else "not match"])
     ))


;(create-table)
 
(main range-start range-end)