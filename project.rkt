#lang racket

(require csc151)
(require json)

(define split
  (let ([find-cutpoint (lambda (str-lst)
                         (let* ([delimiters (list #\space #\! #\? #\,)]
                                [indicies (map1 (section index-of <> str-lst) delimiters)]
                                [reduced (filter (lambda (x) (not (= x -1))) indicies)])
                           (if (null? reduced)
                               -1
                               (reduce min reduced))))]
        [word-clean (let* ([last-char-dot? (lambda (str-lst)
                                             (let ([len (length str-lst)])
                                               (and (not (= 0 len))
                                                    (equal? #\. (list-ref str-lst (sub1 len))))))]
                           [count-dots (lambda (str-lst)
                                         (let ([len (length str-lst)])
                                           (let kernel ([i 0]
                                                        [count 0])
                                             (cond [(<= len i) count]
                                                   [(equal? #\. (list-ref str-lst i))
                                                    (kernel (add1 i) (add1 count))]
                                                   [else
                                                    (kernel (add1 i) count)]))))]
                           [ellipsis? (let ([just-dots? (lambda (str-lst)
                                                          (= (count-dots str-lst) (length str-lst)))])
                                        (lambda (str-lst)
                                          (let ([len (length str-lst)])
                                            (and (< 1 (count-dots str-lst))
                                                 (just-dots? (drop str-lst (- len 2)))))))]
                           [remove-ellipsis (letrec ([ellipsis-index (lambda (str-lst)
                                                                       (let ([i (index-of #\. str-lst)])
                                                                         (if (equal? (list-ref str-lst i) (list-ref str-lst (add1 i)))
                                                                             i
                                                                             (+ 2 i (ellipsis-index (drop str-lst (add1 i)))))))])
                                              (lambda (str-lst)
                                                (if (ellipsis? str-lst)
                                                    (take str-lst (ellipsis-index str-lst))
                                                    str-lst)))])
                      (lambda (str-lst)
                        (list->string (cond [(and (= 1 (count-dots str-lst))
                                                  (last-char-dot? str-lst))
                                             (take str-lst (sub1 (length str-lst)))]
                                            [(ellipsis? str-lst)
                                             (remove-ellipsis str-lst)]
                                            [else str-lst]))))])
    (lambda (str)
      (let ([lst (string->list str)])
        (let kernel ([so-far null]
                     [rest lst])
          (let ([cutpoint (find-cutpoint rest)])
            (if (= -1 cutpoint)
                (reverse (let ([word (word-clean rest)])
                           (if (equal? "" word)
                               so-far
                               (cons word so-far))))
                (let ([word (word-clean (take rest cutpoint))])
                  (if (equal? "" word)
                      (kernel so-far (drop rest (add1 cutpoint)))
                      (kernel (cons word so-far)
                              (drop rest (add1 cutpoint))))))))))))
                    
; data from:
; https://github.com/bpb27/trump_tweet_data_archive

(define get-tweets
  (let ([extract-tweet
         (lambda (hash)
           (let ([txt (hash-ref hash 'full_text #f)]
                 [date (hash-ref hash 'created_at #f)])
             (if (and txt date)
                 (list txt date)
                 null)))])
    (lambda (path)
      (let* ([file-port (call-with-input-file path read-json)])
        (let kernel ([so-far null]
                     [lst file-port])
          (if (null? lst)
              so-far
              (let ([twt (extract-tweet (car lst))])
                (if (null? twt)
                    (kernel so-far (cdr lst))
                    (kernel (append so-far (list twt)) (cdr lst))))))))))

(define tweets (get-tweets "/users/chriswon/Downloads/master_2017.json"))
; (define tweets (get-tweets "/home/chris/Downloads/master_2017.json"))

(define tallies (sort (tally-all (reduce append (map1 (o split car) tweets)))
                      (lambda (x y) (>= (cadr x) (cadr y)))))