#lang racket

;Part A:
(provide ins_beg)

(define (ins_beg el lst)
  (append (list el) lst)
  )


;Part B:
(provide ins_end)

(define (ins_end el lst)
  (append lst (list el))
 )


;Part C:
(provide count_top_level)
(define (count_top_level lst)
  (if (null? lst)
  0
  (+ 1 (count_top_level (cdr lst)))
  )
  )

;Part D: non tail recursive count instances
(provide count_instances)
(define (count_instances lst item)
  (if (null? lst)
      0
      (cond
        ((= (car lst) item) (+ 1 (count_instances (cdr lst) item)))
        (else (count_instances (cdr lst) item))
        )
  )
  )

;Part E: tail recursive count instances
(provide count_instances_tr)
(provide tail_count)

(define (count_instances_tr lst item)
  (tail_count  lst 0 item)
  )


(define (tail_count lst total item)
  (cond
    [(equal? '() lst) (write total)]
    [else
       (if (= (car lst) item)
         (tail_count (cdr lst) (+ 1 total) item)
         (tail_count (cdr lst) total item)
         ) ]
      )
  )

;Part F
(provide count_instances_deep)
(provide count_deep)
(provide count_sublist)

(define (count_instances_deep lst item)
  (count_deep lst item 0)
)

(define (count_deep lst item total)
  (cond
    [(equal? '() lst) (write total)]
    [(list? (car lst))(count_deep (cdr lst) (+ total (count_sublist (car lst) item)) item)]
    [else (if (= (car lst) item)
         (count_deep (cdr lst) (+ 1 total) item)
         (count_deep (cdr lst) total item)
         ) ]
      )
  )

(define (count_sublist lst item) ;Ok so this isnt 100% working but running out of time
  (cond
    [(equal? '() lst) 0]
    [(list? (car lst))(count_sublist (car lst) item)]
    [else (if (= (car lst) item)
         (+ 1 (count_deep (cdr lst) 0 item))
         (+ 0 (count_deep (cdr lst) 0 item))
         ) ]
      )
  )
