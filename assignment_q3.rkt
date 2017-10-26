#lang racket
;Question 3
;Part A
(provide traverse)
(provide traverse_left)
(provide traverse_right)


(define (traverse tree)
  (if (null? tree)
      (printf "Can't traverse empty tree.")
      (begin
       (traverse_left (car tree))
       (write (cadr tree))
       (printf " ")
       (traverse_right (caddr tree))
      )
  )
)


(define (traverse_left subtree)
  ;(printf " left ")
  (if (null? (cdr subtree)) ;if leaf node
      (begin
        (write (car subtree)) ; visit self
        (printf " "))
      (begin
       (traverse_left (car subtree)) ; else traverse left
       (write (cadr subtree)) ;then visit root of current sub tree
       (printf " ")
       (traverse_right (caddr subtree)) ;and then traverse right
      ); end else
      );end if  
)

(define (traverse_right subtree)
  ;(printf " right ")
  (if (null? (cdr subtree))
      (begin(write (car subtree))(printf " "))
      (begin
        (if (null? (cdddr subtree))
             (begin
               (write (caar subtree))
               (printf " ")
               (write (cadr subtree))
               (printf " ")
               (write (caaddr subtree))
               (printf " "))
        (begin
          (traverse_left(car (list subtree))) ; else traverse left child
          (write (cadr subtree))
          (printf " ")
          (traverse_right (caddr subtree))) ; then traverse right child
       ) ; end else
      );end if
      )
  );end function


;Part B:
;not finished
(provide present)
(define (present tree item)
  (if (null? tree)
      #f
      (if (equal? (car tree) item)
           #t
           (begin
             (cond
             [(list? (car tree))(present (car tree) item)]
             [(equal? (car tree) item) #t]
             [else (present (cdr tree) item)]
             )
             (cond
               [(null? (cdr tree)) #f]
               [else (cond
                       [(equal? (cadr tree) item) #t]
                       [(null? (cddr tree)) #f]
                       [else (present (caddr tree) item)]
                       )]
               
               )
             )
       )
     ) 
  )