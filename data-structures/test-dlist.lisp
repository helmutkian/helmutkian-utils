
(define-test test-dlist-insert-front
  (assert-equal
   '(0)
   (dlist-to-list (dlist-insert-front 0 (make-dlist))))
  (assert-equal 
   '(3 2 1 0)
   (let ((the-dlist (make-dlist))) 
     (dotimes (i 4)
       (dlist-insert-front i the-dlist))
     (dlist-to-list the-dlist))))

(define-test test-dlist-insert-back
  (assert-equal
   '(0)
   (dlist-to-list (dlist-insert-back 0 (make-dlist))))
  (assert-equal 
   '(0 1 2 3)
   (let ((the-dlist (make-dlist)))
     (dotimes (i 4)
       (dlist-insert-back i the-dlist))
     (dlist-to-list the-dlist))))

(define-test test-dlist-insert-before
  (assert-equal
   '(0)
   (dlist-to-list (dlist-insert-before 0 (random 100) (make-dlist))))
  (assert-equal
   '(0 1)
   (dlist-to-list (dlist-insert-before 
		   0 ; new element
		   0 ; index
		   (dlist-insert-front 1 (make-dlist)))))
  (assert-equal
   '(0 1 3 2)
   (let ((the-dlist (make-dlist)))
     (dotimes (i 3)
       (dlist-insert-back i the-dlist))
     (dlist-insert-before 3 2 the-dlist)
     (dlist-to-list the-dlist))))

(define-test test-dlist-insert-after
  (assert-equal 
   '(0)
   (dlist-to-list (dlist-insert-after 0 (random 100) (make-dlist))))
  (assert-equal
   '(0 1)
   (dlist-to-list (dlist-insert-after 
		   1 ; new element
		   0 ; index
		   (dlist-insert-front 0 (make-dlist)))))
  (assert-equal
   '(0 1 3 2)
   (let ((the-dlist (make-dlist)))
     (dotimes (i 3)
       (dlist-insert-back i the-dlist))
     (dlist-insert-after 3 1 the-dlist)
     (dlist-to-list the-dlist))))

(define-test test-list-to-dlist
  (assert-equal
   '(a b c d)
   (dlist-to-list (list-to-dlist '(a b c d)))))

(define-test test-dlist-remove-if    
  (assert-equal
   '(1 3)
   (dlist-to-list (dlist-remove-if #'evenp 
				   (list-to-dlist '(1 2 3 4)))))
  (assert-equal
   '(2 4)
   (dlist-to-list (dlist-remove-if #'oddp
				   (list-to-dlist '(1 2 3 4)))))
  (assert-equal
   '(1 1 1)
   (dlist-to-list (dlist-remove-if #'zerop
				   (list-to-dlist '(1 1 1 0))))))
