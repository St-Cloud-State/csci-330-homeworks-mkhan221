(defun inefficient-split-into-pairs (lst)
  "Splits a list into sorted pairs."
  (if (null lst)
      nil
      (if (null (cdr lst))
          (list (list (car lst))) ;; Handle single element at end
          (cons (list (min (car lst) (cadr lst)) (max (car lst) (cadr lst)))
                (inefficient-split-into-pairs (cddr lst))))))

(defun inefficient-merge (lst1 lst2)
  "Merges two sorted lists into one sorted list."
  (cond
    ((null lst1) lst2)
    ((null lst2) lst1)
    ((<= (car lst1) (car lst2))
     (cons (car lst1) (inefficient-merge (cdr lst1) lst2)))
    (t (cons (car lst2) (inefficient-merge lst1 (cdr lst2))))))

(defun inefficient-merge-all (lst)
  "Merges adjacent lists in each pass."
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((merged-lst '()))
        (loop for i from 0 to (- (length lst) 1) by 2 do
              (if (< i (- (length lst) 1))
                  (push (inefficient-merge (nth i lst) (nth (+ i 1) lst)) merged-lst)
                  (push (nth i lst) merged-lst)))
        (inefficient-merge-all (reverse merged-lst)))))

(defun bottom-up-mergesort (lst)
  "Sorts a list using the Bottom-Up Mergesort algorithm."
  (car (inefficient-merge-all (inefficient-split-into-pairs lst))))

;; Example Usage:
(format t "Original list: ~a~%" '(1 7 2 1 8 6 5 3 7 9 4))
(format t "Sorted list: ~a~%" (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))
