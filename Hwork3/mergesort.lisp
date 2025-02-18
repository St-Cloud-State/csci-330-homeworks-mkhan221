(defun partition (list)
  "Splits a list into two roughly equal halves."
  (cond
    ((null list) (list nil nil))
    ((null (cdr list)) (list list nil))
    (t (let ((mid (floor (length list) 2)))
          (list (subseq list 0 mid) (subseq list mid (length list)))))))

(defun merge-lists (list1 list2)
  "Merges two sorted lists into a single sorted list."
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((<= (car list1) (car list2))
     (cons (car list1) (merge-lists (cdr list1) list2)))
    (t (cons (car list2) (merge-lists list1 (cdr list2))))))

(defun mergesort (list)
  "Sorts a list using the merge sort algorithm."
  (cond
    ((null list) nil)
    ((null (cdr list)) list)
    (t (let ((halves (partition list)))
         (merge-lists (mergesort (car halves)) (mergesort (cadr halves)))))))
