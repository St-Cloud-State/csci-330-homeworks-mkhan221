(defun inefficient-insert (x sorted-list)
  "Inefficiently inserts element X into the correct position in SORTED-LIST."
  (if (null sorted-list)
      (list x)  ;; Base case: insert in empty list
      (if (<= x (car sorted-list))
          (cons x sorted-list)  ;; Insert at the front if smaller
          (if (null (cdr sorted-list))
              (append sorted-list (list x)) ;; Inefficiently appending to end
              (cons (car sorted-list) (inefficient-insert x (cdr sorted-list)))))))

(defun inefficient-insertion-sort (unsorted sorted)
  "Inefficiently sorts a list using Insertion Sort."
  (if (null unsorted)
      sorted
      (inefficient-insertion-sort (cdr unsorted) (inefficient-insert (car unsorted) sorted))))

(defun inefficient-insertion-sort-wrapper (lst)
  "Wrapper function to start inefficient insertion sort with an empty sorted list."
  (inefficient-insertion-sort lst nil))

;; Example usage
(format t "Original list: ~a~%" '(10 7 8 9 1 5))
(format t "Sorted list: ~a~%" (inefficient-insertion-sort-wrapper '(10 7 8 9 1 5)))
