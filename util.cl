
(defun iota (size &optional (start 1))
  (nlet iota-iter ((c size)
		   (ret '()))
	(if (<= c 0)
	    ret
	  (iota-iter (- c 1) (push (+ c start -1) ret)))))


(defun aiota (size &optional (start 1))
  (make-array (list size) :initial-contents (iota size start) :fill-pointer size))

(defun aswap! (ar x1 x2)
  (let ((tmp (aref ar x2)))
    (setf (aref ar x2) (aref ar x1))
    (setf (aref ar x1) tmp)
    ar))

(defun ashuffle! (rank-one-array)
  (if (equal (array-rank rank-one-array) 1)
      (let ((len (array-dimension rank-one-array 0)))
	(loop repeat (* len 2)
	      do (let ((x1 (random len))
		       (x2 (random len)))
		   (aswap! rank-one-array x1 x2)))
	rank-one-array)))

