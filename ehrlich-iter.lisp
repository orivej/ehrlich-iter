(in-package #:iterate)

(defmacro swap (a b)
  `(psetf ,a ,b ,b ,a))

(defun vreverse (vec start end)
  (iter (for i from start to (floor (+ start end) 2))
        (for j from end downto (ceiling (+ start end) 2))
        (swap (aref vec i) (aref vec j)))
  vec)

(defun ehrlich-iter-init (count)
  (let* ((n count)
         (b (make-array count :element-type 'fixnum))
         (c (make-array (1+ count) :element-type 'fixnum :initial-element 0)))
    (iter (for i index-of-vector b)
          (setf (aref b i) i))
    (list* n b c)))

(defun ehrlich-iter (state &aux (k 1))
  "Generate a sequence of permutating swaps with the first element"
  (symbol-macrolet ((n (car state))
                    (b (cadr state))
                    (c (cddr state)))
    (iter (while (eql (aref c k) k))
          (setf (aref c k) 0)
          (incf k))
    (unless (eql n k)
      (incf (aref c k))
      (vreverse b 1 (1- k))
      (aref b k))))

(defmacro-driver (for var in-ehrlich-permutations v &optional destructive d)
  "All permutations of vector V, each in a single swap difference from the other

If destructive is set, values cannot be collected and must be
processed immediately"
  (let ((vec   (gensym (symbol-name '#:vec)))
        (state (gensym (symbol-name '#:state)))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,vec = ,(if (and d (not (constantp v))) v `(copy-seq ,v)))
       (with ,state)
       (,kwd ,var next (if ,state
                           (let ((next (ehrlich-iter ,state)))
                             (if next
                                 (progn
                                   (swap (aref ,vec 0) (aref ,vec next))
                                   ,(if d vec `(copy-seq ,vec)))
                                 (terminate)))
                           (progn
                             (setf ,state (ehrlich-iter-init (length ,vec)))
                             ,(if d vec `(copy-seq ,vec))))))))

(export 'in-ehrlich-permutations)
