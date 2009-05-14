;; -*- mode: lisp; syntax: common-lisp; -*-

(in-package "COMMON-LISP-USER")

(defclass reservoir ()
  ((samples :type (array t))
   (total-count :type fixnum :initform 0)))

(defmethod initialize-instance :after ((new-instance reservoir) &key n &allow-other-keys)
  (with-slots (samples) new-instance
    (setf samples (make-array n :element-type t))))

(defgeneric observe-stream-elment (reservoir stream-element)
  (:documentation 
   "Observe-stream-element gives the reservoir an oportunity to add
    the element to it's sample.  A single integer N is returned.  The
    caller should not invoke the observe-stream-element again until
    it comes to the Nth next element."))

(defmethod observe-stream-elment ((r reservoir) stream-element)
  (with-slots (samples total-count) r
    (let ((len (length samples)))
    (cond
      ((< total-count len)
       (setf (svref samples total-count) stream-element)
       (incf total-count)
       1)
      (t
       (incf total-count)
       (when (< (random total-count) len)
         (setf (svref samples (random len)) stream-element))
       1)))))

;; note we can do better, see: http://www.cs.umd.edu/~samir/498/vitter.pdf


#|

;; test

(defvar *test-url* "http://www.gutenberg.org/dirs/etext91/roget15a.txt")

(defun test-me ()
  (let ((reservoir (make-instance 'reservoir :n 60))
        (stream (drakma:http-request *test-url* :want-stream t)))
    (loop for line? = (read-line stream nil nil)
          while line?
          do
       (flet ((punks (c)
                (position c ".,;:!?()[]-&"))
              ((feature (word)
                        (length word)))
         (let* ((line (subseq line? 0 (1- (length line?))))
                (line (string-downcase line))
                (line (remove-if #'punks line))
                (words (split-sequence:split-sequence 
                        #\space line :remove-empty-subseqs t)))
           (loop for word in words
                 do (observe-stream-elment reservoir (feature word))))))
    (with-slots (samples total-count) reservoir
        (values
          (if (< (length samples) total-count)
              samples
              (subseq samples 0 (1- total-count)))
          total-count))))

|#