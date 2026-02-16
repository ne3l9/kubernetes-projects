;;;; Conway's Game of Life
;;;; Clean version – SBCL / Alive compatible

(defpackage :life
  (:use :cl))

(in-package :life)

(defparameter *width* 40)
(defparameter *height* 20)

(defun make-grid ()
  (make-array (list *height* *width*)
              :initial-element 0))

(defun randomize-grid (grid)
  (dotimes (y *height*)
    (dotimes (x *width*)
      (setf (aref grid y x) (random 2))))
  grid)

(defun neighbors (grid y x)
  (loop for dy from -1 to 1
        sum (loop for dx from -1 to 1
                  unless (and (= dx 0) (= dy 0))
                  sum (let ((ny (+ y dy))
                            (nx (+ x dx)))
                        (if (and (>= ny 0) (< ny *height*)
                                 (>= nx 0) (< nx *width*))
                            (aref grid ny nx)
                            0)))))

(defun next-step (grid)
  (let ((new (make-grid)))
    (dotimes (y *height*)
      (dotimes (x *width*)
        (let ((n (neighbors grid y x))
              (c (aref grid y x)))
          (setf (aref new y x)
                (if (or (= n 3)
                        (and (= n 2) (= c 1)))
                    1
                    0)))))
    new))

(defun draw (grid)
  (fresh-line)
  (dotimes (y *height*)
    (dotimes (x *width*)
      (princ (if (= (aref grid y x) 1) "█" " ")))
    (terpri)))

(defun run ()
  (let ((g (randomize-grid (make-grid))))
    (loop
      (draw g)
      (setf g (next-step g))
      (sleep 0.1))))

(write-line "Code it up!")
