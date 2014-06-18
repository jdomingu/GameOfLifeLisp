;;;;Conway's Game of Life in Lisp
;;;;
;;;;Sample patterns:
;;;;  Blinker: (game-start '((0 1) (1 1) (2 1)) 3 10)
;;;;  Beacon: (game-start '((1 1) (2 1) (1 2) (4 3) (3 4) (4 4)) 6 10)
;;;;  Glider: (game-start '((1 0) (2 1) (0 2) (1 2) (2 2)) 20 50)
;;;;  Combination: (game-start '((0 12) (1 12) (2 12) (1 6) (2 7) (0 8) (1 8) (2 8)) 20 100)

;;live-list is a list of coordinates for live cells. For example: '((0 1) (1 1) (2 1))
;;size-sq is the size of the square grid. For example, 3 to draw a 3x3 grid.
;;num-gen is the number of generations to simulate.
(defun game-start (live-list size-sq num-gen)
  (if (zerop num-gen)
    next-gen
    (progn
      (setf next-gen '())
      (setf grid (get-grid-values size-sq))
      (dolist (grid-cell grid next-gen)
        (let ((neighbor-list (get-neighbors (car grid-cell) (car (cdr grid-cell)))))
          (setf num-alive-neighbors (get-num-alive live-list neighbor-list))
          (setf currently-alive (member grid-cell live-list :test #'equal))
          (if (will-live num-alive-neighbors currently-alive )
            (setf next-gen (append next-gen (list grid-cell))))))
      (draw next-gen grid size-sq)
      (sleep 0.15)
      (game-start next-gen size-sq (- num-gen 1)))))

;;Create a two-dimensional list of coordinates based on the grid size.
(defun get-grid-values (size-sq)
  (setf cells '())
  (do ((i 0 (+ i 1)))
    ((= i size-sq) cells)
      (do ((j 0 (+ j 1)))
      ((= j size-sq) cells)
        (setf cells (append cells (list (list i j))))))
  cells)
 
;;Get the list of neighbors for a cell with coordinates (x,y). 
(defun get-neighbors (x y)
  (list 
    (list (- x 1) (- y 1)) (list (- x 1) y) (list (- x 1) (+ y 1))
    (list x (- y 1)) (list x (+ y 1))
    (list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))))

;;Get the number of neighboring cells that are alive.    
(defun get-num-alive (live-list neighbor-list)
  (setf num-alive 0)
  (dolist (neighbor neighbor-list num-alive)
      (if (member neighbor live-list :test #'equal)
        (progn 
          (setf num-alive (+ 1 num-alive))))))

;;Determine whether a given cell will be alive in the next generation.          
(defun will-live (num-alive currently-alive)
  (cond ((< num-alive 2) nil)
    ((and (= num-alive 2) currently-alive) t)
    ((= num-alive 3) t)
    ((> num-alive 3) nil)))

;;Print on the command line. Keep track of the x coordinate and enter new lines.
(defun draw (live-cells grid size-sq)
  (dolist (grid-cell grid)
    (if (member grid-cell live-cells :test #'equal)
      (format t "X")
      (format t "-"))
    (if (zerop (mod (+ 1 (car (cdr grid-cell))) size-sq))
      (format t "~%"))))
