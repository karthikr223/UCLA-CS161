;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) t)
  		((atom s) (not (or (isKeeper s) (isBox s))))
		(t (and (goal-test (car s)) (goal-test (cdr s))))
));end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
;next-states returns a list of valid next states, trying to move the keeper up, down, left and right.
;Arguments: s is the current state.
;Returns result, a list formed by calling the function try-move in all four directions.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s x y 0) (try-move s x y 1) (try-move s x y 2) (try-move s x y 3)))
	 )
    (cleanUpList result);end
   );end let
  );

;try-move returns the next state formed by moving the keeper in a particular direction.
;Arguments: s is the current state, x and y are the coordinates of the keeper, and direction can be 0, 1, 2 or 3 corresponding to up, down, left and right.
;Checks if the keeper can move in that direction. If there is a wall in the way or a box that can not move, return nil, otherwise update the resulting state from that move and return it.
(defun try-move (s x y direction)
	(let* ((prevpos (cond ((isKeeper (getElementAtPosition s x y)) 0)
						(t 4))))
		(cond ((= direction 0) (cond ((isWall (getElementAtPosition s x (- y 1))) nil);up
									((isBlank (getElementAtPosition s x (- y 1))) (setElementAtPosition (setElementAtPosition s 3 x (- y 1)) prevpos x y ))
									((isStar (getElementAtPosition s x (- y 1))) (setElementAtPosition (setElementAtPosition s 6 x (- y 1)) prevpos x y ))
									((isBox (getElementAtPosition s x (- y 1))) (cond ((isWall (getElementAtPosition s x (- y 2))) nil)
																					((isBlank (getElementAtPosition s x (- y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 x (- y 2)) 3 x (- y 1)) prevpos x y))
																					((isStar (getElementAtPosition s x (- y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 x (- y 2)) 3 x (- y 1)) prevpos x y))
																					(t nil)))
									((isBoxStar (getElementAtPosition s x (- y 1))) (cond ((isWall (getElementAtPosition s x (- y 2))) nil)
																						((isBlank (getElementAtPosition s x (- y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 x (- y 2)) 6 x (- y 1)) prevpos x y))
																						((isStar (getElementAtPosition s x (- y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 x (- y 2)) 6 x (- y 1)) prevpos x y))
																						(t nil)))))
			((= direction 1) (cond ((isWall (getElementAtPosition s x (+ y 1))) nil);down
									((isBlank (getElementAtPosition s x (+ y 1))) (setElementAtPosition (setElementAtPosition s 3 x (+ y 1)) prevpos x y ))
									((isStar (getElementAtPosition s x (+ y 1))) (setElementAtPosition (setElementAtPosition s 6 x (+ y 1)) prevpos x y ))
									((isBox (getElementAtPosition s x (+ y 1))) (cond ((isWall (getElementAtPosition s x (+ y 2))) nil)
																					((isBlank (getElementAtPosition s x (+ y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 x (+ y 2)) 3 x (+ y 1)) prevpos x y))
																					((isStar (getElementAtPosition s x (+ y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 x (+ y 2)) 3 x (+ y 1)) prevpos x y))
																					(t nil)))
									((isBoxStar (getElementAtPosition s x (+ y 1))) (cond ((isWall (getElementAtPosition s x (+ y 2))) nil)
																						((isBlank (getElementAtPosition s x (+ y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 x (+ y 2)) 6 x (+ y 1)) prevpos x y))
																						((isStar (getElementAtPosition s x (+ y 2))) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 x (+ y 2)) 6 x (+ y 1)) prevpos x y))
																						(t nil)))))
			((= direction 2) (cond ((isWall (getElementAtPosition s (- x 1) y)) nil);left
									((isBlank (getElementAtPosition s (- x 1) y)) (setElementAtPosition (setElementAtPosition s 3 (- x 1) y) prevpos x y ))
									((isStar (getElementAtPosition s (- x 1) y)) (setElementAtPosition (setElementAtPosition s 6 (- x 1) y) prevpos x y ))
									((isBox (getElementAtPosition s (- x 1) y)) (cond ((isWall (getElementAtPosition s (- x 2) y)) nil)
																					((isBlank (getElementAtPosition s (- x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 (- x 2) y) 3 (- x 1) y) prevpos x y))
																					((isStar (getElementAtPosition s (- x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 (- x 2) y) 3 (- x 1) y) prevpos x y))
																					(t nil)))
									((isBoxStar (getElementAtPosition s (- x 1) y)) (cond ((isWall (getElementAtPosition s (- x 2) y)) nil)
																						((isBlank (getElementAtPosition s (- x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 (- x 2) y) 6 (- x 1) y) prevpos x y))
																						((isStar (getElementAtPosition s (- x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 (- x 2) y) 6 (- x 1) y) prevpos x y))
																						(t nil)))))
			((= direction 3) (cond ((isWall (getElementAtPosition s (+ x 1) y)) nil);right
									((isBlank (getElementAtPosition s (+ x 1) y)) (setElementAtPosition (setElementAtPosition s 3 (+ x 1) y) prevpos x y ))
									((isStar (getElementAtPosition s (+ x 1) y)) (setElementAtPosition (setElementAtPosition s 6 (+ x 1) y) prevpos x y ))
									((isBox (getElementAtPosition s (+ x 1) y)) (cond ((isWall (getElementAtPosition s (+ x 2) y)) nil)
																					((isBlank (getElementAtPosition s (+ x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 (+ x 2) y) 3 (+ x 1) y) prevpos x y))
																					((isStar (getElementAtPosition s (+ x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 (+ x 2) y) 3 (+ x 1) y) prevpos x y))
																					(t nil)))
									((isBoxStar (getElementAtPosition s (+ x 1) y)) (cond ((isWall (getElementAtPosition s (+ x 2) y)) nil)
																						((isBlank (getElementAtPosition s (+ x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 2 (+ x 2) y) 6 (+ x 1) y) prevpos x y))
																						((isStar (getElementAtPosition s (+ x 2) y)) (setElementAtPosition (setElementAtPosition (setElementAtPosition s 5 (+ x 2) y) 6 (+ x 1) y) prevpos x y))
																						(t nil)))))

																						)))

;getElementAtPosition returns the element at coordinates (x,y) in state s.
;It iterates through each row, calling getElementAtCol to iterate horizontally through the correct row. If the coordinates are outside the range (0, state size), return 1 representing a wall.
(defun getElementAtPosition (s x y)
	(cond ((or (< y 0) (< x 0) (> y (- (getNumRows s) 1))(> x (- (getNumCols s) 1))) 1)
		((> y 0) (getElementAtPosition (cdr s) x (- y 1)))
		(t (getElementAtCol (car s) x))))

;Helper funcion for getElementAtPosition - iterates through the row, accepting s as the input row and x as the position in the row to get an element.
(defun getElementAtCol (s x)
	(cond ((> x 0) (getElementAtCol (cdr s) (- x 1)))
		(t (car s))))

;setElementAtPosition sets the element at coordinates (x,y) in state s to v. 
;It iterates through the rows until it reaches the correct one, calling setElementAtCol to iterate horizontally and update the state.
(defun setElementAtPosition (s v x y)
	(cond ((> y 0) (cons (car s) (setElementAtPosition (cdr s) v x (- y 1))))
		(t (cons (setElementAtCol (car s) v x) (cdr s)))))

;Helper function for setElementAtPosition - iterates through the row s till position x, calling butlast and nthcdr to retrieve the two parts of the list on either side of the replaced element, and appending them with element v in the middle.
(defun setElementAtCol (s v x)
	(append (butlast s (- (length s) x)) (list v) (nthcdr (+ x 1) s)))

;Returns the number of rows in state s.
(defun getNumRows (s)
	(cond ((null s) 0)
			(t (length s))))

;Returns the number of columns in state s.
(defun getNumCols (s)
	(cond ((null s) 0)
			(t (length (car s)))))


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; Recursively checks the entire state, adding 1 whenever isBox returns true.
(defun h1 (s)
	(cond ((null s) 0)
		((atom s) (cond ((isBox s) 1)
						(t 0)))
		(t (+ (h1 (car s)) (h1 (cdr s))))))



; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;h904918243 is an admissible heuristic for the Sokoban problem.
;For a state s, for each box present in it, it finds the Manhattan distance to the closest goal to it. These distances are then summed to give the heuristic.
;This heuristic is admissible because the boxes have to be moved at minimum to the closest goal to them, therefore it is impossible that an optimal solution would have a fewer number of moves.
(defun h904918243 (s)
	(sumMinimumDistances (getBoxPositions s 0) (getGoalPositions s 0)))

;Helper functions for h904918243

;getBoxPositions finds the coordinates of all the boxes in the state, and returns them as a list.
;Arguments: s is the current state, and row is the coordinate of the starting row to be checked by the function.
;Uses helper function getBoxColumns to scan each row for a box, and calls the getList function to form the list of positions.
(defun getBoxPositions (s row)
  (cond ((null s) nil)
	(t (let ((x (getBoxColumns (car s) 0)))
	     (cond ((not (null x)) (append (getList row x) (getBoxPositions (cdr s) (+ row 1))))
		 	(t (getBoxPositions (cdr s) (+ row 1))))))))

;Helper function for getBoxPositions - iterates through the row r from starting column position col, and returns all positions in the row where there is a box.
(defun getBoxColumns (r col)
  (cond ((null r) nil)
	(t (cond ((isBox (car r)) (cons col (getBoxColumns (cdr r) (+ col 1))))
			(t (getBoxColumns (cdr r) (+ col 1)))))))


;getGoalPositions finds the coordinates of all the goals in the state, and returns them as a list.
;Arguments: s is the current state, and row is the coordinate of the starting row to be checked by the function.
;Uses helper function getBoxColumns to scan each row for a goal, and calls the getList function to form the list of positions.
(defun getGoalPositions (s row)
  (cond ((null s) nil)
	(t (let ((x (getGoalColumns (car s) 0)))
	     (cond ((not (null x)) (append (getList row x) (getGoalPositions (cdr s) (+ row 1))))
		 	(t (getGoalPositions (cdr s) (+ row 1))))))))

;Helper function for getBoxPositions - iterates through the row r from starting column position col, and returns all positions in the row where there is a goal.
(defun getGoalColumns (r col)
  (cond ((null r) nil)
	(t (cond ((isStar (car r)) (cons col (getGoalColumns (cdr r) (+ col 1))))
			(t (getGoalColumns (cdr r) (+ col 1)))))))

;Helper function for getBoxPositions and getGoalPositions - accepts a row number and a list of associated column numbers (where there were boxes or goals) and returns it as a list of coordinates in the form (col, row).
(defun getList (r cols) 
	(cond ((null cols) nil)
		(t (cons (cons (car cols) (list r)) (getList r (cdr cols))))))

;getManhattanDistance finds the manhattan distance between two points a and b, which are lists of the form (x,y).
;It finds the absolute value of the difference in x coordinates and y coordinates, and adds these to obtain the manhattan distance.
(defun getManhattanDistance (a b)
	(cond ((or (null a) (null b)) 0)
		(t (+ (abs (- (first a) (first b))) (abs (- (second a) (second b)))))))

;distanceToClosestGoal has arguments box, the coordinates of a single box, and goals, the list of all goals in the state.
;It calculates the manhattan distance  by calling getManhattanDistance from the box to each goal, and returns the minimum of all of these distances.
(defun distanceToClosestGoal (box goals)
	(cond ((null goals) nil)
		(t (cond ((= (length goals) 1) (getManhattanDistance box (car goals)))
				(t (min (getManhattanDistance box (car goals)) (distanceToClosestGoal box (cdr goals))))))))

;sumMinimumDistances accepts a list of coordinates of all boxes and a list of coordinates of all goals in the state. 
;It finds the distance to closest goal by calling distanceToClosestGoal on each element of boxes, and returns the sum of these values.
(defun sumMinimumDistances (boxes goals)
	(cond ((null boxes) 0)
		((null goals) 0)
		((atom boxes) (distanceToClosestGoal boxes goals))
		(t (+ (distancetoClosestGoal (car boxes) goals) (sumMinimumDistances (cdr boxes) goals)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
