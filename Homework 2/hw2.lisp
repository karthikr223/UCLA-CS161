;BFS returns a list of terminal nodes in the order they would be visited by a left to right breadth first search.
;Arguments: TREE is a tree with arbitrary branching factor.
;Solution: If first TREE is an atom, it is at the current least depth, and is appended on to the front of the list, running BFS on the rest.
;If first TREE is a list, it is appended to the back of the TREE and BFS run again on the tree.
(defun BFS (TREE)
    (cond ((null TREE) nil)
        ((atom (first TREE)) (append (list (first TREE))(BFS (rest TREE))))
        (t (BFS (append (rest TREE) (first TREE))))))

;DFS returns a list of terminal nodes in the order they would be visited by a right to left depth first search.
;Arguments: TREE is a tree with arbitrary branching factor.
;Solution: If TREE is an atom, the search has reached the maximum depth of a branch of a tree and returns the node upwards recursively.
;If not, recursively call DFS on rest of tree and first of tree and append them in that order.        
(defun DFS (TREE)
    (cond ((null TREE) nil)
        ((atom TREE) (list TREE))
        (t (append (DFS(rest TREE))(DFS(first TREE))))))


;DFID2 is a helper function for DFID. It returns a list of terminal nodes visited by a left to right limited depth first search until the given depth limit.
;Arguments: TREE is a tree with arbitrary branching factor and depth is the depth limit for the search.
;Solution: Base cases: If depth reaches 0, we have hit the depth limit and we return upwards. If tree is null or an atom, we are at the max depth and return upwards.
;If not, call DFID2 recursively, subtracting 1 from depth each time we go down a level, appending from left to right order (first then rest).
(defun DFID2 (tree depth)
    (cond ((< depth 0) nil)
        ((null TREE) nil)
        ((atom TREE) (list TREE))
        (t (append (DFID2 (first TREE) (- depth 1))(DFID2 (rest TREE) depth)))))

;DFID is the top level function that implements iterative deepening, returning a list of terminal nodes in the order
;they would be visited by a left to right depth first iterative deepening search.
;Arguments: TREE is a tree with arbitrary branching factor and depth is the maximum depth of the iterative deepening search.
;Solution: Recursively call DFID subtracting 1 from depth from each time, calling helper function DFID2 at each level to return the nodes found by DFS at that depth. 
(defun DFID (tree depth)
    (cond ((< depth 0) nil)
        ((null TREE) nil)
        (t (append (DFID tree (- depth 1)) (DFID2 tree depth)))))

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
(equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
    (cond ((or (> m (first s))(> c (second s))) NIL) ;check if moving more cannibals or missionaries than there are
        ((or (> (+ m c) 2)(< (+ m c) 1)) NIL) ;check there are either 1 or 2 people in the boat
        ((and (> (+ (- 3 (second s)) c) (+ (- 3 (first s)) m))(not (= (+ (- 3 (first s)) m) 0))) NIL) ;check if cannibals outnumber missionaries on boat side
        ((and (> (- (second s) c)(- (first s) m))(not (= (- (first s) m) 0))) NIL) ;check if cannibals outnumber missionaries on opposite side
        (t (list (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) (not (third s))))))) ;return next state

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
    (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
    (cond ((null states) nil)
        ((equal s (first states)) t)
        (t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
    (cond ((mc-dfs (first states) path) (mc-dfs (first states) path)) ;if a path exists to the final state from the first state in the list, return it
          ((not (null (rest states))) (mult-dfs (rest states) path)) ;else call mult dfs on rest of states
          (t nil))) ;if states is empty return nil

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
    (cond ((final-state s) (append (list s) path)) ;if final state reached, return path
        ((on-path s path) nil) ;if s is already on the path, return nil to avoid infinite looping
        ((null (succ-fn s)) nil) ;if no successor states, return nil
        (t (mult-dfs (succ-fn s) (append (list s) path))))) ;else call mult-dfs on list of successor states