;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


;EXERCISE: Modify this function to decide satisifiability of delta.

;sat? is a function that accepts two parameters, delta, a CNF represented as a list of lists, and n, the number of variables in delta.
;It calculates the satisfiability of delta, returning a list of n integers that satisfies the clauses if satisifiable, and NIL otherwise. 
;It calls backtrackSearch, a helper function that carries out backtrack search to solve the SAT problem.
(defun sat? (n delta)
	(backtracksearch n delta '()))

;backtrackSearch carries out backtrack search to solve the satisifiablility problem.
;Arguments: delta is a CNF represented as a list of lists, n is the number of variables in delta, and assignmentlist is the list of variables that have been assigned so far.
;Solution: If the current set of assignments does not satisfy delta, return nil. If the set of assignments is length n (all variables have been assigned) and it satisfies delta, this is a solution, return it.
;Otherwise add the next unassigned variable, and try backtrack search on both true and false possibilities for that variable.
(defun backtrackSearch (n delta assignmentlist)
    (cond ((not (evaluateCNF delta assignmentlist)) nil)
        ((= (length assignmentlist) n) assignmentlist)
        (t (or (backtrackSearch n delta (appendAssignment 0 assignmentlist)) (backtrackSearch n delta (appendAssignment  1 assignmentlist))))))

;Helper function for backtrackSearch
;Arguments: bool is whether the next variable to be assigned should be assigned true or false, and assignmentlist is the list of variables that have been assigned so far.
;Assignments are appended in increasing order from 1 to n, so just append the next consecutive value to assignmentlist.
(defun appendAssignment (bool assignmentlist)
    (cond ((= bool 0) (append assignmentlist (list (- 0 (+ (length assignmentlist) 1)))))
        (t (append assignmentlist (list (+ (length assignmentlist) 1))))))

;evaluateCNF checks if the current assignments satisfy the CNF.
;Arguments: cnf is a CNF represented as a list of lists, and assignmentlist is the list of variables that have been assigned so far.
;Call evaluateClause on each element of the cnf recursively, returning nil up the chain if any clause is not satisfied with the current set of assignments, and returning true if all clauses are satisfied.
(defun evaluateCNF (cnf assignmentlist)
    (cond ((null cnf) t)
        ((not (evaluateClause (car cnf) assignmentlist)) nil)
        (t (evaluateCNF (cdr cnf) assignmentlist))))

;evaluateClause checks if the current assignments satisfy a clause.
;Arguments: clause is a clause represented as a list of integers, and assignmentlist is the list of variables assigned so far.
;Call evaluateLiteral on each element of the clause recursively, returning t up the chain if any of the literals is satisfied with the current set of assignments, and returning nil if all literals are not satisfied.
(defun evaluateClause (clause assignmentlist)
    (cond ((null clause) nil)
        ((evaluateLiteral (car clause) assignmentlist) t)
        (t (evaluateClause (cdr clause) assignmentlist))))

;evaluateLiteral checks if the current assignments satisfy a single literal.
;Arguments: literal is an integer representing a literal of the CNF, and assignmentlist is the list of variables assigned so far.
;Call evaluateLiteral on each element of assignmentlist recursively.
;If there is an assignment that is equal to the literal, return true. If there is an assignment that is the negation of literal, return nil. If neither (the variable hasnt been assigned yet), return t.
(defun evaluateLiteral (literal assignmentlist)
    (cond ((null assignmentlist) t)
        ((= literal (car assignmentlist)) t)
        ((and (not (= literal (car assignmentlist))) (= (abs literal) (abs (car assignmentlist)))) nil)
        (t (evaluateLiteral literal (cdr assignmentList)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

