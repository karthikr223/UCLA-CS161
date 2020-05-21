;Question 1: 
;Arguments: N is a number and TREE is an ordered tree.
;The function TREE-CONTAINS returns t if element N is present in TREE, and NIL otherwise.
;Solution: This is done by recursively checking the left, middle and right tree (first, second and third) until reaching an atom, in which case true is returned if the atom is equal to N.
(defun TREE-CONTAINS (N TREE)
(cond ((null TREE) NIL)
    ((atom TREE) (cond ((= TREE N) t)(t NIL)))
    (t (or (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (second TREE)) (TREE-CONTAINS N (third TREE))))))

;Question 2:
;Arguments: TREE is an ordered tree.
;This function returns the minimum value in the tree.
;Solution: Since the minimum value is the left most value, the left subtree is recursively checked until reaching an atom, which is returned as the minimum value.
(defun TREE-MIN (TREE)
(cond ((null TREE) NIL)
    ((atom TREE) TREE)
    (t (TREE-MIN (first TREE)))))

;Question 3:
;Arguments: TREE is an ordered tree.
;This function returns a pre-ordered list of the numbers appearing in TREE.
;Solution: The function recursively orders the middle tree, left tree and right tree and then appends them in middle, left, right order.
(defun TREE-ORDER (TREE)
(cond ((null TREE) NIL)
    ((atom TREE) (list TREE))
    (t (append (TREE-ORDER (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))))

;Question 4:
;Arguments: L is a list, START is an integer representing the starting position of the sub-list, and LEN is an integer representing length of the sub-list
;This function returns a sub-list of list L, starting at position START and with length LEN.
;Solution: If start is greater than 0, the function is recursively called on (rest L) with START = START - 1. If START is 0, then (first L) is concatenated with SUB-LIST of (rest L), with START 0 and LEN = LEN - 1, recursively until LEN reaches 0.
(defun SUB-LIST (L START LEN)
(cond ((null L) NIL)
    ((= LEN 0) NIL)
    ((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))))
    (t (SUB-LIST (rest L) (- START 1) LEN))))

;Question 5:
;Arguments: L is a list.
;This function returns a list of two lists where list 1 is either 1 element larger or the same size as list 2, and the two lists appended give the original list.
;Solution: Check if list length is odd or even, then call sub list from the appropriate starting position and length, and return the list of both lists.
(defun SPLIT-LIST (L)
(let* ((list-length (length L)))
    (cond ((evenp list-length) (list (SUB-LIST L 0 (/ list-length 2)) (SUB-LIST L (/ list-length 2) (/ list-length 2))))
        (t (list (SUB-LIST L 0 (/ (+ list-length 1) 2)) (SUB-LIST L (/ (+ list-length 1) 2) (/ (- list-length 1) 2)))))))

;Question 6:
;Arguments: TREE is a binary tree.
;This function returns the height (or the length of the longest path) of the binary tree TREE.
;Solution: At each level of the tree, recursively call the function on the left child and right child of tree, adding 1 at each level, and finally returning the max of the left tree height and right tree height
(defun BTREE-HEIGHT (TREE)
(cond ((atom TREE) 0)
    (t (let* ((leftheight (+ (BTREE-HEIGHT (first TREE)) 1))(rightheight (+ (BTREE-HEIGHT(second TREE)) 1)))
        (cond ((> leftheight rightheight) leftheight)
            (t rightheight))))))

;Question 7:
;Arguments: LEAVES is a list of atoms.
;This function returns a binary tree with the elements of LEAVES, such that for any node the left child has 0 or 1 more leaves than the right child.
;Solution: Split LEAVES using SPLIT-LIST, and then recursively call LIST2BTREE on the two halves of the list, returning up the recursive cycle when the list is 1 or 2 elements long.
(defun LIST2BTREE (LEAVES)
(cond ((null LEAVES) NIL)
    ((= (length LEAVES) 1)(first LEAVES))
    ((= (length LEAVES) 2) LEAVES)
    (t (list (LIST2BTREE (first (SPLIT-LIST LEAVES)))(LIST2BTREE (second (SPLIT-LIST LEAVES)))))))
    
;Question 8:
;Arguments: TREE is a binary tree.
;This function returns a list of atoms consisting of the nodes of the binary tree, and is the inverse function of LIST2BTREE.
;Solution: The function recursively calls itself on the left and right child of the tree, appending the two together as one list at each level of the tree.
(defun BTREE2LIST (TREE)
(cond ((null TREE) NIL)
    ((atom TREE) (list TREE))
    (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))

;Question 9:
;Arguments: E1 and E2 are lisp expressions with atoms that are all numbers
;This function returns t if the two expressions are identical and NIL otherwise.
;Solution: If both are null, return true. If both are atoms, compare the atoms and return true if equal. If one is null and the other is not, return NIL.
;If they are lists, recursively call the function on the head of both lists as well as the tail of both lists, returning up the chain when the lists have been reduced to 0 or 1 element.
(defun IS-SAME (E1 E2)
(cond ((and (null E1)(null E2)) t)
    ((and (not (null E1))(null E2)) NIL)
    ((and (not (null E2))(null E1)) NIL)
    ((and(atom E1)(atom E2))(= E1 E2))
    ((and(listp E1)(listp E2))(and (IS-SAME (first E1)(first E2))(IS-SAME (rest E1)(rest E2))))
    (t NIL)))

