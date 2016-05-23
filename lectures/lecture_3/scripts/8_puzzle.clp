(defglobal ?*highest-priority* = 1000)
(defglobal ?*ask-to-halt* = FALSE)
(defglobal ?*print-board* = FALSE)

(deftemplate 8-puzzle
  (slot one
	(type INTEGER)
	(default ?NONE))
  (slot two
	(type INTEGER)
	(default ?NONE))
  (slot three
	(type INTEGER)
	(default ?NONE))
  (slot four
	(type INTEGER)
	(default ?NONE))
  (slot five
	(type INTEGER)
	(default ?NONE))
  (slot six
	(type INTEGER)
	(default ?NONE))
  (slot seven
	(type INTEGER)
	(default ?NONE))
  (slot eight
	(type INTEGER)
	(default ?NONE))
  (slot nine
	(type INTEGER)
	(default ?NONE)))

(deftemplate move
  (slot parent
	(type FACT-ADDRESS)
	(default ?NONE))
  (slot next
	(type FACT-ADDRESS)
	(default ?NONE)))

; (deffacts initial-board
;   (8-puzzle (one 1) (two -1) (three 3)
; 	    (four 4) (five 2) (six 6)
; 	    (seven 7) (eight 5) (nine 8)))
(deffacts initial-board
  (8-puzzle (one 1) (two 2) (three 3)
	    (four 4) (five -1) (six 6)
	    (seven 7) (eight 5) (nine 8)))

;; Routines for question-driven interaction
;; Modified from Riley's& Giarratano's
(deffunction ask-question (?question $?allowed-values)
  (printout t ?question)
  (bind ?answer (read))
  (if (lexemep ?answer) ;; TRUE is ?answer is a STRING or SYMBOL
      then (bind ?answer (lowcase ?answer)))
     (while (not (member ?answer ?allowed-values)) do
	    (printout t ?question)
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
		then (bind ?answer (lowcase ?answer))))
     ?answer)

(deffunction yes-or-no-halt ()
  (if (eq ?*ask-to-halt* TRUE)
      then (bind ?question "Continue with next move? (yes/y/no/n): ")
           (bind ?response (ask-question ?question yes no y n))
	   (if (or (eq ?response no) (eq ?response n))
	       then (halt))
      else FALSE))

(deffunction print-board-cell (?board ?cell-pos)
  (if (eq (fact-slot-value ?board ?cell-pos) -1)
      then "  X  "
      else (str-cat " [" (fact-slot-value ?board ?cell-pos) "] ")))

(deffunction print-board
  (?board)
  "Utility to print the puzzle board"
  (printout t "--------------------" crlf)
  (printout t "   " (print-board-cell ?board one))
  (printout t (print-board-cell ?board two))
  (printout t (print-board-cell ?board three))
  (printout t crlf)
  (printout t "   " (print-board-cell ?board four))
  (printout t (print-board-cell ?board five))
  (printout t (print-board-cell ?board six)) 
  (printout t crlf)
  (printout t "   " (print-board-cell ?board seven))
  (printout t (print-board-cell ?board eight))
  (printout t (print-board-cell ?board nine))
  (printout t crlf))

(deffunction print-board-moves
  (?moves)
  (progn$ (?m (fact-slot-value ?moves implied))
	  (print-board ?m)))

(deffunction print-boards
  (?board1 ?board2)
  "Utility to print the puzzle board transitions"
  (if (eq ?*print-board* TRUE)
      then  (printout t "-------------------------------------------" crlf)
            (printout t "   " (print-board-cell ?board1 one))
	    (printout t (print-board-cell ?board1 two))
	    (printout t (print-board-cell ?board1 three))
	    (printout t "       " (print-board-cell ?board2 one))
	    (printout t (print-board-cell ?board2 two))
	    (printout t (print-board-cell ?board2 three))
	    (printout t crlf)
	    (printout t "   " (print-board-cell ?board1 four))
	    (printout t (print-board-cell ?board1 five))
	    (printout t (print-board-cell ?board1 six))
	    (printout t "  ==>  " (print-board-cell ?board2 four))
	    (printout t (print-board-cell ?board2 five))
	    (printout t (print-board-cell ?board2 six))
	    (printout t crlf)
	    (printout t "   " (print-board-cell ?board1 seven))
	    (printout t (print-board-cell ?board1 eight))
	    (printout t (print-board-cell ?board1 nine))
	    (printout t "       " (print-board-cell ?board2 seven))
	    (printout t (print-board-cell ?board2 eight) )
	    (printout t (print-board-cell ?board2 nine))
	    (printout t crlf "-------------------------------------------" crlf)))

(defrule from-one-to-two
  ?s <-(8-puzzle
	(one -1) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	   (one ?T) (two -1) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?T) (two -1) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  ; (assert (search-history
  ; 	   (8-puzzle
  ; 	    (one ?T) (two -1) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  ; 	   ?s))
  (printout t "Moved from one to two" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-one-to-four
  ?s <-(8-puzzle
	(one -1) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?F) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?F) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from one to four" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-two-to-one
  ?s <-(8-puzzle
	(one ?O) (two -1) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one -1) (two ?O) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one -1) (two ?O) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from two to one" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-two-to-three
  ?s <-(8-puzzle
	(one ?O) (two -1) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?H) (three -1) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?H) (three -1) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from two to three" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-two-to-five
  ?s <-(8-puzzle
	(one ?O) (two -1) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?I) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?I) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from two to five" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-three-to-two
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three -1) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two -1) (three ?T) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two -1) (three ?T) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from three to two" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-three-to-six
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three -1) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?S) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?S) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from three to six" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-four-to-one
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one -1) (two ?T) (three ?H) (four ?O) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one -1) (two ?T) (three ?H) (four ?O) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from four to one" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-four-to-five
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?I) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?I) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from four to five" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-four-to-seven
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?E) (five ?I) (six ?S) (seven -1) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?E) (five ?I) (six ?S) (seven -1) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from four to seven" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-five-to-two
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two -1) (three ?H) (four ?F) (five ?T) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two -1) (three ?H) (four ?F) (five ?T) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from five to two" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-five-to-four
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four -1) (five ?F) (six ?S) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four -1) (five ?F) (six ?S) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from five to four" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-five-to-six
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?S) (six -1) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?S) (six -1) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from five to six" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-five-to-eight
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?G) (six ?S) (seven ?E) (eight -1) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?G) (six ?S) (seven ?E) (eight -1) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from five to seven" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-six-to-three
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three -1) (four ?F) (five ?I) (six ?H) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three -1) (four ?F) (five ?I) (six ?H) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from six to three" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-six-to-five
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?I) (seven ?E) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?I) (seven ?E) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from six to five" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-six-to-nine
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?N) (seven ?E) (eight ?G) (nine -1)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?N) (seven ?E) (eight ?G) (nine -1))))
  (print-boards ?s ?n)
  (printout t "Moved from six to nine" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-seven-to-four
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven -1) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?F) (eight ?G) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four -1) (five ?I) (six ?S) (seven ?F) (eight ?G) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from seven to four" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-seven-to-eight
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven -1) (eight ?G) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?G) (eight -1) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?G) (eight -1) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from seven to eight" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-eight-to-seven
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight -1) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven -1) (eight ?E) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven -1) (eight ?E) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from eight to seven" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-eight-to-nine
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight -1) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?N) (nine -1)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?N) (nine -1))))
  (print-boards ?s ?n)
  (printout t "Moved from eight to nine" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-eight-to-five
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight -1) (nine ?N))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?I) (nine ?N)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five -1) (six ?S) (seven ?E) (eight ?I) (nine ?N))))
  (print-boards ?s ?n)
  (printout t "Moved from eight to five" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))


(defrule from-nine-to-eight
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine -1))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight -1) (nine ?G)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight -1) (nine ?G))))
  (print-boards ?s ?n)
  (printout t "Moved from nine to eight" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule from-nine-to-six
  ?s <-(8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six ?S) (seven ?E) (eight ?G) (nine -1))
  (not (8-puzzle
	(one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?S)))
  =>
  (bind ?n (assert (8-puzzle (one ?O) (two ?T) (three ?H) (four ?F) (five ?I) (six -1) (seven ?E) (eight ?G) (nine ?S))))
  (print-boards ?s ?n)
  (printout t "Moved from nine to six" crlf)
  (assert (move (parent ?s) (next ?n)))
  (yes-or-no-halt))

(defrule found-solution
  (declare (salience ?*highest-priority*))
  ?s <- (8-puzzle (one 1) (two 2) (three 3) (four 4) (five 5) (six 6) (seven 7) (eight 8) (nine -1))
  (move (parent ?p) (next ?s))
  =>
  (print-boards ?s ?s)
  (assert (moves ?p ?s))
  (printout t "FOUND solution" crlf))

(defrule get-solution-path
  (declare (salience ?*highest-priority*))
  ?m <- (moves ?last-move $?other-moves)
  ?k <- (move (parent ?previous) (next ?last-move))
  =>
  (retract ?m ?k)
  (assert (moves ?previous ?last-move $?other-moves)))

(defrule print-solution-path
  (declare (salience ?*highest-priority*))
  ?m <- (moves ?last-move $?other-moves)
  (not (move (parent ?previous) (next ?last-move))) ;; this is the root
  =>
  (printout t "Found a solution" crlf)
  (print-board-moves ?m)
  (halt))

;; (print-boards(assert (8-puzzle (one 1) (two 5) (three 2) (four 4) (five -1) (six 3) (seven 7) (eight 8) (nine 6)))

