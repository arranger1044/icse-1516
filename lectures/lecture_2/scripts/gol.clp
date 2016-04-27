(defglobal ?*num-rows* = 5)
(defglobal ?*num-cols* = 5)
(defglobal ?*high-priority* = 100)
(defglobal ?*time* = 0)

(deffunction print-list
  "An utility function that prints the contents of a multifield"
  ($?list)
  (if (> (length$ ?list) 0)
      then (printout t (nth$ 1 ?list))
      (print-list (rest$ ?list))
      else (printout t crlf)))

(deffunction vec-coords
  "converts a pair of matrix coordinates into vector ones"
  (?x ?y ?width)
  (+ (* (- ?x 1) ?width) ?y))

(deffunction get-8-neighborhood
  "extracts the 8-neighbors as a list of pairs of integers"
  (?x ?y ?max-x ?max-y)
  (bind ?neighbors (create$))
  (loop-for-count (?i -1 1)
		  (loop-for-count (?j -1 1)
				  (if (and (>= (+ ?x ?i) 1)
					   (<= (+ ?x ?i) ?max-x)
					   (>= (+ ?y ?j) 1)
					   (<= (+ ?y ?j) ?max-y)
					   (or (<> ?i 0) (<> ?j 0)))
				      then (bind ?neighbors (create$ ?neighbors (+ ?x ?i) (+ ?y ?j))))))
  return ?neighbors)

(deffunction is-cell-alive
  "checks for liveness of a cell in the world given its coordinates"
  (?x ?y ?width $?world)
  (nth (vec-coords ?x ?y ?width) ?world))

; (deffunction filter-alive-cells
;   "filters alive cells from a list of pairs of coordinates")

(deffunction num-alive-neighbors
  ""
  (?x ?y ?height ?width $?world)
  (bind ?num-alive-neigh 0)
  (bind ?neighborhood (get-8-neighborhood ?x ?y ?height ?width))
  (bind ?length (div (length$ ?neighborhood) 2))
  (loop-for-count (?i 1 ?length)
		  ; (printout t ?i t crlf)
		  (bind ?neigh-x (nth$ (- (* ?i 2) 1) ?neighborhood))
		  (bind ?neigh-y (nth$ (* ?i 2) ?neighborhood))
		  ; (printout t "translated" ?neigh-x ?neigh-y t crlf)
		  (if (is-cell-alive ?neigh-x ?neigh-y ?width ?world)
		      then (bind ?num-alive-neigh (+ ?num-alive-neigh 1))))
  return ?num-alive-neigh)

(deffunction will-the-cell-live
  "Determine if a cell will live based on the exact number of alive neighbor cells"
  (?x ?y ?height ?width $?world)
  (bind ?is-alive (is-cell-alive ?x ?y ?width $?world))
  (bind ?n-alive (num-alive-neighbors ?x ?y ?height ?width $?world))
  (if ?is-alive
      then (if (or (< ?n-alive 2) (> ?n-alive 3))
	       then (return FALSE)
	       else (return TRUE))
      else (if (= ?n-alive 3)
	       then (return TRUE)
	       else (return FALSE))))

(deffunction print-world
  "Printing the world as a grid with . (dead cells) and X (alive ones)"
  (?height ?width $?world)
  (loop-for-count (?i 1 ?height)
		  (loop-for-count (?j 1 ?width)
				  (if (is-cell-alive ?i ?j ?width ?world)
				      then (printout t " X ")
				      else (printout t " . ")))
		  (printout t crlf)))

(deffunction evolve-world
  "Creating a new world status"
  (?height ?width $?world)
  (bind ?next-world (create$))
  (loop-for-count (?i 1 ?height)
		  (loop-for-count (?j 1 ?width)
				  (bind ?cell-status FALSE)
				  (if (will-the-cell-live ?i ?j ?height ?width ?world)
				      then (bind ?cell-status TRUE))
				  (bind ?next-world (create$ ?next-world ?cell-status))))
  return ?next-world)

(defrule update-world
  "A simple rule to update the world configuration"
  ?world <- (world $?world-status)
  =>
  ;;(printout t "matching")
  (retract ?world)
  (bind ?new-world-status (evolve-world ?*num-rows* ?*num-cols* ?world-status))
  (assert (world ?new-world-status))
  (bind ?*time* (+ ?*time* 1)) ;; updating time
  (printout t "New world (at time " ?*time* "): " t crlf)
  (print-world ?*num-rows* ?*num-cols* ?new-world-status)
  (assert (key-command (get-char t))) ;; press 'e' to exit
  (printout t crlf))

(defrule end-evolution
  "Ending evolution "
  (declare (salience ?*high-priority*))
  ?k <- (key-command 101)
  =>
  (retract ?k)
  (printout t crlf "Ending evolution" crlf)
  (halt))

;; Initial board state
(deffacts initial-world
  (world FALSE FALSE FALSE FALSE FALSE
	 FALSE FALSE TRUE FALSE FALSE
	 FALSE FALSE TRUE FALSE FALSE
	 FALSE FALSE TRUE FALSE FALSE
	 FALSE FALSE FALSE FALSE FALSE))
