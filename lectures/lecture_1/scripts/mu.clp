;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A take on the MU Game in CLIPS
; as defined in "Godel, Escher and Bach", Chapter 1
; (spoiler MU cannot be generated)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts axiom
  (miu-string M I))

(defrule rule-I
  "If a string ends with I, then you can add a U at the end"
  ?s <- (miu-string $?prefix I)
  =>
  (retract ?s)
  (assert (miu-string $?prefix I U))
  (printout t (implode$ $?prefix) I U " (by applying rule I)" crlf)
  (assert (key-command (get-char t))))

(defrule rule-II
  "If a string is in the form Mx, then you can rewrite it as Mxx"
  ?s <- (miu-string M $?x)
  =>
  (retract ?s)
  (assert (miu-string M $?x $?x))
  (printout t M (implode$ $?x) (implode$ $?x) " (by applying rule II)" crlf)
  (assert (key-command (get-char t))))

(defrule rule-III
  "If a string contains III, then you can write a rule that has U instead of it"
  ?s <- (miu-string $?prefix I I I $?suffix)
  =>
  (retract ?s)
  (assert (miu-string $?prefix U $?suffix))
  (printout t (implode$ $?prefix) U (implode$ $?suffix) " (by applying rule III)" crlf)
  (assert (key-command (get-char t))))

(defrule rule-IV
  "If a string contains UU, then you can remove it"
  ?s <- (miu-string $?prefix U U $?suffix)
  =>
  (retract ?s)
  (assert (miu-string $?prefix $?suffix))
  (printout t (implode$ $?prefix) (implode$ $?suffix) " (applying rule IV)" crlf)
  (assert (key-command (get-char t))))

(defrule halt-mu
  "Halts the inference if 'e' is entered on stdin"
  (declare (salience 10000))
  (key-command 101)
  =>
  (halt))