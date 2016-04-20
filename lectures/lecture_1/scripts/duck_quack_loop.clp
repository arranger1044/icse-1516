;; How to create an infinite loop in clips
; by binding to rules in mutual recursion

; (load ...)
; (reset)
; (run)

(deffacts first-fact
  (animal-is duck))

(defrule duck-quack
  ?d <- (animal-is duck)
  =>
  (retract ?d)
  (printout t "if it is a duck, then it does quack" crlf)
  (assert (sound-is quack)))
(defrule quack-duck
  ?q <- (sound-is quack)
  =>
  (retract ?q)
  (printout t "if it does quack, then it is a duck" crlf)
  (assert (animal-is duck)))