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

(deffunction yes-or-no-p (?question)
  (bind ?question (sym-cat ?question " (yes/y/no/n): "))
     (bind ?response (ask-question ?question yes no y n))
     (if (or (eq ?response yes) (eq ?response y))
         then TRUE 
         else FALSE))
