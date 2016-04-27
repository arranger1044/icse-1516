;;;;;
;;;
; 
(deffacts family-dag
        ;; parent
	(parent julian tony)
	(parent julian rose)
	(parent laura tony)
	(parent laura rose)
	(parent john julian)
	(parent john frank)
	(parent jenny julian)
	(parent jenny frank)
	(parent frank anna)
	(parent mary anna)
	(parent johna mary)
	;; male
	(male tony)
	(male julian)
	(male john)
	(male johna)
	(male frank)
	;; female
	(female rose)
	(female laura)
	(female jenny)
	(female anna)
	(female mary))


(defrule fatherhood
  "match-and-assert all father-child relationships"
  (male ?father)
  (parent ?father ?child)
  =>
  (assert (father ?father ?child)))

(defrule brother
  "matching brother relationships"
  (male ?brother)
  (parent ?parent ?child)
  (parent ?parent ?brother)
  (test (neq ?brother ?child))
  =>
  (assert (brother ?brother ?child)))


(defrule motherhood
  "match-and-assert all mother-child relationships"
  (female ?mother)
  (parent ?mother ?child)
  =>
  (assert (mother ?mother ?child)))

(defrule is-father
  "match-and-assert all fathers' names"
  (male ?father)
  (parent ?father ?child)
  =>
  (assert (is-father ?child)))

(defrule grandparent
  "match-and-assert all grand parent relationships"
  (parent ?grandparent ?parent)
  (parent ?parent ?child)
  =>
  (assert (grandparent ?grandparent ?child)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule uncle
  ""
  (parent ?grandparent ?father)
  (parent ?grandparent ?uncle)
  (parent ?father ?child)
  (male ?uncle)
  ;; this is crucial
  (test (neq ?uncle ?father))
  =>
  (assert (uncle ?uncle ?child)))

(defrule uncle-fc
  ""
  (parent ?grandparent ?father)
  (parent ?grandparent ?uncle&~?father)
  (parent ?father ?child)
  (male ?uncle)
   =>
   (assert (uncle-fc ?uncle ?child)))

(defrule married
  ""
  (mother ?parent-1 ?child)
  (father ?parent-2 ?child)
  (test (neq ?parent-1 ?parent-2))
  =>
  (assert (married ?parent-1 ?parent-2)))

(defrule cousins
  ""
  (parent ?grandparent ?parent1)
  (parent ?grandparent ?parent2)
  (test (neq ?parent1 ?parent2))
  (parent ?parent1 ?child1)
  (parent ?parent2 ?child2)
  (test (neq ?child1 ?child2))
  ; if we want to assert the relation only once
  (not (cousins ?child2 ?child1))
  =>
  (assert (cousins ?child1 ?child2)))

(defrule orphan-1
  ""
  (or (male ?person)
      (female ?person))
  (not (parent ?parent ?person))
  =>
  (assert (orphan ?person)))

;; activate first the rules for mother and father first
(defrule orphan-2
  ""
  (or (male ?person)
      (female ?person))
  (or
   (not
    (father ?parent ?person))
   (not
    (mother ?parent ?person))
   (and
    (not
     (father ?parent ?person))
    (not
     (mother ?parent ?person))))
  =>
  (assert (orphan-2 ?person)))




