;;;;;
;; A very simple conceptualization of a family directed acyclic graph

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




