;;;
;; Conceptualizing the same family dag with deftemplated facts

(deftemplate person
  (slot name
	(type STRING))
  (slot gender
	(allowed-symbols male female))
  (multislot children
	(type STRING)))

(deffacts templated-family-dag
  (person (name "tony") (gender male))
  (person (name "julian") (gender male) (children "tony" "rose"))
  (person (name "john") (gender male) (children "julian" "frank"))
  (person (name "johna") (gender male) (children "mary"))
  (person (name "frank") (gender male) (children "anna"))
  (person (name "rose") (gender female))
  (person (name "laura") (gender female) (children "tony" "rose"))
  (person (name "jenny") (gender female) (children "julian" "frank"))
  (person (name "anna") (gender female))
  (person (name "mary") (gender female) (children "anna")))

(deftemplate father
  (slot name
	(type STRING))
  (multislot children
	(type STRING)))

(defrule is-father
  "asserting fatherhood"
  (person (name ?father) (gender male) (children $?children))
  =>
  (assert (father (name ?father) (children $?children))))

;;; more rules...