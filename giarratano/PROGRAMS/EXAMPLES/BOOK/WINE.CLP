
;;;======================================================
;;;   Backward Chaining Program
;;;
;;;     This program was introduced in Section 12.4.
;;;     It tries to select the best color of wine
;;;     to serve with a meal.
;;;
;;;     CLIPS Version 6.0 Example
;;;
;;;     To execute, merely load, reset and run. The
;;;     correct wine color is not printed, but can
;;;     be determined by viewing the facts.
;;;======================================================

(defmodule BC 
   (export deftemplate rule goal attribute))

(deftemplate BC::rule
   (multislot if)
   (multislot then))

(deftemplate BC::attribute 
   (slot name)
   (slot value))

(deftemplate BC::goal 
   (slot attribute))

(defrule BC::attempt-rule
   (goal (attribute ?g-name))
   (rule (if ?a-name $?)
         (then ?g-name $?))
   (not (attribute (name ?a-name)))
   (not (goal (attribute ?a-name)))
   =>
   (assert (goal (attribute ?a-name))))

(defrule BC::ask-attribute-value
   ?goal <- (goal (attribute ?g-name))
   (not (attribute (name ?g-name)))
   (not (rule (then ?g-name $?)))
   =>
   (retract ?goal)
   (printout t "What is the value of " ?g-name "? ")
   (assert (attribute (name ?g-name) 
                      (value (read)))))

(defrule BC::goal-satisfied
   (declare (salience 100))
   ?goal <- (goal (attribute ?g-name))
   (attribute (name ?g-name))
   =>
   (retract ?goal))

(defrule BC::rule-satisfied
   (declare (salience 100))
   (goal (attribute ?g-name))
   (attribute (name ?a-name)
              (value ?a-value))
   ?rule <- (rule (if ?a-name is ?a-value) 
                  (then ?g-name is ?g-value))
   =>
   (retract ?rule)
   (assert (attribute (name ?g-name) 
                      (value ?g-value))))

(defrule BC::remove-rule-no-match
   (declare (salience 100))
   (goal (attribute ?g-name))
   (attribute (name ?a-name) (value ?a-value))
   ?rule <- (rule (if ?a-name is ~?a-value) 
                  (then ?g-name is ?g-value))
   =>
   (retract ?rule))

(defrule BC::modify-rule-match
   (declare (salience 100))
   (goal (attribute ?g-name))
   (attribute (name ?a-name) (value ?a-value))
   ?rule <- (rule (if ?a-name is ?a-value and 
                      $?rest-if) 
                  (then ?g-name is ?g-value))
   =>
   (retract ?rule)
   (modify ?rule (if $?rest-if)))

(defmodule MAIN (import BC deftemplate rule goal))

(deffacts MAIN::wine-rules
   (rule (if main-course is red-meat) 
         (then best-color is red))

   (rule (if main-course is fish)
         (then best-color is white))

   (rule (if main-course is poultry and
             meal-is-turkey is yes)
         (then best-color is red))

   (rule (if main-course is poultry and
             meal-is-turkey is no)
         (then best-color is white)))

(deffacts MAIN::initial-goal
   (goal (attribute best-color)))

(defrule MAIN::start-BC
   =>
   (focus BC))


