(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

;; A very simple template for symptomps

(deftemplate symptom
  (slot name
	(type SYMBOL))
  (slot observed
	(default FALSE)))

(deftemplate diagnosis
  (slot name
	(type STRING)
	(default ?NONE)))

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

(deffunction yes-or-no-p (?question)
  (bind ?question (sym-cat ?question " (yes/y/no/n): "))
     (bind ?response (ask-question ?question yes no y n))
     (if (or (eq ?response yes) (eq ?response y))
         then TRUE 
         else FALSE))


;; Utility functions to let you revise the final diagnosis

(deffunction get-all-facts-by-names
  ($?template-names)
  (bind ?facts (create$))
  (progn$ (?f (get-fact-list))
	   (if (member$ (fact-relation ?f) $?template-names)
	       then (bind ?facts (create$ ?facts ?f))))
  ?facts)

(deffunction print-all-symptoms-status
  ()
  (bind ?i 1)
  (progn$ (?f (get-all-facts-by-names symptom))
	  (format t "(%d) %s: %s%n" ?i (fact-slot-value ?f name) (fact-slot-value ?f observed))
	  (bind ?i (+ ?i 1))))


;; decomposing functions in such a way is inefficient since we have
;; to recompute get-all-facts-by-names each time
;; modify the code to compute it only once and pass it as a parameter
(deffunction change-symptom-by-index
  (?index)
  (bind ?f (nth ?index (get-all-facts-by-names symptom)))
  (modify ?f (observed (not (fact-slot-value ?f observed)))))


(deffunction gen-int-list
  (?max-n)
  (bind ?int-list (create$))
  (loop-for-count (?i 1 ?max-n)
		  (bind ?int-list (create$ ?int-list ?i)))
  ?int-list)

(deffunction ask-to-change-symptom
  (?question)
  ;;(printout t  crlf)
  (printout t "Would you like to change some observed symptom?" crlf)
  (print-all-symptoms-status)
  (bind ?num-symptoms (length$ (get-all-facts-by-names symptom)))
  ;; allowed answers are the integers from 1 to num of symptoms or 'e' to exit
  (bind ?response (ask-question ?question (create$ (gen-int-list ?num-symptoms) e h)))
  (printout t "Response " ?response crlf crlf)
  (switch ?response
	  (case h
	    then (printout t "Halting inference" crlf)
	         (halt))
	  (case e then (return))
	  (default (change-symptom-by-index ?response)
      (ask-to-change-symptom ?question))))



;; CONTROL RULES

; (defrule starting-rule
;   (declare (salience ?*highest-priority*))
;   =>
;   (printout t crlf crlf)
;   (printout t "*** A simple diagnostic classifier ***" crlf crlf))

; (defrule print-disease
;   (declare (salience ?*highest-priority*))
;   (diagnosis ?disease)
;   =>
;   (printout t crlf ">>>> Diagnosed disease: " ?disease crlf crlf)
;   (if (yes-or-no-p "Would you like to revise the diagnosis?")
;       then (assert (revise-diagnosis))
;       else (halt)))

; (defrule revise-diagnosis
;   ""
;   (declare (salience ?*highest-priority*))
;   ?d <- (diagnosis ?)
;   ?r <- (revise-diagnosis)
;   =>
;   (retract ?d ?r)
;   ;; (printout t crlf "It is possible to revise the diagnosis" crlf crlf)
;   (ask-to-change-symptom "Enter the symptom number or 'e' to return or 'h' to stop: "))


;; RULES for DIAGNOSIS

(defrule gilbert-syndrome
  (symptom (name scleral-icterus) (observed TRUE))
  (symptom (name fever) (observed TRUE))
  (or (symptom (name stress) (observed TRUE))
      (symptom (name without-food) (observed TRUE)))
  =>
  (assert (diagnosis (name "Gilbert's syndrome"))))

(defrule acute-viral-hepatitis
  (symptom (name icterus) (observed TRUE))
  (symptom (name fever) (observed TRUE))
  (symptom (name young) (observed TRUE))
  (symptom (name tired) (observed TRUE))
  (symptom (name dyspepsia) (observed TRUE))
  (symptom (name enlarged-liver) (observed TRUE))
  =>
  (assert (diagnosis (name "Acute viral hepatitis"))))

(defrule cholecystitis
  (symptom (name icterus) (observed TRUE))
  (symptom (name fever) (observed TRUE))
  (symptom (name young) (observed FALSE))
  (symptom (name recurrent-pain) (observed TRUE))
  (symptom (name cholecyst-pain) (observed TRUE))
  =>
  (assert (diagnosis (name "Cholecystitis"))))

(defrule alchoolic-cirrhosis
  (symptom (name icterus) (observed TRUE))
  (symptom (name fever) (observed FALSE))
  (symptom (name young) (observed FALSE))
  (symptom (name alcohol-abuse) (observed TRUE))
  (symptom (name enlarged-liver) (observed TRUE))
  (symptom (name enlarged-spleen) (observed TRUE))
  =>
  (assert (diagnosis (name "Alchoolic cirrhosis"))))

; (defrule unknown-disease
;   (declare (salience ?*lowest-priority*))
;   (not (diagnosis (name ?)))
;   =>
;   (assert (diagnosis (name "Unknown disease"))))

(defrule icterus
  (symptom (name yellowish-eyes) (observed TRUE))
  (symptom (name yellowish-skin) (observed TRUE))
  =>
  (assert (symptom (name icterus) (observed TRUE))))

(defrule not-icterus
  (symptom (name yellowish-eyes) (observed TRUE))
  (symptom (name yellowish-skin) (observed FALSE))
  ?i <- (symptom (name icterus) (observed TRUE))
  =>
  (modify ?i (observed FALSE)))

(defrule scleral-icterus
  (symptom (name yellowish-eyes) (observed TRUE))
  (symptom (name yellowish-skin) (observed FALSE))
  =>
  (assert (symptom (name scleral-icterus) (observed TRUE))))

(defrule not-scleral-icterus
  (symptom (name yellowish-eyes) (observed TRUE))
  (symptom (name yellowish-skin) (observed TRUE))
  ?i <- (symptom (name scleral-icterus) (observed TRUE))
  =>
  (modify ?i (observed FALSE)))

; ;; RULES for QUESTIONING

; (defrule ask-for-fever
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name fever) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient got a fever?" ))
;   (assert (symptom (name fever) (observed ?answer))))

; (defrule ask-for-yellowish-eyes
;   (declare (salience ?*low-priority*))
;   (not (diagnosis  ?))
;   (not (symptom (name yellowish-eyes) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient yellowish eyes?"))
;   (assert (symptom (name yellowish-eyes) (observed ?answer))))

; (defrule ask-for-yellowish-skin
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name yellowish-skin) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient yellowish skin?"))
; 	(assert (symptom (name yellowish-skin) (observed ?answer))))

; (defrule ask-for-stress
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name stress) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Is the patient stressed?"))
;   (assert (symptom (name stress) (observed ?answer))))

; (defrule ask-for-without-food
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name without-food) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Is the patient without food?"))
;   (assert (symptom (name without-food) (observed ?answer))))

; (defrule ask-for-youth
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name young) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Is the patient young?" ))
;   (assert (symptom (name young) (observed ?answer))))

; (defrule ask-for-tiredness
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name tired) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Is the patient tired?"))
;   (assert (symptom (name tired) (observed ?answer))))

; (defrule ask-for-dyspepsia
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name dyspepsia) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient been diagnosed dyspepsia?"))
;   (assert (symptom (name dyspepsia) (observed ?answer))))

; (defrule ask-for-enlarged-liver
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name enlarged-liver) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient's liver enlarged?"))
;   (assert (symptom (name enlarged-liver) (observed ?answer))))

; (defrule ask-for-recurrent-pain
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name recurrent-pain) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient recurrent pain?"))
;   ;; (printout t crlf (observed ?answer))
;   (assert (symptom (name recurrent-pain) (observed ?answer))))

; (defrule ask-for-cholecyst-pain
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name cholecyst-pain) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Has the patient cholecyst pain?"))
;   ;; (printout t crlf (observed ?answer))
;   (assert (symptom (name cholecyst-pain) (observed ?answer))))

; (defrule ask-for-alcool-abuse
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name alcohol-abuse) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Does the patient use too much alcohol?"))
;   (assert (symptom (name alcohol-abuse) (observed ?answer))))

; (defrule ask-for-enlarged-spleen
;   (declare (salience ?*low-priority*))
;   (not (diagnosis ?))
;   (not (symptom (name enlarged-spleen) (observed ?)))
;   =>
;   (bind ?answer (yes-or-no-p "Is the patient's spleen enlarged?"))
;   (assert (symptom (name enlarged-spleen) (observed ?answer))))





