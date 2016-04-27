(deftemplate movie
  (slot title
	(type STRING)
	(default ?NONE))
  (slot genre
	(type SYMBOL)
	(allowed-symbols noir indie romance action cartoon surreal comedy))
  (slot main-actor
	(type STRING))
  (multislot actors
	     (type STRING))
  (slot director
	(type STRING))
  (slot producer
	(type STRING))
  (slot box-office
	(type NUMBER)))

(deffacts some-movies
  (movie (title "Un Chien Andalou")
	 (director "L. Bunuel")
	 (producer "S. Dalì")
	 (main-actor "")
	 (genre surreal)
	 (actors)
	 (box-office 10.0))
  (movie (title "Coming to America")
	 (director "John Landis")
	 (main-actor "Eddie Murphy")
	 (genre comedy)
	 (actors "Eddie Murphy" "Arsenio Hall")
	 (box-office 550.0))
  (movie (title "A Romantic Film")
	 (director "")
	 (producer "")
	 (main-actor "")
	 (genre romance)
	 (actors)
	 (box-office 1560.0))
  (movie (title "Ecce Bombo")
	 (director "Nanni Moretti")
	 (producer "Nanni Moretti")
	 (main-actor "Nanni Moretti")
	 (genre comedy)
	 (actors "Nanni Moretti")
	 (box-office 40.0)))

(defrule interesting-movies
  "Defining interesting films as those not belonging to the romance genre"
  (movie (title ?title) (genre ~romance))
  =>
  (printout t "An interesting movie " ?title crlf))

(defrule hipster-movies
  ""
  (movie (title ?title) (genre indie | surreal))
  =>
  (printout t "Probably a hipster movie " ?title crlf))

(defrule hipster-movies-I
  ""
  (movie (title ?title) (genre indie))
  =>
  (printout t "Probably a hipster movie (indie): " ?title crlf))

(defrule hipster-movies-II
  ""
  (movie (title ?title) (genre surreal))
  =>
  (printout t "Probably a hipster movie (surreal): " ?title crlf))

(defrule hipster-movies-or
  ""
  (or (movie (title ?title) (genre indie))
      (movie (title ?title) (genre surreal)))
  
  =>
  (printout t "Probably a hipster movie (OR): " ?title crlf))

(defrule intellectual-movies
  ""
  (movie (title ?title) (genre ?genre&~romance&~action&~comedy))
  =>
  (printout t "Likely a movie for intellectuals: ", ?title crlf))

(defrule eddie-murphy-movies
  ""
  (movie (title ?title) (actors $?actors&:(member$ "Eddie Murphy" $?actors)))
  =>
  (printout t "A film with Eddie Murphy: " ?title crlf))

(defrule blockbuster-movies
  ""
  (movie (title ?title) (box-office ?total&:(> ?total 500.0)))
  =>
  (printout t "Surely a blockbuster: " ?title crlf))

(defrule homemade-movies
  ""
  (movie (title ?title) (director ?director) (producer ?producer) (main-actor ?actor))
  (test (eq ?producer ?director ?actor))
  =>
  (printout t "This is a one made film: " ?title crlf))

(defrule one-blockbuster
  ""
  (exists (movie (box-office ?total&:(> ?total 500.0))))
  =>
  (printout t "There was at least one movie to engross more than 500.0" crlf))