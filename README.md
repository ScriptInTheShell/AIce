## AIce
AKS-Interface (R-Interface for the AKS Niedersachsen)

### Das Paket enthaelt:

### [1] "aiceHowTo.pdf"
-	Eine Anleitung zur Benutzung von AICE.


### [2] "aice"-Ordner

- Enthaelt den einzubindenden Quellcode; und zudem einen Ablageordner fuer die exportierten Kauffalldaten 
	  ("aice/data/Kauffalldaten") sowie fuer die exportierten Selektionsansaetze ("aice/data/Selektionsansaetze"). 
- Man kopiert den "aice"-Ordner einfach in den aktuellen R-Projektordner und bindet die Datei "aice.R" 
	  via 'source("<path_to_project>/aice/aice.R")' ein. 
- Es liegt bereits ein exportierter Beispiel-Selektionsansatz in "aice/data/Selektionsansaetze" ("Testansatz_bb.csv"); der Ordner 
	  "aice/data/Kauffalldaten" ist natuerlich leer. In [1] ist beschrieben, wie die Daten zu exportieren sind. 

### [3] "Sample.Rmd"
-	Ein kleines Beispiel. Man muss hier wie in [2] beschrieben ein R-Projekt anlegen, und sowohl "Sample.Rmd" als
	auch [2] in den entsprechenden Projektordner kopieren.

