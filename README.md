## AIce
AKS-Interface (R-Interface fuer die AKS Niedersachsen)

### Das Paket enthaelt:

### [1] "AIceHowTo.html"
-	Eine Anleitung zur Benutzung von AIce.


### [2] "aice"-Ordner

- Enthaelt den einzubindenden Quellcode; und zudem den default-Ablageordner fuer die exportierten Kauffalldaten 
	  ("aice/data/Kauffalldaten") sowie fuer die exportierten Selektionsansaetze ("aice/data/Selektionsansaetze"). 
- Man kopiert den "aice"-Ordner einfach in den aktuellen R-Projektordner und bindet die Datei "aice.R" 
	  via 'source("<path_to_project>/aice/aice.R")' ein. 
- Es liegen bereits exportierte Beispiel-Selektionsansaetze in "aice/data/Selektionsansaetze"; der Ordner 
	  "aice/data/Kauffalldaten" ist natuerlich leer. In [1] ist beschrieben, wie die Daten zu exportieren sind. 

### [3] "Sample.qmd"
-	Ein kleines Beispiel. Man muss hier wie in [2] beschrieben ein R-Projekt anlegen, und sowohl "Sample.qmd" als
	auch [2] in den entsprechenden Projektordner kopieren.

