###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
###########################################################################################################################
#
# com
#
# check ID to KB conversion neccessary?
com_chItoK = function(vecIKs = c()) {
  # Keine IDs/KBs uebergeben?  
  if (is.null(vecIKs)) {
    #warning("Kein Identifier uebergeben. -> 'NULL' zurueckgegeben!")
    return(NULL)
  }
  # Falscher Datentyp?
  if (!(class(vecIKs) %in% c("character","integer","numeric"))) {
    warning("Uebergabeparameter muss vom Typ 'character', 'numeric' oder 'integer' sein. -> 'NULL' zurueckgegeben!")
    return(NULL)
  }
  if (class(vecIKs) %in% c("integer","numeric")) {
    return(TRUE)
  }
  # class = 'character' ~ Es muss Nichts transformiert werden.
  return(FALSE)
}