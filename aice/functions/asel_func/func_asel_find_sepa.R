###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
# asel
#
# ALL = FALSE ~ default: Es wird nur nach der uebergebenen ID gesucht. 
#                        - Definiert man ein Sepa auf einem Perioden-Hauptelement, 
#                          so werden auch die Unterelemente als gesetzt erkannt und gefunden.
# ALL = TRUE:            Sucht man nach einem Perioden-Unter/Hauptelement, so werden alle
#                        Sepas gesucht und gefunden, die in dem entsprechenden Periodenbereich 
#                        definiert sind.
#
asel_find_sepa =  function(.self, IdKb = NULL, ALL = FALSE) {
  # Keine Parameter vorhanden?
  if (.self$nObAsp.int == 0) {
    message("Die Selektionsparameterliste ist leer  ~ 'NULL' zurueckgegeben!")
    return(NULL)
  }
  # Keine/Falsche GRUA festgelegt?
  grua <- .self$getGrua()
  if (is.null(grua)) {
    return(NULL)
  }
  # Im Folgenden mit den IDs arbeiten:
  stat <- .self$chItoK(IdKb)
  if (is.null(stat)) { return(NULL)
  } else if (!stat)   { # Falls KBs, dann in IDs umrechnen:
                        ID <- .self$IotoK(IdKb,grua) }
  #
  if(!ALL) { # nur diese ID, nicht die der assoziierten Periodenelemente 
    vIDs <- ID
  } else {   # alle assoziierten Periodenelemente
    retLst <- .self$ItoP(ID, HID = TRUE)
      vIDs <- retLst$vID
    if(is.null(vIDs)) {
      warning("Fehler in ItoP aufgetreten ~ 'NULL' zurueckgegeben!")
      return(NULL)
    }
  } 
  # Sepas sammeln:
  vIndices <- c()
  for (i in 1:.self$nObAsp.int) {
    if(any(vIDs %in% .self$ObAsp.lst[[i]]$Meta.lst$ID.lst$vID)) {
      vIndices <- c(vIndices, i)
    }
  }
  if(is.null(vIndices)) {
    message("Keinen Eintrag gefunden ~ 'NULL' zurueckgegeben!")
    return(NULL)
  } else {
    return(vIndices)
  }
}