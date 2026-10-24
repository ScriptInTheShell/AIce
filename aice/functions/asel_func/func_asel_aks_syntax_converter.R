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
asel_aks_syntax_converter = function(InputStr = "") {
  ErrRet       <- ""
  tmpVec       <- ""
  tmpNegValVec <- c()
  omitInds     <- c()
  startPos     <- 0
  stopPos      <- 0
  ## Fehlerhafte Eingabe abstrafen:
  if (class(InputStr) != "character") {
    warning(paste0("aks_syntax_converter: Fehlerhafte Eingabe: ", InputStr, " ~ emptystring zurureckgegeben."))
    return(ErrRet)
  }
  tmpVec <- strsplit(InputStr, split = "#")[[1]]
  ## Wenn irgendwo '<' (inbes. '<','<=','<>') enthalten, dann '='-ODER-Segment hinzufuegen.
   # Andernfalls ist nichts zu bereinigen ~ return(InputStr): 
  if (any(grepl("<",tmpVec))) {
    tmpVec <- c("=", tmpVec)
  } else {
    return(InputStr)
  }
  ## Negator-Syntax bereinigen:
   # In den einzelnen ODER-Kloetzen etwaige Leerzeichen und Tabstopps entfernen:
   # In der AKS-Eingabe ist naemlich gleichbedeutend/erlaubt:
   #      < >A <> B <>C   ~   <>A<>B<>C 
   # und wird dann auch so im Selektionsansatz ausgegeben.
   # Wir muessen das nun zunaechst bereinigen:
  helpVec <- com_helper_str_cleanST(tmpVec)
  ## Nun auf "<>" testen
  IndsNegs <- which(grepl("<>",helpVec))
  # Wenn irgendwo '<>' enthalten, dann transformieren:
  #       <>Wert1<>Wert2...<>Wertn  ->  <>(Wert1#Wert2#...#Wertn)
  if (length(IndsNegs) > 0) {
    for (ind in IndsNegs) {
      tmpNegValVec <- c()
            tmpPos <- gregexpr("<>", helpVec[ind])[[1]]
              nAnz <- length(tmpPos)
      #
      for (i in 1:nAnz) {
        # Erstes Zeichen nach '<>':
        startPos <- tmpPos[i] + 2
        if (i < nAnz) {
          # Letztes Zeichen vor dem naechsten '<>':
          stopPos <- tmpPos[i + 1] - 1
        } else {
          # Letztes Zeichen:
          stopPos <- nchar(helpVec[ind])
        }
        # Nur Wert auslesen, falls vorhanden ~ wenn also der String nicht so aussieht: "<><>", bzw. auf "<>" endet.
        if (startPos <= stopPos) {
          tmpNegValVec <- c(tmpNegValVec, substr(helpVec[ind], start = startPos, stop = stopPos))
        }
      }
      if (is.null(tmpNegValVec)) {
        omitInds <- c(omitInds, ind) 
      } else if (length(tmpNegValVec) == 1) {
        tmpVec[ind] <- paste0("<>", tmpNegValVec[1])
      } else {
        tmpVec[ind] <- paste0("<>(", paste(tmpNegValVec, collapse = "#") ,")") 
      }
    }
  }
  ## Schließlich den Selektionsstring wieder via '#' zusammentackern:
  if (!is.null(omitInds)) {
      tmpVec <- tmpVec[-omitInds]
  }
  return(Vec_to_InptStr(tmpVec))
}
