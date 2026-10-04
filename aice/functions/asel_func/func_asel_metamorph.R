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
asel_metamorph = function(InputStr = "") {
  ## Die benoetigten Variablen initialisieren:
  ErrRet       <- list(INVERT = FALSE, SEPASTR = "")
  blnINV       <- FALSE
  tmpInvInhStr <- ""
  tmpInvPrtStr <- ""
  retStrV      <- c()
  retPrtV      <- c()
  ## Fehlerhafte Eingabe abstrafen:
  if (class(InputStr) != "character") {
    warning(paste0("metamorph: Fehlerhafte Eingabe: ", InputStr, " ~ emptystring zurureckgegeben."))
    return(ErrRet)
  }
  ## Zunaechst auf den "<>(...)"-Operator testen:
  blnNegOpEx <- grepl("<>\\(.*", InputStr) & grepl("\\)$", InputStr)
  if (!blnNegOpEx) {
    # wenn kein '<>(...)' im String enthalten ist dann einfach an den '#'s aufsplitten:
    tmpVec <- strsplit(InputStr, split="#")[[1]]
  } else {
    # wenn ein '<>(...)' im String enthalten ist, dann zunächst auf grobe Syntaxfehler prüfen:
    #
    # Positionen umschließender Klammern:
    pKa <- gregexpr("\\(", InputStr)[[1]]
    pKz <- gregexpr("\\)", InputStr)[[1]]
    #
    # Im Falle ganz grober Syntaxfehler abbrechen:
    if (length(pKa) != 1 | length(pKz) != 1) {
      warning(paste0("metamorph: Fehlerhafte Eingabe: ", InputStr, " ~ emptystring zurureckgegeben.\nDie Anzahl der '(', ')' muss jeweils 1 betragen!"))
      return(ErrRet)
    }
    if (pKa >= pKz) {
      warning(paste0("metamorph: Fehlerhafte Eingabe: ", InputStr, " ~ emptystring zurureckgegeben.\nTeilausdruck der Form ')...<>(' detektiert!"))
      return(ErrRet)
    } else { 
      ## Nun InputStr an den '#'s aufsplitten, die NICHT in <>(...) liegen:
      #
      # Schonmal Default-Wert festlegen fuer den Fall, das gar kein '#' enthalten ist, oder nur welche in <>(...):
      tmpVec <- InputStr
      # Positionen der '#' bestimmen:
      RautePos <- gregexpr("#", InputStr)[[1]]
      # Gib es ueberhaupt ein '#' ?
      if (RautePos > 0) {
        # Wenn ja, dann schauen, ob es welche außerhalb von <>(...) gibt:
        tmpInd <- which((RautePos < pKa) | (RautePos > pKz))
        if (length(tmpInd) > 0) {
          # die Positionen der '#' in <>(...) extrahieren:
          RautePos <- RautePos[tmpInd]
          # tmpVec updaten:
          tmpVec   <- str_pos_splt(InputStr, RautePos)
        }
      }
    }
  }
  ## Nun in den einzelnen ODER-Kloetzen etwaige Leerzeichen und Tabstopps entfernen:
   # hlpVec dient dazu Leer- sowie Metaeintraege sicher zu detektieren.
  hlpVec <- com_helper_str_cleanST(tmpVec)
  # Nun etwaige Leereintraege/Kloetze entfernen:
  if (length(which(hlpVec == "")) >= 1) {
    hlpVec <- hlpVec[- which(hlpVec == "")]
    tmpVec <- tmpVec[- which(hlpVec == "")]
  }
  ## Indexpositionen der Metas und <>(...)s bestimmen:
  IposMetaB <- which((hlpVec == '!') | (hlpVec == "istbelegt"))
  IposMetaN <- which((hlpVec == '=') | (hlpVec == "nichtbelegt"))
  IposNeg   <- which(grepl("<>\\(.*", hlpVec) & grepl("\\)$", hlpVec))
  #
  ## Wenn ein INV enthalten ist, dann wird das in 'blnINV' vermerkt; und der erste (am weitesten links stehende) 
   # davon herausgepickt, und dessen <>("Inhalt") in tmpInvInhStr extrahiert.
   # der Ausdruck selbst wird in tmpInvPrtStr gespeichert
   # etwaige weitere "<>(...)"-Ausdruecke werden geschreddert
  if (length(IposNeg) > 0) {
    blnINV       <- TRUE
    tmpInvStr    <- tmpVec[IposNeg[1]]
    startPos     <- regexpr("<>\\(", tmpInvStr)[[1]] + 3
    stopPos      <- nchar(tmpInvStr) - regexpr("\\)",str_rev(tmpInvStr) )[[1]]
    tmpInvInhStr <- substr(tmpInvStr, start = startPos, stop = stopPos)
    tmpInvPrtStr <- substr(tmpInvStr, start = (startPos-3), stop = (stopPos+1))
  }
  ## Wenn beide META-Symbole !/= im Selektionsstring enthalten waren, dann Abbruch:
  if (length(IposMetaB) > 0 & length(IposMetaN) > 0) {
    warning(paste0("metamorph: Fehlerhafte Eingabe - '!' und '=' treten beide auf: ", InputStr, "~ emptystring zurureckgegeben."))
    return(ErrRet)
  } else if (length(IposMetaB) > 0) {
    ## Nur ein Metakriterium uebrig lassen (Dopplungen entfernen) 
    # '!' hat Vorrang:
    #
    retStrV <- c("ist belegt")
    retPrtV <- c("!")
    # "ist belegt" steht immer alleine - auch etwaige Negationsterme werden wieder verworfen:
    blnINV <- FALSE
  } else if (length(IposMetaN) > 0) {
    # nun '='
    #
    if (blnINV) {
      # Wenn ein '<>(...)'-Term vorhanden ist, dann wird außer diesem und dem Meta-Kriterium alles Andere verworfen:
      retStrV <- c("nicht belegt", tmpInvInhStr) 
      retPrtV <- c("=", tmpInvPrtStr) 
    } else {
      # Wenn kein '<>(...)'-Term vorhanden ist, dann wird alles mitgenommen:
      retStrV <- c("nicht belegt", tmpVec[- IposMetaN]) 
      retPrtV <- c("=", tmpVec[- IposMetaN])
    }    
  } else {
    # Kein Meta-Kriterium vorhanden:
    #
    if (blnINV) {
      # Wenn ein '<>(...)'-Term vorhanden ist, dann wird außer diesem alles Andere verworfen:
      retStrV <- tmpInvInhStr
      retPrtV <- tmpInvPrtStr
    } else {
      retStrV <- tmpVec
      retPrtV <- retStrV
    }
  }
  ## Schließlich den Selektionsstring wieder via '#' zusammentackern: 
  SepaSTR  <- ""
  PrtSTR   <- ""
  #
  blnFirst <- TRUE
  for (str in retStrV) {
    if (blnFirst) { SepaSTR <- str 
    } else        { SepaSTR <- paste0(SepaSTR, "#", str) }
    #
    blnFirst <- FALSE
  }
  blnFirst <- TRUE
  for (str in retPrtV) {
    if (blnFirst) { PrtSTR <- str 
    } else        { PrtSTR <- paste0(PrtSTR, "#", str) }
    #
    blnFirst <- FALSE
  }
  ## Rueckgabe:
  return(list(INVERT = blnINV, SEPASTR = SepaSTR, PRTSTR = PrtSTR))  
}