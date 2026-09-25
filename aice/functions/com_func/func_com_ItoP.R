###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
# com
#
# ID to Period-IDs:
com_ItoP = function(.self, ID = NULL, HID = FALSE) {
  if (is.null(ID) | !(class(ID) %in% c("integer","numeric"))) {
    warning("Fehlerhafter Index uebergeben ~ 'NULL' zurueckgegeben!")
    return(NULL)
  }
  grua <- ""
  if (.self$checkGrua()) {
      grua <-.self$getGrua()
  }
  # Falls grua = "", dann liefert .self$getSndKat(grua) -> NULL, und .self$getHptKat() bleibt uebrig
  tmpKtg <- rbind(.self$getHptKat(),.self$getSndKat(grua, SILENT = TRUE))
  #
  KB <- .self$IotoK(ID,grua)
  if (is.na(KB)) {
    warning(paste0("Zu der <ID: ", ID, "> wurde keine KB gefunden - NULL zurueckgegeben"))
    return(NULL)
  }
  if (!HID & com_Is_SubPeriod(ID)) {
    return(list(vID = ID, vKB = KB))
  } else {
    # HauptId rausfiltern
    strHptID <- substr(toString(ID), start=1, stop=3)
    # pruefen, ob Perioden existieren:
    strKtgIDs <- paste(tmpKtg$Nummer)
    strIDs <- grep(paste0("^", strHptID), strKtgIDs, value = TRUE)
    intIDs <- sort(as.integer(strIDs))
    # KBs verschaffen:
    KBs <- .self$IotoK(intIDs,grua)
    #moegliche NAs entfernen - also unaufgelöste (ID_p,KB_p) - Paare:
    notNAs <- which(!(is.na(intIDs) | is.na(KBs)))
    intIDs <- intIDs[notNAs]
    KBs <- KBs[notNAs]
    #
    return(list(vID = intIDs, vKB = KBs))
  }
}
#
#-------------------------------------------------------------------------------
#
# Ist das uebergenene ELement eine Unterperiode?
# (Kriterium mag sich in Zukunft aendern...)
com_Is_SubPeriod = function(ID = NULL) {
  if (is.null(ID)) {
    warning("Fehlerhefte ID uebergeben ~ 'NULL' zurueckgegeben.")
    return(NULL)
  }
  return(nchar(toString(ID)) > 3)
}  
