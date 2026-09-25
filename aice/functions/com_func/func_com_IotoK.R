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
# ID onto KB
# "Umrechnung:"   ElementeId  <->  Kurzbezeicnung
#  Wird grua angegeben, so taetigt er die Umrechnung via rbind(.self$ElHptKtg.df, .self$ElSndKtgList.df)
#                                          ansonsten via      ".self$ElHptKtg.df"
#  Wird kein match gefunden, so wird (indirekt) 'NA' zurueckgegeben
com_IotoK = function(.self, vIKs = c(), grua = "") {
  # Falscher Parameter?
  if(is.null(.self$chItoK(vIKs))) {
    return(NULL)
  }
  tmpKtg <- rbind(.self$getHptKat(),.self$getSndKat(grua, SILENT = TRUE))
  #
  if (class(vIKs) == "character") {
    # KB -> ID:
    return(tmpKtg$Nummer[match(vIKs,tmpKtg$Kurzbezeichnung)])
  } else if (class(vIKs) %in% c("integer","numeric")) {
    # ID -> KB:
    return(tmpKtg$Kurzbezeichnung[match(vIKs,tmpKtg$Nummer)])
  }
  # Soweit duerfte es nicht kommen ... :
  return(NA) 
}