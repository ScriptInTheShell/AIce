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
# ID|KB to SPT
com_IKtoS = function(.self, vIKs = c(), grua="") {
  # Falscher Parameter?
  if(is.null(.self$chItoK(vIKs))) {
    return(NULL)
  }
  tmpKtg <- rbind(.self$getHptKat(),.self$getSndKat(grua, SILENT = TRUE))
  #
  if (class(vIKs) == "character") {
    # KB -> SPT:
    return(tmpKtg$SPT[match(vIKs,tmpKtg$Kurzbezeichnung)])
  } else if (class(vIKs) %in% c("integer","numeric")) {
    # ID -> SPT:
    return(tmpKtg$SPT[match(vIKs,tmpKtg$Nummer)])
  }
  # Soweit duerfte es nicht kommen ... :
  return(NA) 
}