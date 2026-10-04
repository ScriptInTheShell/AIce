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
com_typeCast = function(.self, dFrm = NULL, grua = "", NoEntryErrMsg = FALSE, SptErrMsg = FALSE, TPCST = TRUE) {
  if (is.null(dFrm)) {
    warning("typeCast: Keine Daten uebergeben ~ 'NULL' zurueckgegeben.")
    return(NULL) 
  }
  for (col in colnames(dFrm)) {
    SPT <- .self$IKtoS(col, grua)
    if (is.na(SPT)) {
      if (NoEntryErrMsg) {
        message(paste0("typeCast: (SPT/IKtoS) - Fehlender Eintrag im Elementekatalog fuer \"", col, "\" ~ ggf. ergaenzen!"))
      }
    } else if ((SPT == "") & SptErrMsg) {
      message(paste0("typeCast: SPT/IKtoS - Fehlende SPT im Elementekatalog fuer \"", col, "\" ~ ggf. ergaenzen!"))
      #
    } else if (TPCST) {
       NA_Dummy <- dFrm[col] 
      dFrm[col] <- switch(SPT,
                          "DAT"  = as.Date(dFrm[,col], format = "%d.%m.%Y"),
                          "DEZ"  = as.numeric(gsub(",", ".", dFrm[,col])), 
                          "INT"  = as.integer(dFrm[,col]),
                          "KINT" = as.integer(dFrm[,col]),
                          "NINT" = as.integer(dFrm[,col]),
                          "JINT" = as.integer(dFrm[,col]),
                          "NSTR" = as.integer(dFrm[,col]),
                          "STR"  = dFrm[col],
                          "FSTR" = dFrm[col],
                          "KSTR" = dFrm[col],
                          "BOOL" = dFrm[col],
                          "POS"  = dFrm[col],
                          dFrm[col]
                          )
      newNAs <- setdiff(which(is.na(dFrm[col])), which(is.na(NA_Dummy)))
      if (length(newNAs) >= 1) {
        warning(paste0("typeCast: 'NA' durch Typecast erzeugt in Spalte \"",col, "\":  
                       ~>   SPT <",SPT,">   |   VALUE <",NA_Dummy[newNAs,col],">   |   KFKZ <", dFrm$KFKZ[newNAs], ">   |   GRUA <", grua,">"))
      }
    }
  }
  if (TPCST) { 
    return(dFrm) 
  } else {
    return(NULL)
  }
}
