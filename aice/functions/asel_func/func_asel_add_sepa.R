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
asel_add_sepa = function(.self, IdKb = NA, InputStr = c(""), pInd = 0, AKS = FALSE) {
  # Falls Parameter in Vektorform  -  vgl. asel.rc$loadStab()  ~>  asel.rc$merge()  ~>  asel.rc$addSepa():
  InputStr <- Vec_to_InptStr(InputStr)
  if(is.na(IdKb) | com_helper_str_cleanST(InputStr) == "") {
    stop(paste0("Unvollstaendige Eingabe: ", 
                " | IdKb: ",      toString(IdKb),
                " | SepaStr: ",   InputStr,
                " |"))
  }
  #
  tmpID      <- NA
  tmpKB      <- NA
  tmpSPT     <- NA
  grua       <- .self$getGrua(SILENT = TRUE)
  # Wenn AKS = TRUE, wird "AKS-Logik" angenommen ~ NA (das NICHTS) ist kleiner und ungleich als jeder richtige Wert ... :-O
  if (AKS & (.self$IKtoS(IdKb, grua) %in% c("DAT","DEZ","INT","KINT","NINT","JINT"))) {
    InputStr <- .self$AKS_Syntax_Converter(InputStr)
  }
  # '!' -> "ist belegt", "=" -> "nicht belegt", Inversion <>(...) extrahieren 
    tmpList  <- .self$metamorph(InputStr)
      tmpINV <- tmpList[["INVERT"]]
  tmpPrtSTR  <- tmpList[["PRTSTR"]]
  tmpSepaSTR <- tmpList[["SEPASTR"]]
  #
  if (class(IdKb) == "character") {
    tmpKB  <-  IdKb
    tmpID  <- .self$IotoK(IdKb, .self$GruaSel.chr)
    tmpSPT <- .self$IKtoS(IdKb, .self$GruaSel.chr)
  } else if (class(IdKb) %in% c("integer","numeric")) {
    tmpID  <-  IdKb
    tmpKB  <- .self$IotoK(IdKb, .self$GruaSel.chr)
    tmpSPT <- .self$IKtoS(IdKb, .self$GruaSel.chr)
  } else {
    warning("ElementeId muss vom Typ 'character', 'numeric' oder 'integer' sein. ~ 'NULL' zurueckgegeben!")
    return(NULL)
  }
  #
  # Falls ID|KB in Elementekatalog nicht auftaucht, ist entspr. KB|ID = NA
  # 
  if (asel_helper_check_valid_sepa(tmpID, tmpKB, tmpSepaSTR, tmpSPT, tmpINV)) {
    #
      tmpIDs <- .self$ItoP(tmpID, HID = FALSE, grua)
    tmpObAsp <- asp.rc$new(iID.lst  = tmpIDs,
                           iSPT     = tmpSPT,
                           iPrtSTR  = tmpPrtSTR,
                           iSepaSTR = tmpSepaSTR,
                           iInvert  = tmpINV)
    #
    if(!is.null(tmpObAsp)) {
      if(tmpObAsp$nTeilKrits.int > 0 | tmpObAsp$MetaKrit.log) {
        if (pInd == 0) {
          .self$nObAsp.int <- as.integer(.self$nObAsp.int + 1)
          .self$ObAsp.lst[[.self$nObAsp.int]] <- tmpObAsp
          message(paste0("Index des neuen Selektionsparameters:", .self$nObAsp.int))
          return(.self$nObAsp.int)
        } else if (.self$IsInRange(pInd)) {
          .self$ObAsp.lst[[pInd]] <- tmpObAsp
          message(paste0("Index des neuen Selektionsparameters:", pInd))
          return(pInd)
        }
      }
    }
  }
}