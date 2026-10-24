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
# Helper:
asel_check_Sela_File_Path = function(.self, SelPath = "") {
  if (SelPath == "") {
    #aktuellen Pfad abspeichern:
    tmpPath <- getwd()
    #Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
    setwd(m_PathSelektAns)
    #Benutzerauswahl Selektions-csv:
    message("")
    message("Bitte Selektionsansatz via Open-Dialog auswaehlen.")
    SelPath <- file.choose()
    #Pfad zurücksetzen
    setwd(tmpPath)
    return(SelPath)
  } else if (file.exists(SelPath)) {
    message("Lade Selektionsansatz via uebergebenem Dateifpad.")
    return(SelPath)
  } else {
    warning(paste0("Die angebgebene Datei existiert nicht: ", SelPath, " Keine Daten geladen."))
    return(NULL)
  }
}
#
asel_load_sela = function(.self, SelPath = "", MERGE = TRUE) {
  ## Bereinigen falls gewuenscht:
  if (!MERGE) {
    suppressMessages(.self$clear())
  }
  ## Pfad zum Sela checken:
  .self$ImpSelPath.chr <- .self$checkSelaFilePath(SelPath)
  if (is.null(.self$ImpSelPath.chr)) {
    return(FALSE)
  }
  ## Den Dateinamen extrahieren:
  tmpV <- strsplit(.self$ImpSelPath.chr, split="\\\\")[[1]]
  .self$ImpSelName.chr <- tmpV[length(tmpV)]
  ## und hieraus die Grundstücksart extrahieren:
  # %-Teil extrahieren:
  tmpMetaToken <- .self$ImpSelName.chr
       ProzPos <- gregexpr("%", .self$ImpSelName.chr)[[1]]
  # Gib es ein '%' ~ wenn ja, dann "tmpMetaToken" beschneiden: 
  if (ProzPos[1] > 0) {
    tmpMetaToken <- substr(tmpMetaToken, start = ProzPos + 1, stop = nchar(tmpMetaToken))
  }
  # GRUA extrahieren: 
  .self$GruaSel.chr <- NA_character_
  for (grua in GALABS) {
    if (grepl(grua, tmpMetaToken)) {
      .self$GruaSel.chr <- grua
      break
    }
  }
  if(is.na(.self$GruaSel.chr)) {
    warning("Keine GRUA festgelegt: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' 
             Der Dateiname des Selektionsansatzes enthielt keine Informationen.
             Benutzte 'asel.rc$setGrua', um die GRUA manuell festzulegen.")
  } else {
    message(paste0("GRUA festgelegt: '", .self$GruaSel.chr,"'")) 
  }
  # Schauen, ob eine Selektionstabelle geladen werden soll:
  if (grepl("STAB", tmpMetaToken)) {
    .self$loadStab(.self$ImpSelPath.chr, MERGE = TRUE)
  } else {
    .self$ImpSel.df <- read.table(.self$ImpSelPath.chr,
                                    colClasses = c("integer", "character", "character","character"),
                                  comment.char = "",
                                        header = TRUE,
                                    na.strings = "",
                                           dec = ",",  # prinzipiell obsolet 
                                           sep = ";",
                                          fill = TRUE,
                                  fileEncoding = AKS_FILE_ENCODING)
    # Wenn keine Kriterien, dann abbrechen:
    nrows <- nrow(.self$ImpSel.df)
    if (nrows <= 2) {
      warning("Selektionsansatz enthaelt keine Kriterien!")
      return(FALSE)
    }
    # Schauen, ob die Sepas in AKS/AICE-Syntx vorliegen:
    if (.self$ImpSel.df$Elementname[nrows] == "AICE") {
      blnAKS <- FALSE
    } else {
      blnAKS <- TRUE
    }
    nrows <- nrows - 2  # die letzten beiden sind Anz Krit, Anz KF, bzw. "dummy", "AICE"
    # Selektionsansatz in IDs aufsplitten:
    .self$nImpSS.int <- as.integer(0)
    cline <- 1
    while (cline <= nrows) {
      tmpStart <- cline
      curID <- .self$ImpSel.df[tmpStart,1]
      #
      if(cline < nrows) {
        while (.self$ImpSel.df[cline + 1,1] == curID) {
          cline <- cline + 1 
          if (is.na(.self$ImpSel.df[cline + 1,1])) {
            break
          }
        }
      }
      tmpEnd <- cline
      #
      .self$nImpSS.int <- as.integer(.self$nImpSS.int + 1)
      .self$ImpSelSplitList.df[[.self$nImpSS.int]] <- .self$ImpSel.df[tmpStart:tmpEnd,]
      # KurzBez - Spalte hinzufuegen und fuellen:
      .self$ImpSelSplitList.df[[.self$nImpSS.int]]$Kurzbezeichnung <- .self$IotoK(curID, .self$GruaSel.chr) 
      # SP-Typenspalte hinzufügen und fuellen:
      .self$ImpSelSplitList.df[[.self$nImpSS.int]]$SPT <- .self$IKtoS(curID, .self$GruaSel.chr)
      #
      cline <- cline + 1
    }
    # asp-Objekte aus den Split-Tabellen erstellen:
    for (i in 1:.self$nImpSS.int) {
      .self$addSepa(    IdKb = .self$ImpSelSplitList.df[[i]]$Nummer[1],
                    InputStr = .self$ImpSelSplitList.df[[i]]$Selektionsansatz[1],
                        pInd = 0,
                         AKS = blnAKS)
    }
  }
}
