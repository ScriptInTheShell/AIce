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
asel_load_sela = function(.self, SelPath = "") { 
  ##Pfad zum Sela abprüfen:
  if (SelPath == "") {
    #aktuellen Pfad abspeichern:
    tmpPath <- getwd()
    #Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
    setwd(m_PathSelektAns)
    #Benutzerauswahl Selektions-csv:
    .self$ImpSelPath.chr <- file.choose()
    #Pfad zurücksetzen
    setwd(tmpPath)
  } else if (file.exists(SelPath)) {
    message("Lade Selektionsansatz via uebergebenem Dateifpad.")
    .self$ImpSelPath.chr <- SelPath
  } else {
    warning(paste0("Die angebgebene Datei existiert nicht: ", SelPath, " Keine Daten geladen."))
  }
  ## Den Dateinamen extrahieren:
  tmpV <- strsplit(.self$ImpSelPath.chr, split="\\\\")[[1]]
  .self$ImpSelName <- tmpV[length(tmpV)]
  # und hieraus die Grundstücksart extrahieren:
  .self$GruaSel.chr <- ""
  for (grua in GALABS) {
    if(grepl(grua, .self$ImpSelName)) {
      .self$GruaSel.chr <- grua
    }
  }
  if(.self$GruaSel.chr == "") {
    warning("Keine GRUA festgelegt: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf'\n 
             Der Dateiname des Selektionsansatzes enthielt keine Informationen.\n
             Benutzte asel.rc$setGrua, um die GRUA manuell festzulegen.")
  }
  .self$ImpSel.df <- read.table(.self$ImpSelPath.chr,
                                  colClasses = c("integer", "character", "character","character"),
                                comment.char = "",
                                      header = TRUE,
                                  na.strings = "",
                                         dec = ",",
                                         sep = ";",
                                        fill = TRUE,
                                fileEncoding = AKS_FILE_ENCODING)
  # Selektionsansatz in IDs aufsplitten:
  nrows <- nrow(.self$ImpSel.df) - 2  # die letzten beiden sind Anz Krit, Anz KF
  if (nrows == 0) stop("Selektionsansatz enthaelt keine Kriterien!")
  .self$nImpSS.int <- 0
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
    tmpAnz <- tmpEnd - tmpStart + 1
    #
    .self$nImpSS.int <- .self$nImpSS.int + 1
    .self$ImpSelSplitList.df[[.self$nImpSS.int]] <- .self$ImpSel.df[tmpStart:tmpEnd,]
    # KurzBez - Spalte hinzufuegen und fuellen:
    .self$ImpSelSplitList.df[[.self$nImpSS.int]]$Kurzbezeichnung <- rep(.self$IotoK(curID,.self$GruaSel.chr), tmpAnz) 
    # SP-Typenspalte hinzufügen und fuellen:
    .self$ImpSelSplitList.df[[.self$nImpSS.int]]$SPT <- rep(.self$IKtoS(curID,.self$GruaSel.chr), tmpAnz) 
    #
    cline <- cline + 1
  }
  #
  # asp-Objekte aus den Split-Tabellen erstellen:
  nAnzValidKrit <- 0
  for (i in 1:.self$nImpSS.int) {
    .self$addSepa(.self$ImpSelSplitList.df[[i]]$Nummer[1],
                  .self$ImpSelSplitList.df[[i]]$Selektionsansatz[1],
                  0)
  }
  return(TRUE)
}
