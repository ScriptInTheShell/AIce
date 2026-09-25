###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
# aice
#
aice_load_data <- function(.self, pathKaufDat = "") {
  # Ordner auf Existenz pruefen:
  if (!dir.exists(pathKaufDat)) {
    warning(paste0("Der angebgebene Ordner existiert nicht: ", pathKaufDat, " Keine Daten geladen."))
    return(FALSE) 
  } 
  # Variablendeklarationen (zunächst leer):
  #
  # Liste mit den Kauffalldaten (bb,uf,ei) ~ zunaechst leer:
            KFdata <- list("bb"=NULL,"ei"=NULL,"uf"=NULL)
   KFdata_uf_split <- list("bb"=NULL,"ei"=NULL,"uf"=NULL,"ub"=NULL,"lf"=NULL,"gf"=NULL,"sf"=NULL)
  ElemIDs_uf_split <- list("bb"=NULL,"ei"=NULL,"ub"=NULL,"lf"=NULL,"gf"=NULL,"sf"=NULL)
  #
  #-------------------------------------------
  # Die exportierten AKS-Datensaetze laden:  |
  #------------------------------------------
  # Welche Daten-Dateien gibt es?
  filenames <- dir(pathKaufDat)
  # Lade die Daten
  for (grua in GLABS) {
    KFdata[[grua]] <- read.table(paste0(pathKaufDat,"/",filenames[grep(grua, filenames)]),
                                     colClasses = "character",
                                         header = TRUE,
                                     na.strings = "",
                                            dec = ",",
                                            sep = ";",
                                           fill = TRUE,
                                   fileEncoding = AKS_FILE_ENCODING)  # (*)
  # (*):  Die Daten werden von der AKS üblicherweise NICHT in "UTF-8" exportiert, sondern in ANSI:
  #                       https://de.wikipedia.org/wiki/ANSI-Zeichencode
    # Die Ersetzung ',' -> '.' muss noch manuell durchgefuehrt werden, denn wegen "colClasses = "character"", 
    # wird " dec = "," ignoriert.
    for (kb in colnames(KFdata[[grua]])) {
      pos <- which(!is.na(KFdata[[grua]][kb]))
      KFdata[[grua]][pos,kb] <- gsub(",", ".",  KFdata[[grua]][pos,kb])
    }
  }
  #
  #---------------------------------------
  # DATENSPLIT:     uf -> ub,lf,gf,sf :  |
  #--------------------------------------
  #
  # "bb"
  KFdata_uf_split[["bb"]] <- KFdata[["bb"]]
  # "ei"
  KFdata_uf_split[["ei"]] <- KFdata[["ei"]]
  # "uf"
  GRZ <- list("ub"=c("MIN" = 100,"MAX" = 199),
              "gf"=c("MIN" = 200,"MAX" = 299),
              "lf"=c("MIN" = 300,"MAX" = 399),
              "sf"=c("MIN" = 400,"MAX" = 499))
  #
  for (grua in GUFLABS) {
    # Die Daten müssen mit Hilfe der Spalte "GRUA" aufgeteilt werden:
    KFdata_uf_split[[grua]] <- KFdata[["uf"]][which(KFdata[["uf"]]$GRUA >= GRZ[[grua]]["MIN"] & 
                                                       KFdata[["uf"]]$GRUA <= GRZ[[grua]]["MAX"]),]
  }
  #
  #---------------------------------------------------------------------------------------------------------------------------
  # Via separater Liste:  Den Kurzbezeichnungen in den Spalten die entspr. Nummer (Indentifier) zuordnen (Elementekatalog):  |
  #--------------------------------------------------------------------------------------------------------------------------
  for (grua in GSLABS) {
    nAnzBez <- ncol(KFdata_uf_split[[grua]])
    tmpEIDs <- rep(0,times = nAnzBez)
    for (cInd in 1:nAnzBez) {
                                  colname <- colnames(KFdata_uf_split[[grua]])[cInd]
                            tmpEIDs[cInd] <- .self$IotoK(colname,grua)
                ElemIDs_uf_split[[grua]] <- as.data.frame(t(tmpEIDs))
      colnames(ElemIDs_uf_split[[grua]]) <- colnames(KFdata_uf_split[[grua]])
    }
  }
  # "RUECKGABE: 
  # - nach GRUA ~ GSLABS <- c("bb","ei","ub","lf","gf","sf") gesplittete Kauffalldaten
  # - nach GRUA ~ GSLABS gesplittete Elementekataloge - Reihenfolge jeweils wie die Spalten in den KF-Daten
  #
  .self$KFdataList.df  <- KFdata_uf_split
  .self$ElemIdList.df  <- ElemIDs_uf_split
  .self$DataLoaded.log <- TRUE
  # Trivialen Filter setzen
  .self$rmvFltr(grua = NULL)
  return(TRUE)
}

