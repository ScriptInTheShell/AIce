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
#-------------------------------------------------------------------------------
aice_load_data <- function(.self, pathKaufDat = "", NoGhosts = TRUE, TypeRepair = TRUE, NoUnnamed = FALSE) {
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
                                            dec = ",",   # obsolet wegen 'colClasses = "character"'
                                            sep = ";",
                                           fill = TRUE,
                                   fileEncoding = AKS_FILE_ENCODING)  # (*)
  # (*):  Die Daten werden von der AKS üblicherweise NICHT in "UTF-8" exportiert, sondern in ANSI:
  #                       https://de.wikipedia.org/wiki/ANSI-Zeichencode
  # Hinweis: Die Dezimaltrenner ',' werden bei addSepa(), den Selektionen, sowie dem Picken (typecast) durch '.' ersetzt. 
  }
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### CLEANUP ~ Geisterelemente:
  if (NoGhosts) {
   ## Geisterelemente bereinigen:
    # Die AKS haelt wohl historische Elementefelder wie "WGWB" (bzw. "WGWB1",...,"WGWB5"), die nicht mehr befuellt
    # werden koennen, aber beim Export in der CSV auftauchen ~ und dann einen "Rechtsshift" des Datensatzes 
    # verursachen.
    KFdata <- aice_cast_out(KFdata)
  }
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### CLEANUP ~ Unbenannte Leerspalten:
  if (NoUnnamed) {
    # Die AKS exportiert (zumindest momentan) mit einem zusätzlichen ";"-CSV-Trenner am Ende jeder Zeile. 
    # read.table interpretiert das als unbenannte Spalte, und gibt ihr den Namen "X".
    # Weitere solche Spalten werden (zumindest in meiner R Version) durchnummeriert benannt:  "X.1", "X.2" ...
    # Wir testen auf eine derartige Spalten und entfernen diese:
    KFdata <- aice_rmv_unnamed(KFdata)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### CLEANUP-Typenuntreue:
  if (TypeRepair) {
    ## typenuntreu exportierte Elemente casten:
    KFdata_uf_split <- aice_type_repair(KFdata_uf_split)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#
#-------------------------------------------------------------------------------
# 
aice_cast_out = function(KFdata.lst) {
  for (grua in GLABS) {
    tmpList <- read.table(paste0(m_PathMetaDat,"/DataCleanups/Geister_",grua,".csv"),
                          colClasses = "character",
                          header=TRUE,
                          dec=",",# obsolet
                          sep=";",
                          fill=TRUE,
                          fileEncoding = STD_FILE_ENCODING)
    #
    if (is.null(tmpList)) {
      warning("aice_type_repair: Geisterspalten-Ladefehler ~ Keine Aktion durchgefuehrt.")
    } else if (nrow(tmpList) > 0) {
      tmpGhosts <- tmpList$Ghosts
      ColNms <- colnames(KFdata.lst[[grua]])
       nCols <- length(ColNms) 
       vInds <- na.omit(match(tmpGhosts, ColNms))
        nAnz <- length(vInds)
      if (nAnz > 0) {
        NewColNms <- c(ColNms[-vInds],rep("DummyColName", nAnz))
        colnames(KFdata.lst[[grua]]) <- NewColNms
        vtrashInds <- (nCols - nAnz + 1):nCols
        KFdata.lst[[grua]] <- subset(KFdata.lst[[grua]], select = -vtrashInds)
      }
    }
  }
  return(KFdata.lst)
}
#
#-------------------------------------------------------------------------------
# 
aice_rmv_unnamed = function(KFdata.lst) {
  for (grua in GLABS) {
    ColNms <- colnames(KFdata.lst[[grua]])
    if ("X" %in% ColNms) {
      vIndices <- c(match("X", ColNms), which(grepl("X\\.[0-9]*$", ColNms)))
      KFdata.lst[[grua]] <- subset(KFdata.lst[[grua]], select = -vIndices)
    }
  }
  return(KFdata.lst)
}
#
#-------------------------------------------------------------------------------
# 
aice_type_repair = function(KFdata.lst) {
  for (grua in GSLABS) {
    tmpList <- read.table(paste0(m_PathMetaDat,"/DataCleanups/TypeRepair_",grua,".csv"),
                          colClasses = "character",
                          header=TRUE,
                          dec=",",# obsolet
                          sep=";",
                          fill=TRUE,
                          fileEncoding = STD_FILE_ENCODING)
    #
    if (is.null(tmpList)) {
      warning("aice_type_repair: Reparaturdaten-Ladefehler ~ Keine Aktion durchgefuehrt.")
    } else if (nrow(tmpList) > 0) {
      #
      vKBs <- tmpList$Kurzbezeichnung
      vTCs <- tmpList$TypecastAktion
      for (index in 1:nrow(tmpList)) {
        # Notationsentlastung:
        KB <- vKBs[index]
        TC <- vTCs[index]
        #
        if (KB %in% colnames(KFdata.lst[[grua]])) {
          KFdata.lst[[grua]][[KB]] <- switch(TC,
                                             "DEZtoINT" = aice_type_repair_DEZtoINT(KFdata.lst[[grua]][[KB]])
                                            )
        }
      }
    }
  }
  return(KFdata.lst)
}
#
# aice_type_repair() - HELPERS:
aice_type_repair_DEZtoINT = function(vec = c()) {
  indices <- which(!is.na(vec))
  vec[indices] <- paste(as.integer(gsub(",",".", vec[indices])))
  return(vec) 
}

