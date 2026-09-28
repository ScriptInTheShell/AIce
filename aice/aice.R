###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#----------------
# AKS-INTERFACE |
#---------------
# Version:      1.0.0-beta
# Author:       Dr. Maximilian Hanusch
# Maintainer:   Dr. Maximilian Hanusch  ~  "maximilian.hanusch@lkprignitz.de"
#
###########################################################################################################################
#
source("aice/meta.R")
source("aice/functions/com_func/func_com_meta.R")
source("aice/functions/aice_func/func_aice_meta.R")
source("aice/asel.R")
#
###########################################################################################################################
#--------------------------------------------------------------------------------------------------------------------------
#  AKS-INTERFACE.REFERENCE-CLASS  |
#----------------------------------
#
aice.rc <- setRefClass(
 # Name der RC-Klasse
  "AKS_Interface",
 #------------------------------------ 
 #  DATENFELDER  |
 #---------------
  fields = c(
  ##  
  ## KAUFFALLDATEN:  
  # Kauffalldaten-Liste ~ data.frame - indiziert durch Labels: "bb","ei","ub","lf","gf","sf"
    KFdataList.df  = "list",
  # Vorselektierte (gefilterte) KFdataList.df - vgl. setFilter, rmvFilter
    FltrKFdataList.df  = "list",
  # Flag - Daten geladen
    DataLoaded.log = "logical",
  ##
  ## ELEMENTEKATALOGDATEN:
  # Elementekatalog-Liste ~ data.frame - indiziert durch Labels: "bb","ei","ub","lf","gf","sf"
  #     colnames = Kurzbezeichnungen, Zeile_1 = Identifier - jeweils in der Reihenfolge wie im Urdatensatz  
    ElemIdList.df  = "list", 
  # Hauptkatalog - enthaelt die GRUA-unspezifischen Eintraege aus "HauptKTG.csv"
    ElHptKTG.df    = "data.frame",
  # Sonderkatalog-Liste ~ data,frame - indiziert durch Labels: "bb","ei","ub","lf","gf","sf"
  # MOMENTAN LEER:  enthaelt die GRUA-spezifischen Eintraege 
  #                 ~ ausgelagert aus "Elementekatalog.csv" in "SonderKTG_<grua>.csv"
    ElSndKtgList.df = "list",
  ##
  ## SELEKTIONSDATEN
  # Liste mit den bereits getaetigten Selektionen - jeder Listeneintrag von Form: 
  #     list(GRUA, MASK, NAs) 
  #          GRUA = Selektierte GRUA, 
  #          MASK = logischer Vektor (TRUE = positiv selektiert), 
  #          NAs  = data.frame - Kauffaelle mit mind. einem NA in einem der Nicht-Meta-Kriterien
  #                              diese sind dann insbes. NICHT-selektiert
    SelectionList.df  = "list",
  # Anzahl der bereits getaetigten Selektionen:
    AnzSelections.int = "integer"
  ),
 #------------------------------------ 
 #  METHODEN:  |
 #-------------
  methods = list(
 #
 #ELEMENTE-KATALOG-LOADER:  wegen Umfang ausgelagert in "aice_func_load_data.R"
    loadKtgs = com_load_ktgs,
 #DATA-LOADER:  wegen Umfang ausgelagert in "aice_func_load_data.R"
    loadData = aice_load_data,
 #
 #KONSTRUKTOR:
    # laedt die Kauffalldaten falls gewuenscht
    # laedt alle ELEMENTKATALOGDATEN
    #
    initialize = function(loadData = TRUE) {
      # Variablen initialisieren:
      .self$KFdataList.df     <- list()
      .self$FltrKFdataList.df <- list()
      .self$DataLoaded.log    <- FALSE
      .self$ElemIdList.df     <- list() 
      .self$ElHptKTG.df       <- data.frame()
      .self$ElSndKtgList.df   <- list()
      #
      .self$SelectionList.df  <- list()
      .self$AnzSelections.int <- as.integer(0)
      #
      # Elemenete-Kataloge laden
      .self$loadKtgs(m_PathMetaDat)
      # Kaufall-Daten laden, falls gewuescht:
      if (loadData) {
        # Daten Laden:
        .self$loadData(m_PathKaufDat)
      }
    },
 #
 #DESTRUKTOR:
    finalize = function()  {
        #print("Aufruf des aice.rc Destruktors.")
    },
 #ELEMENTARE OPERATIONEN:
  # selectMask - selektiert in FltrKFdataList.df via Mask
  # die Selektion wird NICHT intern abgespeichert
    selectMask = function(grua = "", mask = NULL, TPCST = FALSE) {
      if (grua == "") {
        warning("Keine GRUA uebergeben, in der selektiert werden soll ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (is.null(mask)) {
        warning("Keine Selektionsmaske uebergeben, die selektiert werden kann ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      message(paste0(toString(length(which(mask))), " Kauffaelle selektiert."))
      retData <- .self$FltrKFdataList.df[[grua]][which(mask),]
      if (TPCST) {
        retData <- .self$typeCast(retData, grua)
      }
      return(retData)
    },  
  # directSelectMask - selektiert in data via Mask  
    selectMaskDirect = function(data = NULL, mask = NULL, TPCST = FALSE) {
      if (is.null(data)) {
        warning("Keine Daten uebergeben, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (is.null(mask)) {
        warning("Keine Selektionsmaske uebergeben, anhand derer selektiert werden kann ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      message(paste0(toString(length(which(mask))), " Kauffaelle selektiert."))
      retData <- data[which(mask),]
      if (TPCST) {
        retData <- .self$typeCast(retData, grua)
      }
      return(retData)
    },
  # directSelect - wie select, selektiert aber aus "data" und nicht aus ".self$FltrKFdataList.df"
  # - die GRUA im asel-Objekt bleibt unberuecksichtigt
  # - die Selektion wird NICHT intern abgespeichert
    selectDirect = function(data = NULL, aselObjPth = NULL, INVERT = FALSE, META = FALSE, TPCST = FALSE) {
      ## asel - Pruefung wie in .self$select()
      if (is.null(data)) {
        warning("Keine Daten uebergeben, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (is.null(aselObjPth) | class(aselObjPth) == "character") {
        message("Kein AKS-Selektionsobjekt (asel.rc)-Objekt uebergeben.")
        if (is.null(aselObjPth)) {
          tmpPath <- getwd()
          # Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
          setwd(m_PathSelektAns)
          # Benutzerauswahl Selektions-csv:
          filePath <- file.choose()
          # Pfad zuruecksetzen:
          setwd(tmpPath)
        } else if (class(aselObjPth) == "character") {
          message("Lade Selektionsansatz via uebergebenem Dateifpad.")
          filePath <- aselObjPth   
        }
        aselOb <- asel.rc$new(filePath)
      } else if (class(aselObjPth) == "AKS_Selektionsansatz") {
        message("AKS-Selektionsobjekt asel.rc-Objekt uebergeben.")
        aselOb <- aselObjPth
      }
      ## Daten selektieren:
      # Inversionsflag setzen:
      aselOb$setInv(INVERT)
      # selektieren:
      retList <- aselOb$select(data)
      # ausgeben:
      if (META) {
        return(retList)
      } else {
        tmpMask  <- retList$blnSMASK
        tmpRows  <- which(tmpMask)
        tmpAnzRw <- length(which(tmpMask))
        #
        if (length(tmpRows) > 0){
          message(paste0(toString(tmpAnzRw), " Kauffaelle selektiert."))
          retData <- data[tmpRows,]
          if (TPCST) {
            retData <- .self$typeCast(retData, grua)
          }
          return(retData)
        } else {
          message("0 Kauffaelle selektiert ~ 'NULL' zurueckgegeben!")
          return(NULL)
        }
      }
    },
  # select - Selektiert die Daten via uebergebenem asel.rc-Objekt
  # aselObjPth:
  #    asel.rc-Obj. es wird via dem uebergebenen asel selektiert 
  #    NULL / ""    es wird neues asel erzeugt und via Open-Dialog geladen (in asel_load_sela)
  #    "<Pfad>"     es wird neues asel erzeugt und der Selektionsansatz unter "<Pfad>" geladen (in asel_load_sela)  
    select = function(aselObjPth = NULL, INVERT = FALSE, TPCST = TRUE) {
      if (!.self$DataLoaded.log) {
        warning("Keine Daten geladen, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (class(aselObjPth) == "AKS_Selektionsansatz") {
        message("AKS-Selektionsobjekt asel.rc-Objekt uebergeben.")
        aselOb <- aselObjPth
      } else if (is.null(aselObjPth) | class(aselObjPth) == "character") {
        message("Kein AKS-Selektionsobjekt (asel.rc)-Objekt uebergeben.")
        if (is.null(aselObjPth)) {
          aselObjPth <- ""
        }
        aselOb <- asel.rc$new(aselObjPth)
        # Inversionsflag setzen:
        aselOb$setInv(INVERT)
      }
      #
      tmpGrua <- aselOb$getGrua()
      if (is.null(tmpGrua)) {
        warning("Fehlende GRUA ~ keine Selektion durchgefuehrt ~ 'NULL' zurueckgegeben.")
        return(NULL) 
      }
      # selektieren:
         retList <- aselOb$select(.self$FltrKFdataList.df[[tmpGrua]])
      tmpSelMask <- retList$blnSMASK
          tmpNAs <- retList$NAS
      # Intern abspeichern:
                                .self$AnzSelections.int <- as.integer(.self$AnzSelections.int + 1)
      .self$SelectionList.df[[.self$AnzSelections.int]] <- list(GRUA =  tmpGrua, MASK = tmpSelMask, NAs =tmpNAs)
      # Selektion zurueckgeben:
      return(.self$getSelda(Index = 0, META = FALSE, TPCST = TPCST))
    },
 #GETTER-METHODEN:
  # get-Selektions-Maske- Gibt die boolsche Selektionliste zurueck, die sich auf den Index-Eintrag der FltrKFdataList.df - Liste bezieht
    getSelma = function(Index = 0) {
      retList <- getSelda(Index, META = TRUE)
      if (!is.null(retList)) {
        return(retList$MASK)
      } else {
        return(NULL)
      }
    },
  # getAnzSel - gibt die Anzahl der abgespeicherten Selektionsdaten ab:
    getAnzSel = function() {
      message(paste0("Anzahl der abgespeicherten Selektionen: ", toString(.self$AnzSelections.int)))
      return(.self$AnzSelections.int)
    },
  # getSelda - gibt die Index-te Selektion zurueck, bzw. die letzte wenn Index = 0 (oder kein Parameter) uebergeben 
    getSelda = function(Index = 0, META = FALSE, TPCST = FALSE) {
      if (.self$AnzSelections.int == 0) {
        warning("Keine Selektionen gespeichert ~ 'NULL'zurueckgegeben.")
        return(NULL)
      }
      if (Index == 0) { 
        Index <- .self$AnzSelections.int
      }
      if (!(1 <= Index && Index <= .self$AnzSelections.int)){
        warning(paste0("Fehlerhafter Index ~ Wert muss zwischen 0 und ", toString(.self$AnzSelections.int)," liegen!"))
        return(NULL)
      }
      if (META) {
        return(.self$SelectionList.df[[Index]])
      } else {
        tmpGrua <- .self$SelectionList.df[[Index]]$GRUA
        tmpRows <- which(.self$SelectionList.df[[Index]]$MASK)
        #
        if (length(tmpRows) > 0) {
          message(paste0(toString(length(tmpRows)), " Kauffaelle selektiert."))
          retData <- .self$FltrKFdataList.df[[tmpGrua]][tmpRows,] 
          if (TPCST) {
            retData <- .self$typeCast(retData, tmpGrua)
          }
          return(retData)
        } else {
          message("0 Kauffaelle selektiert ~ 'NULL' zurueckgegeben!")
          return(NULL)
        }
      }
    },
  # prtSelda - gibt die letzte Selektion aus, falls '0' oder kein Parameter uebergeben; ansonsten entsprechend des Index:
    prtSelda = function(Index = 0, META = FALSE) {
      if (.self$AnzSelections.int == 0) {
        warning("Keine Selektionen gespeichert ~ 'NULL'zurueckgegeben.")
        return(NULL)
      }
      if (Index == 0) { 
        Index <- .self$AnzSelections.int
      } else if (!(1 <= Index && Index <= .self$AnzSelections.int)) {
        warning(paste0("Fehlerhafter Index ~ Wert muss zwischen 0 und ", toString(.self$AnzSelections.int)," liegen!"))
        return(NULL)
      }
      tmpGrua <- .self$SelectionList.df[[Index]]$GRUA
      tmpRows <- which(.self$SelectionList.df[[Index]]$MASK)
      tmpNAs <- .self$SelectionList.df[[Index]]$NAs
      #
      message(paste0(toString(length(tmpRows)), " Kauffaelle selektiert."))
      #
      if (META) {
        print(paste0("Selektierte  Grundstuecksart: ", tmpGrua))
        print("---------------------------------------------------------------------------------------------------------------")
        print("Selektierte Zeilen:")
        print(tmpRows)
        print("---------------------------------------------------------------------------------------------------------------")
        print("Selektierte Kauffaelle:")
        print(.self$KFdataList.df[[tmpGrua]][tmpRows,]$KFKZ)
        print("---------------------------------------------------------------------------------------------------------------")
        print("NAs:")
        print(tmpNAs)
      } else {
        print(.self$KFdataList.df[[tmpGrua]][tmpRows,])
      }
    },
  # getKauffalldaten nach GRUA:
    getData  = function(grua = "", FILTER = TRUE, TPCST = FALSE) { 
      if (!.self$DataLoaded.log) {
        warning("getData: Keine Daten uebergeben ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      if (FILTER) {
        retData <- .self$FltrKFdataList.df[[grua]]
      } else {
        retData <- .self$KFdataList.df[[grua]]
      }
      if (TPCST) { retData <- .self$typeCast(retData, grua) }
      return(retData)
    },
  # getColInds - gibt die Spaltenpositionen im Datensatz der uebergebenen (entweder nur) KBs / (oder nur) IDs wieder:
    getColInds = function(vIKs = c(), grua = "") {
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Keine gueltige GRUA uebergeben. -> 'NULL' zurueckgegeben!")
        return(NULL)
      }
      #
      stat <- .self$chItoK(vIKs)
      if (is.null(stat)) { 
        warning("getColInds: Kein Identifier uebergeben. -> 'NULL' zurueckgegeben!")
        return(NULL)
      } else if (stat) { 
        # Falls IDs, dann in KBs umrechnen:
        vIKs <- .self$IotoK(vIKs,grua)
      }
      #
      return(match(vIKs,colnames(.self$KFdataList.df[[grua]])))
    },
  # getElementeIDs nach GRUA (nur die, die auch im DS auftreten) bzw.im Hauptkatalog, falls Nichts angegeben:
    getElids = function(grua = "") {
      if (!meta_check_grua(grua, TYPE = GSLABS))  {
        warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      #
      return(.self$ElemIdList.df[[grua]])
    }, 
  # getHauptkatalog:
    getHptKat = function() {
      return(.self$ElHptKTG.df)
    },  
  # getSonderkatalog:
    getSndKat = function(grua = "", SILENT = TRUE) {
      if (!meta_check_grua(grua, TYPE = GSLABS))  {
        if (!SILENT) {
          warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
        }
        return(NULL)
      }
      return(.self$ElSndKtgList.df[[grua]])
    },
 #SETTER-METHODEN:
  # setKauffalldaten nach GRUA:       CHTPC ~ char-typecast 
    setData  = function(data = NULL, grua = "", FILTER = TRUE, CHTPCST = TRUE) { 
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Ungueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ Keine aKtion durchgefuehrt!")
      }
      if (CHTPCST) {
       data <- as.char(data) 
      }
      if (FILTER) {
        .self$FltrKFdataList.df[[grua]] <- data
      } else {
        .self$KFdataList.df[[grua]] <- data
      }
    },
  # set Filter - die entsprechenden Kauffalldaten werden vorselektiert:  
    setFltr = function(aselOb = NULL) {
      #
      if (is.null(aselOb)) {
        message("Kein AKS-Selektionsobjekt (asel.rc)-Objekt uebergeben.")
        tmpPath <- getwd()
        # Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
        setwd(m_PathSelektAns)
        # Benutzerauswahl Selektions-CSV:
        filePath <- file.choose()
        #Pfad zuruecksetzen
        setwd(tmpPath)
        #Asel-Objekt laden
        aselOb <- asel.rc$new(filePath, TRUE)
      }
      if (aselOb$checkGrua()) {
        .self$FltrKFdataList.df[[aselOb$getGrua()]] <- .self$select(aselOb, INVERT = FALSE, TPCST = FALSE)
      } else {
        message("Keine GRUA uebergeben - Filter fur alle GRUAs festgelegt.")
        for (grua in GSLABS) {
          aselOb$setGrua(grua)
          .self$FltrKFdataList.df[[grua]] <- .self$select(aselOb, INVERT = FALSE, TPCST = FALSE)
        }
      }
    },
  # remove Filter - entfernt die gesetzten Filter:
  #                 grua = "" :        alle Filter
  #                 grua = <grua>:     den <grua>-Filter
    rmvFltr = function(grua = NULL) {
      if (is.null(grua)) {
        .self$FltrKFdataList.df <- .self$KFdataList.df
        message("keine Filter gesetzt (alle Filter entfernt)")
      } else if (meta_check_grua(grua, TYPE = GSLABS)) {
        .self$FltrKFdataList.df[[grua]] <- .self$KFdataList.df[[grua]]
        message(paste0(grua, " - Filter entfernt."))
      } else {
        warning("Ungueltige Eingabe: Keine Aktion durchgefuehrt!")
      }
    },
 #HELPER-METHODEN:
  # chItoK - # check ID / KB - prueft ob "vecIKs" richtig belegt; und ob ID in KB umgerechnet werden muss: 
    chItoK = com_chItoK, 
  # Spaltentausch Data.Frame
      dfm = com_helper_df_mutate,
  # IotoK- "Umrechnung:"   ElemneteId  <->  Kurzbezeicnung
    IotoK = com_IotoK,
  # IKtoS - gibt den SP(selektionsparameter)-Typen zurueck, der zur uebergebenen ID bzw. Kurzbez. gehört:
    IKtoS = com_IKtoS,
  # typeCast - konvertiert die Spalten vom uebergebenen data.frame in den vis SPT (ELmenetekatalog) festgelegten Datentyp
  # Beachte:  Die Kauffallsaten werden von AICE per default als Strings geladen 
    typeCast = com_typeCast,
  # pickFltr - Extrahiert die zu vIKs (IDs oder KBs) gehoerigen Spalten aus "FltrKFdataList.df[[grua]]"  
    pickFltr = function(vIKs = c(), grua = "", TPCST = TRUE) {
      .self$pick(.self$FltrKFdataList.df[[grua]], vIKs, grua, TPCST)
    },
  # 
    pickSelda = function(Index = 0, vIKs = c(), TPCST = TRUE) {
      dfrm <- .self$getSelda(Index = Index, META = FALSE, TPCST = FALSE)
      meta <- .self$getSelda(Index = Index, META = TRUE, TPCST = FALSE)
      return(.self$pick(dfrm, vIKs, meta$GRUA, TPCST))
    },
  # pick     Extrahiert die zu vIKs (IDs oder KBs) gehoerigen Spalten aus "dfrm" (data.frame)
  # Beachte: Werden IDs statt KBs uebergeben, so werden diese, falls grua = "" uebergeben,  
  #          allein aus dem "Hauptkatalog" umgerechnet; ansonsten aus dem entsprechenden Sonderkatalog.
  #          Per Default - (TYPECAST) TPCST = TRUE - werden die Spalten in ihren entsprechenden Datentypen umgewandelt (IotoS).
    pick = function(dfrm = NULL, vIKs = c(), grua = "", TPCST = TRUE) {
      if (grua != "") {
        if (!meta_check_grua(grua, TYPE = GSLABS)) {
          warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
          return(NULL)
        }
      }
      if (is.null(dfrm)) {
        warning("Keine Daten uebergeben ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      #
      stat <- .self$chItoK(vIKs)
      # Wenn keine IDs/KBs uebergeben, dann alles casten/zurueckgeben:
      if (is.null(stat)) {
        if (TPCST) { dfrm <- .self$typeCast(dfrm, grua) }
        return(dfrm)
      }
      # Falls IDs, dann in KBs umrechnen:
      if (stat) { vIKs <- .self$IotoK(vIKs, grua) }
      # Spaltebindizes verschaffen:
          vInd <- match(vIKs, colnames(dfrm))
      vIndices <- vInd[which(!is.na(vInd))]
      if (length(vIndices) == 0) {
        warning("Spalten nicht gefunden ~ 'NULL' zurueckgegeben!")
        return(NULL) 
      } 
      retData <- subset(dfrm, select = vIndices)
      if (TPCST) { retData <- .self$typeCast(retData, grua) }
      return(retData)
    },
  # write to file FilterList - Selektionsdaten:
    wtfFltrList = function(fname = "FltrData", MERGE = TRUE, TPCST = FALSE) {
      tmpFltrList <- .self$FltrKFdataList.df
           LABELS <- GSLABS
      #
      if (MERGE) { 
        LABELS <- GLABS
         df_uf <- rbind(tmpFltrList[["ub"]], 
                        tmpFltrList[["lf"]],
                        tmpFltrList[["gf"]],
                        tmpFltrList[["sf"]])
         #
         tmpFltrList[["uf"]] <- df_uf        
      }
      # Ausgabe:
      for (label in LABELS) {
        path <- paste0(m_PathSelektionen,"/",fname,"_",label,".csv")
        .self$wtfData(tmpFltrList[[label]], path, TPCST = TPCST, ENCOD = AKS_FILE_ENCODING)
      }
    },
  # write to file Selektionsdaten:
    wtfSelda = function(Index = 0, fPath = NULL, TPCST = TRUE) {
      data <- .self$getSelda(Index)
      #
      if(is.null(data)) {
        warning("Fehler aufgetreten ~ keine Operation durchgefuehrt.")
      } else {
        retList <- .self$getSelda(Index, META = TRUE)
        .self$wtfData(data, fPath, retList$GRUA, TPCST = TPCST, ENCOD = AKS_FILE_ENCODING) 
      }
    },
  # write to file - Daten:
    wtfData = function(data = NULL, fPath = NULL, grua = "", TPCST = TRUE, ENCOD = STD_FILE_ENCODING) {
      if (is.null(data)) {
        warning("Keine Daten uebergeben ~ Keine Daten abgespeichert.")
        return(FALSE)
      }
      if (is.null(fPath)) {
        # aktuellen Pfad abspeichern:
        tmpPath <- getwd()
        # Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
        setwd(m_PathSelektionen)
        # Benutzerauswahl Selektions-csv:
        fPath <- file.choose()
        # Pfad zuruecksetzen
        setwd(tmpPath)
      }
      if (TPCST) {
        tmpData <- .self$typeCast(data, grua)
        if (!is.null(tmpData)) {
          data <- tmpData
        }
      }
      if(!grepl(grua, fPath)) {
        aStr <- substr(fPath, 1, nchar(fPath)-4)
        fPath <-  paste0(aStr,"_",grua,".csv")
      }
      write.table(x = data,
                  file = fPath,
                  append = FALSE,
                  quote = TRUE,
                  sep = ";",
                  eol = "\n",
                  na = "",
                  dec = ",", 
                  col.names = TRUE,
                  row.names=FALSE,
                  #qmethod = "escape",
                  fileEncoding = ENCOD) 
    },
  # load from file - Daten:
    lffData = function(fPath = NULL, ENCOD = STD_FILE_ENCODING, TPCST = FALSE, grua = "") {
      if (is.null(fPath)) {
        # aktuellen Pfad abspeichern:
        tmpPath <- getwd()
        # Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
        setwd(m_PathSelektionen)
        # Benutzerauswahl Selektions-csv:
        fPath <- file.choose()
        # Pfad zuruecksetzen:
        setwd(tmpPath)
      }
      df <- read.table(file =fPath,
                 colClasses = "character",
                 header = TRUE,
                 na.strings = "",
                 dec = ",",
                 sep = ";",
                 fill = TRUE,
                 fileEncoding = ENCOD)
      #
      if (TPCST) { df <- .self$typeCast(df, grua) }
      #
      return(df)
    }
  )
)
