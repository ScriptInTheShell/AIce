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
# Version:      1.0.3-beta
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
  #     list(ASEL, MASK)
  #          ASEL = Selektionsanssatz-ASel (enthaelt u.a. die GRUA),
  #          MASK = logischer Vektor (TRUE = positiv selektiert), 
    SelectionList.df  = "list",
  # Anzahl der bereits getaetigten Selektionen:
    AnzSelections.int = "integer"
  ),
 #------------------------------------ 
 #  METHODEN:  |
 #-------------
  methods = list(
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
 #DESTRUKTOR:
    finalize = function()  {
      # print("Aufruf des aice.rc Destruktors.")
    },
 #ELEMENTE-KATALOG-LOADER:  wegen Umfang ausgelagert in "aice_func_load_data.R"
    loadKtgs = com_load_ktgs,
 #DATA-LOADER:  wegen Umfang ausgelagert in "aice_func_load_data.R"
    loadData = aice_load_data,
 #
 #SELEKTIONEN:
  # selectMask - selektiert in FltrKFdataList.df via Mask
  # die Selektion wird NICHT intern abgespeichert
    selectMask = function(grua = NA_character_, mask = NULL, TPCST = FALSE) {
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      if (is.null(mask)) {
        warning("Keine Selektionsmaske uebergeben, die selektiert werden kann ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      message(paste0(length(which(mask)), " Kauffaelle selektiert."))
      retData <- .self$FltrKFdataList.df[[grua]][which(mask),]
      if (TPCST) {
        retData <- .self$typeCast(retData, grua)
      }
      return(retData)
    },  
  # selectMaskDirect - selektiert in data via Mask  
    selectMaskDirect = function(data = NULL, mask = NULL, TPCST = FALSE) {
      if (is.null(data)) {
        warning("Keine Daten uebergeben, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (is.null(mask)) {
        warning("Keine Selektionsmaske uebergeben, anhand derer selektiert werden kann ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      message(paste0(length(which(mask)), " Kauffaelle selektiert."))
      retData <- data[which(mask),]
      if (TPCST) {
        retData <- .self$typeCast(retData, grua)
      }
      return(retData)
    },
  # selectDirect - wie select, selektiert aber aus "data" und nicht aus ".self$FltrKFdataList.df"
  # - die GRUA im asel-Objekt wird nur in "typeCast()" benutzt
  # - die Selektion wird NICHT intern abgespeichert
    selectDirect = function(data = NULL, aselObjPth = NULL, INVERT = FALSE, MASK = FALSE, TPCST = FALSE) {
      ## ASel - Pruefung wie in .self$select()
      if (is.null(data)) {
        warning("Keine Daten uebergeben, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      aselObj <- .self$checkAsel(aselObjPth, INVERT = INVERT)
         grua <- aselObj$getGrua()
      #
      ## Daten selektieren:
      blnMASK <- aselObj$select(data)
      if (MASK) {
       return(blnMASK) 
      }
      vRows   <- which(blnMASK)
      AnzRws  <- length(vRows)
      retData <- data[vRows,]
      message(paste0(AnzRws, " Kauffaelle selektiert."))
      ## daten zurueckgeben:
      if (TPCST) {
        retData <- .self$typeCast(retData, grua)
      }
      return(retData)
    },
  # selectSelda - Selektiert aus der entsprechenden intern abgespeicherten Selektion:
    selectSelda = function(aselObjPth = NULL, Index = 0, INVERT = FALSE, TPCST = TRUE) {
      if (!.self$DataLoaded.log) {
        warning("selectSelda: Keine Daten geladen, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      aselObj <- .self$checkAsel(aselObjPth, INVERT = INVERT)
      retList <- .self$getSelda(Index = Index, META = TRUE, TPCST = FALSE)
      #
      if (is.null(retList)) {
        message("selectSelda: Fehler in getSelda() ~ 'NULL' zurueck gegeben!")
        return(NULL)
      }
      # Zunaechst "retList$ASEL" klonen, das es sonst u.U. abgeaendert wird:
      clnAsel <- retList$ASEL$clone()
      # Selektionsansaetze verschmelzen - die GRUA von "aselObj" wird ignoriert:
      clnAsel$merge(aselObj)
      # Hiermit selektieren:
      .self$select(clnAsel, INVERT = INVERT, TPCST = TPCST)
    },
  # select - Selektiert die Daten via uebergebenem asel.rc-Objekt:
  # aselObjPth:
  #    asel.rc-Obj. es wird via dem uebergebenen ASel selektiert 
  #    NULL / ""    es wird neues asel erzeugt und via Open-Dialog geladen (in asel_load_sela)
  #    "<Pfad>"     es wird neues asel erzeugt und der Selektionsansatz unter "<Pfad>" geladen (in asel_load_sela)  
    select = function(aselObjPth = NULL, DATA = NULL, INVERT = FALSE, TPCST = TRUE) {
      if (!is.null(DATA)) {
        return(.self$selectDirect(data = data, aselObjPth = aselObjPth, INVERT = INVERT, TPCST = TPCST))
      } else if (!.self$DataLoaded.log) {
        warning("Keine Daten geladen, die selektiert werden koennen ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      aselObj <- .self$checkAsel(aselObjPth, INVERT = INVERT)
      #
      tmpGrua <- aselObj$getGrua()
      if (is.null(tmpGrua)) {
        warning("Fehlende GRUA ~ keine Selektion durchgefuehrt ~ 'NULL' zurueckgegeben.")
        return(NULL) 
      }
      # selektieren:
      tmpSelMask <- aselObj$select(.self$FltrKFdataList.df[[tmpGrua]])
      # Intern abspeichern:
                                .self$AnzSelections.int <- as.integer(.self$AnzSelections.int + 1)
      .self$SelectionList.df[[.self$AnzSelections.int]] <- list(ASEL = aselObj$clone(), MASK = tmpSelMask)
      # Selektion zurueckgeben:
      return(.self$getSelda(Index = 0, META = FALSE, TPCST = TPCST))
    },
 #PRUEF-METHODEN:
  # checkWrongs:  
  # aselObj mit INVERT = FALSE als Ausgangspunkt:
  #    ~ gibt Liste der Kauffaelle, die Selektion nicht erfuellen zusammen mit Selektionstabelle, 
  #      sodass man sehen kann, welche der aselObj-Kriterien nicht erfuellt wurden.
  #    ~ benutzt checkPar
  # data: 
  #    ~ data.frame: Selektion aus dem uebergebenen data.frame 
  #    ~ NULL:       Selektion aus der Filterliste zur GRUA im ASel (falls gesetzt)
  #    ~ integer:    Selektion aus dem entsprechenden Selda
    checkWrongs = function(aselObjPth = NULL, data = NULL) {
      #
      aselObj <- .self$checkAsel(aselObjPth, INVERT = FALSE)
      #
      if (is.null(data)) {
        if (aselObj$checkGrua()) {
          message(paste0("Keine Daten uebergeben ~ auf Filterliste '",aselObj$getGrua(),"' zurueckgegriffen."))
          data <- .self$FltrKFdataList.df[[aselObj$getGrua()]]
        } else {
          message("Keine Daten uebergeben und keine GRUA im ASel-Objekt festgelegt ~ 'NULL' zurueckgegeben!.")
          return(NULL)
        }
      } else if (class(data) %in% c("numeric", "integer")) {
        data <- .self$getSelda(Index = data, META = FALSE, TPCST = FALSE)
      }
      # Merken und am Ende zuruecksetzen:
      tmpInvFlag <- aselObj$getInv()
      ##
      aselObj$setInv(TRUE)                                            
      df_wrongs <- .self$selectDirect(data = data, aselObjPth = aselObj, INVERT = FALSE, MASK = FALSE, TPCST = FALSE)
      ##                                                                 --------------- wird sowieso ignoriert, da ASel-Ob. uebergeben    
      aselObj$setInv(FALSE)     
      ret_df <- suppressMessages(.self$checkPar(aselObjPth = aselObj, data = df_wrongs))
      #
      # INV-Flag wiederherstellen:
      aselObj$setInv(tmpInvFlag)
      #
      return(ret_df)
    },
  # checkPar - Nimmt data.frame "data" und selektiert jeden Parameter des uebergebenen asel.rc-Objektes separat durch.
  # Wird "data = NULL" uebergeben, so wird aus der entsprechenden Filterliste selektiert.
  # Es wird eine entsprechende Liste erstellt und zurueckgegeben, sodass man sehen kann, welche Selektionsparameter bei 
  # welchem Kauffal erfuellt bzw. nicht erfuellt gewesen sind
  # Die Spaltennamen sind von der Form <KB>[<SepaIndex>]; und zwar zur Unterscheidung, falls man für eine KB mehrere 
  # Sepas festgelegt hat.
  # VORSICHT:  Ist in aselObj das Inversionsflag auf 'TRUE' gesetzt, so wird dieses zwar belassen aber ignoriert! 
    checkPar = function(aselObjPth = NULL, data = NULL) {
      #
      aselObj <- .self$checkAsel(aselObjPth, INVERT = FALSE)
      #  
      nAnzPar <- aselObj$getSepaCnt()
      if (nAnzPar == 0) {
        warning("Das asel.rc-Objekt enthaelt keine Parameter ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      #
      if (is.null(data)) {
        if (aselObj$checkGrua()) {
          message(paste0("Keine Daten uebergeben ~ auf Filterliste '",aselObj$getGrua(),"' zurueckgegriffen."))
          data <- .self$FltrKFdataList.df[[aselObj$getGrua()]]
        } else {
          message("Keine Daten uebergeben und keine GRUA im ASel-Objekt festgelegt ~ 'NULL' zurueckgegeben!.")
          return(NULL)
        }
      } else if (class(data) %in% c("numeric", "integer")) {
        data <- .self$getSelda(Index = data, META = FALSE, TPCST = FALSE)
      }
      #
      if (aselObj$getInv()) {
        message("'INVERT = TRUE' des uebergebenen asel.rc-Objektes wird ignoriert.")
      } 
      #
      retFrm <- subset(data, select = c("KFKZ")) 
      for (ind in (1:nAnzPar)) {
        tmp_asel <- aselObj$reduce(ind)
        # moegliche geerbte Inversionsflags unsetten:
        tmp_asel$setInv(FALSE)
        tmp_sepa <- tmp_asel$getSepaInd(1)
           colnm <- paste0(tmp_sepa$Meta.lst$ID.lst$vKB[1],"[",ind,"]")
         retMask <- .self$selectDirect(data = data, aselObjPth = tmp_asel, INVERT = FALSE, MASK = TRUE, TPCST = FALSE)
         selMask  <- data.frame(retMask)
        colnames(selMask) <- c(colnm)
                   retFrm <- cbind(retFrm, selMask)
      }
      return(retFrm)
    },
  # checkData: per default werden alle GRUAs geprueft, und zwar in den gefilterten Kauffalldaten.
    checkData = function(GRUAS = NA_character_, FLTR = TRUE) {
      if (!all(meta_check_grua(GRUAS, TYPE = GSLABS))) {
        GRUAS <- GSLABS       
      }
      if (FLTR) {
        tmpData <- .self$FltrKFdataList.df
      } else {
        tmpData <- .self$KFdataList.df
      }
      for (grua in GRUAS) {
        .self$typeCast(tmpData[[grua]], grua, NoEntryErrMsg = FALSE, SptErrMsg = FALSE, TPCST = TRUE)
      }
    },  
  # checkKataloge:
    checkKtgs = function(GRUAS = NA_character_, NoEntryErrMsg = TRUE, SptErrMsg = TRUE) {
      if (!all(meta_check_grua(GRUAS, TYPE = GSLABS))) {
        GRUAS <- GSLABS       
      }
      for (grua in GRUAS) {
        message(paste0("\nGRUA: <",grua,">"))
        .self$typeCast(.self$KFdataList.df[[grua]], grua, NoEntryErrMsg = NoEntryErrMsg, SptErrMsg = SptErrMsg, TPCST = FALSE)
      }
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
      message(paste0("Anzahl der abgespeicherten Selektionen: ", .self$AnzSelections.int))
      return(.self$AnzSelections.int)
    },
  # getSelda - gibt die Index-te Selektion zurueck, bzw. die letzte wenn Index = 0 (oder kein Parameter) uebergeben: 
    getSelda = function(Index = 0, META = FALSE, TPCST = FALSE) {
      if (.self$AnzSelections.int == 0) {
        warning("Keine Selektionen gespeichert ~ 'NULL'zurueckgegeben.")
        return(NULL)
      }
      if (Index == 0) { 
        Index <- .self$AnzSelections.int
      }
      if (!(1 <= Index && Index <= .self$AnzSelections.int)){
        warning(paste0("Fehlerhafter Index ~ Wert muss zwischen 0 und ", .self$AnzSelections.int," liegen!"))
        return(NULL)
      }
      if (META) {
        return(.self$SelectionList.df[[Index]])
      } else {
        tmpGrua <- .self$SelectionList.df[[Index]]$ASEL$getGrua()
        tmpRows <- which(.self$SelectionList.df[[Index]]$MASK)
        #
        message(paste0(length(tmpRows), " Kauffaelle selektiert."))
        retData <- .self$FltrKFdataList.df[[tmpGrua]][tmpRows,] 
        if (TPCST) {
          retData <- .self$typeCast(retData, tmpGrua)
        }
        return(retData)
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
        warning(paste0("Fehlerhafter Index ~ Wert muss zwischen 0 und ", .self$AnzSelections.int," liegen!"))
        return(NULL)
      }
      tmpGrua <- .self$SelectionList.df[[Index]]$ASEL$getGrua()
      tmpRows <- which(.self$SelectionList.df[[Index]]$MASK)
      #
      message(paste0(length(tmpRows), " Kauffaelle selektiert."))
      #
      if (META) {
        print(paste0("Selektierte  Grundstuecksart: ", tmpGrua))
        print("---------------------------------------------------------------------------------------------------------------")
        print("Selektierte Zeilen:")
        print(tmpRows)
        print("---------------------------------------------------------------------------------------------------------------")
        print("Selektierte Kauffaelle:")
        print(.self$KFdataList.df[[tmpGrua]][tmpRows,]$KFKZ)
      } else {
        print(.self$KFdataList.df[[tmpGrua]][tmpRows,])
      }
    },
  # getKauffalldaten nach GRUA:
    getData  = function(grua = NA_character_, FLTR = TRUE, TPCST = FALSE) { 
      if (!.self$DataLoaded.log) {
        warning("getData: Keine Daten uebergeben ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      if (FLTR) {
        retData <- .self$FltrKFdataList.df[[grua]]
      } else {
        retData <- .self$KFdataList.df[[grua]]
      }
      if (TPCST) { retData <- .self$typeCast(retData, grua) }
      return(retData)
    },
  # getColInds - gibt die Spaltenpositionen im Datensatz der uebergebenen (entweder nur) KBs / (oder nur) IDs wieder:
    getColInds = function(vIKs = c(), grua = NA_character_) {
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Keine gueltige GRUA uebergeben ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (is.null(vIKs)) {
        warning("getColInds: Kein Identifier uebergeben ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      #
      stat <- .self$chItoK(vIKs)
      if (is.null(stat)) { 
        return(NULL)
      } else if (stat) { 
        # Falls IDs, dann in KBs umrechnen:
        vIKs <- .self$IotoK(vIKs, grua)
      }
      #
      return(match(vIKs, colnames(.self$KFdataList.df[[grua]])))
    },
  # getElementeIDs nach GRUA (nur die, die auch im DS auftreten) bzw.im Hauptkatalog, falls Nichts angegeben:
    getElids = function(grua = NA_character_) {
      if (!meta_check_grua(grua, TYPE = GSLABS))  {
        warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
        return(NULL)
      }
      return(.self$ElemIdList.df[[grua]])
    }, 
  # getHauptkatalog:
    getHptKat = com_getHptKat,
  # getSonderkatalog:
    getSndKat = com_getSndKat,
 #SETTER-METHODEN:
  # setKauffalldaten nach GRUA:       
  # CHTPCST ~ char-typecast 
    setData  = function(data = NULL, grua = NA_character_, FLTR = TRUE, CHTPCST = TRUE) { 
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Ungueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ Keine aKtion durchgefuehrt!")
      } else {
        if (CHTPCST) {
          data <- as.char(data) 
        }
        if (FLTR) {
          .self$FltrKFdataList.df[[grua]] <- data
        } else {
          .self$KFdataList.df[[grua]] <- data
        }
      }
    },
  # setFilter - die entsprechenden Kauffalldaten werden vorselektiert:
  #  BEACHTE: + Als Vorlage werden die gefilterten Daten verwendet, d.h., mehrfaches "Filter-Setzen" via  
  #             unterschiedlichen ASels, reduziert die "Filter-Daten" immer weiter.
  #           + Zum Resetten dann rmvFltr benutzen.
    setFltr = function(aselObjPth = NULL, INVERT = FALSE) {
      #
      aselObj <- .self$checkAsel(aselObjPth, INVERT = INVERT)
      #
      if (aselObj$checkGrua()) {
        .self$FltrKFdataList.df[[aselObj$getGrua()]] <- .self$select(aselObj, INVERT = INVERT, TPCST = FALSE)
      } else {
        message("Keine GRUA festgelegt - Filter fuer alle GRUAs festgelegt.")
        for (grua in GSLABS) {
          aselObj$setGrua(grua)
          .self$FltrKFdataList.df[[grua]] <- .self$select(aselObj, INVERT = INVERT, TPCST = FALSE)
        }
        suppressWarnings(aselObj$setGrua(NA_character_))
      }
    },
  # remove Filter - entfernt die gesetzten Filter:
  #                 grua = NA :        alle Filter
  #                 grua = <grua>:     den <grua>-Filter
    rmvFltr = function(grua = NA_character_) {
      if (is.na(grua)) {
        .self$FltrKFdataList.df <- .self$KFdataList.df
        message("keine Filter gesetzt (alle Filter entfernt)")
      } else if (meta_check_grua(grua, TYPE = GSLABS)) {
        .self$FltrKFdataList.df[[grua]] <- .self$KFdataList.df[[grua]]
        message(paste0(grua, " - Filter entfernt."))
      } else {
        warning("Ungueltige Eingabe: Keine Aktion durchgefuehrt!")
      }
    },
  # reduce - reduziert den Grunddatensatz via Selektion mit aselOb
    reduce = function(aselOb = NULL) {
      # Filter dem Grunddatensatz gleichsetzen:
      .self$rmvFltr(grua = NA_character_)
      # Filterdaten via aselOb reduzieren:
      .self$setFltr(aselOb)
      # Grunddatensatz dem Filterdatensatz gleichsetzen:
      .self$KFdataList.df <- .self$FltrKFdataList.df
      # Ein abschließendes ".self$rmvFltr()" kann man sich an dieser Stelle sparen.
    },
 #WEITERE DATEN-MANAGEMENT-METHODEN:
  # reset ~ Konstruktor afrufen ~ alles neu laden:
    reset = function(loadData = TRUE) {
      .self$initialize(loadData = loadData)
    },
  # clear ~ alles purgen und Filter entfernen:
    clear = function() {
      .self$purge()
      .self$rmvFltr(grua = NA_character_)
    },
  # purge - Entfernt alle Selektionsdaten:
    purge = function() {
      .self$rmvSelda(vInd = v())
    },  
  # rmvSelda() - Entfernt Selektionsdatum gemäß vInds:
    rmvSelda = function(vInd = c()) {
      if(.self$AnzSelections.int == 0) {
        message("aice.rc$rmvSelda: Die Selektionsdatenliste ist bereits leer ~ keine Aktion durchgefuehrt!")
      } else if (is.null(vInd)) {
        .self$SelectionList.df   <- list()
        .self$AnzSelections.int  <- as.integer(0)
      } else {
        vInd <- .self$fltrInd(vInd)
        if (!is.null(vInd)) {
          .self$SelectionList.df   <- .self$SelectionList.df[-vInd]
          .self$AnzSelections.int  <- length(.self$SelectionList.df)
          message("aice.rc$rmvSelda/purge: Alle Selektionsdaten entfernt.")
        } else {
          message("aice.rc$rmvSelda: Keine gueltigen Indizes uebergeben ~ keine Aktion durchgefuehrt!")
        }
      }
    },
 #HELPER-METHODEN:
  # checkAsel
    checkAsel = function(aselObjPth = NULL, INVERT = FALSE) {
      if (class(aselObjPth) == "AKS_Selektionsansatz") {
        message("AKS-Selektionsobjekt asel.rc-Objekt uebergeben.")
        retAselOb <- aselObjPth
      } else if (is.null(aselObjPth) | class(aselObjPth) == "character") {
        message("Kein AKS-Selektionsobjekt (asel.rc)-Objekt uebergeben.")
        if (is.null(aselObjPth)) {
          aselObjPth <- ""
        }
        retAselOb <- asel.rc$new(aselObjPth)
        # Inversionsflag setzen:
        retAselOb$setInv(INVERT)
      }
      return(retAselOb)
    },
  # IsInRange
    IsInRange = function(vInd) {
      return((1 <= vInd) & (vInd <= .self$AnzSelections.int))
    },
  # filtere-Indizes:
    fltrInd = function(vInd = c()) {
      retInd <- vInd[which(.self$IsInRange(vInd))]
      if (length(retInd) >= 1) {
        return(retInd)
      } else {
        warning("Index ausserhalb des Bereiches ~ 'NULL' zurueck gegeben.")
        return(NULL)
      }
    },
  # chItoK ~ check ID to KB conversion neccessary?
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
  # pickData - Extrahiert die zu vIKs (IDs oder KBs) gehoerigen Spalten aus "FltrKFdataList.df[[grua]]"  
    pickData = function(vIKs = c(), grua = NA_character_, FLTR = TRUE, TPCST = TRUE) {
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Keine GRUA uebergeben, in der selektiert werden soll ~ 'NULL' zurueckgegeben!")
        return(NULL)
      }
      if (FLTR) {
        pckData <- .self$FltrKFdataList.df[[grua]]
      } else {
        pckData <- .self$KFdataList.df[[grua]]
      }
      .self$pick(pckData, vIKs, TPCST, grua)
    },
  # 
    pickSelda = function(vIKs = c(), Index = 0, TPCST = TRUE) {
      dfrm <- .self$getSelda(Index = Index, META = FALSE, TPCST = FALSE)
      meta <- .self$getSelda(Index = Index, META =  TRUE, TPCST = FALSE)
      return(.self$pick(dfrm, vIKs, TPCST, meta$ASEL$getGrua()))
    },
  # pick     Extrahiert die zu vIKs (IDs oder KBs) gehoerigen Spalten aus "dfrm" (data.frame)
  # Beachte: Werden IDs statt KBs uebergeben, so werden diese, falls grua = NA uebergeben,  
  #          allein aus dem "Hauptkatalog" umgerechnet; ansonsten aus dem entsprechenden Sonderkatalog.
  #          Per Default - (TYPECAST) TPCST = TRUE - werden die Spalten in ihren entsprechenden Datentypen umgewandelt (IotoS).
    pick = function(dfrm = NULL, vIKs = c(), TPCST = TRUE, grua = NA_character_) {
      if (!is.na(grua)) {
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
      if (!TPCST) {
        return(subset(dfrm, select = vIndices))
      } else {
      # Nun zunaechst typecasten, wobei Dopplungen in den Spaltennamen entfernt werden muessen, 
      # weil "subset(...)" sonst mehrfach auftretende Spaltennamen umbenennt ~ <KB>.<Nummer>
          vUnique <- unique(vIndices)
          vRelInd <- match(vIndices, vUnique)
        tpcstData <- subset(dfrm, select = vUnique) |> .self$typeCast(grua, NoEntryErrMsg = TRUE, SptErrMsg = TRUE)    
        return(subset(tpcstData, select = vRelInd))
      }
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
        .self$wtfData(tmpFltrList[[label]], path, grua = label, TPCST = TPCST, ENCOD = AKS_FILE_ENCODING)
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
        .self$wtfData(data, fPath, retList$ASEL$getGrua(), TPCST = TPCST, ENCOD = AKS_FILE_ENCODING) 
      }
    },
  # write to file - Daten:
    wtfData = function(data = NULL, fPath = NULL, grua = NA_character_, TPCST = TRUE, ENCOD = STD_FILE_ENCODING) {
      if (is.null(data)) {
        warning("Keine Daten uebergeben ~ Keine Daten abgespeichert.")
        return(FALSE)
      }
      if (!meta_check_grua(grua, GSLABS)) {
        grua <- "NA"
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
    lffData = function(fPath = NULL, ENCOD = STD_FILE_ENCODING, TPCST = FALSE, grua = NA_character_) {
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
