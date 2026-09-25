###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#-----------------------
# AKS-SELEKTIONSANSATZ | 
#----------------------
# Version:      1.0.0-beta
# Author:       Dr. Maximilian Hanusch
# Maintainer:   Dr. Maximilian Hanusch  ~  "maximilian.hanusch@lkprignitz.de"
#
###########################################################################################################################
#
source("aice/functions/com_func/func_com_meta.R")
source("aice/functions/asel_func/func_asel_meta.R")
source("aice/asp.R")
#
###########################################################################################################################
#  ERLAEUTERUNGEN #
###########################################################################################################################
#
# Zu den Selektions-Logikregeln, siehe 
#               asp.R  ->  ERLAEUTERUNGEN  ->  LOGIKREGELN
#
###########################################################################################################################
###########################################################################################################################
#--------------------------------------------------------------------------------------------------------------------------
#  AKS-SELEKTIONSANSATZ.REFERENCE-CLASS  |
#-----------------------------------------
#
asel.rc <- setRefClass(
 # Name der RC-Klasse
 "AKS_Selektionsansatz", 
 #------------------------------------ 
 #  DATENFELDER  |
 #---------------
  fields = c(
  ##  
  ## VARIABLEN_FUER_DEN_IMPORT_EINES_SELEKTIONSANSATZES:   
   # Dateipfad zum importierten Selektionsansatzes:
     ImpSelPath.chr = "character",
     # Dateiname des Selektionsansatz:
     ImpSelName = "character",
   # Selektionsansatz.csv:
     ImpSel.df = "data.frame",
   # ImpSel.df gesplittet in: 
     ImpSelSplitList.df = "list", 
   # Anzahl der Teile
     nImpSS.int = "numeric",
  ##
  ## SELKETIONSPARAMETER:
   # Soll nach der Selektion invertiert werden?
     Invert.log  = "logical",
   # GRUA die selektiert werden soll    
     GruaSel.chr = "character",
   # Liste der Selektionsparameter-Objekte (asp.rc-Objekte)
     ObAsp.lst   = "list",
   # Anzahl Selektionsparameter-Objekte
     nObAsp.int  = "numeric",
  ##
  ## ELEMENTEKATALOGDATEN - wie in aice.rc:
   # Hauptkatalog:
     ElHptKTG.df = "data.frame",
   # Sonderkataloge:
     ElSndKtgList.df = "list"
  ),
 #------------------------------------ 
 #  METHODEN:  |
 #-------------
  methods = list(
 #ELEMENTE-KATALOG-LOADER:  wegen Umfang in eine Datei ausgelagert:
    loadKtgs = com_load_ktgs,
 #DATA-LOADER:  wegen Umfang in eigene Datei outgesourced (aice_func_load_sela.R)   
    loadSela = asel_load_sela,
 #KONSTRUKTOR:  wenn NULL         uebergeben, so wird leerer asel erzeugt
    #           wenn "" ~ default uebergeben, so wird Selektionsansatz via Dateimanager-Benutzer-Auswahl geladen  
    #           wenn "<Pfad>"     uebergeben, so wird Selektionsansatz unter "<Pfad>" geladen.  
    #           wenn "<grua>"     uebergeben, so wird leerer asel mit GRUA = <grua> erzeugt  
    initialize = function(entry = "") {
      .self$ObAsp.lst       <- list()
      .self$nObAsp.int      <- as.integer(0)
      .self$Invert.log      <- FALSE
      .self$GruaSel.chr     <- ""
      .self$ImpSelPath.chr  <- ""
      #
      .self$ElHptKTG.df     <- data.frame()
      .self$ElSndKtgList.df <- list()
      .self$loadKtgs(m_PathMetaDat)
      #
      if (is.null(entry)) {
        message("Leeres asel.rc-Objekt erzeugt; keine GRUA festgelegt ~ nutze: asel$setGrua().")
      }
      else {
        if (meta_check_grua(entry, TYPE = GSLABS)) {
          .self$GruaSel.chr <- entry  
        }
        else {
          .self$loadSela(entry)
        }
      }
    },
 #DESTRUKTOR:
    finalize = function() {
      # print("Aufruf des asel.rc Destruktors.")
    },
 #ELEMENTARE OPERATIONEN:
  # select - liefert die zutreffenden Zeilenindices des uebergebenenen Dataframes zurueck:
  # Zur NA-Liste tragen nur die Selektionselemente derjenigen Sepas bei, die KEIN Metakriterium ('!'/'=') enthalten.
    select = function(dataFrame) {
      # NA-Tabelle
      NA.df  <- data.frame("KFKZ" = dataFrame$KFKZ)
      # Selektionstabelle
      blnSel <- rep(TRUE, times = nrow(dataFrame))
      #
      if (.self$nObAsp.int == 0) {
        return(list(blnSMASK = blnSel, NULL))
      }
      # Schleife ueber die asp-Objekte:
      for (i in 1:.self$nObAsp.int) {
        # selektieren:
        retList <- .self$ObAsp.lst[[i]]$select(dataFrame)
        # Seleketionsdaten updaten:
        blnSel <- blnSel & retList$blnSELECT
        # Wenn keine Meta-Selektion, dann NA-Daten an Tabelle anhaengen:
        if (!.self$ObAsp.lst[[i]]$Meta.lst$blnMTK) {
          # NA-Daten:
          NA.df <- cbind(NA.df, retList$naLIST)
        }
      }
      # Wenn Inversionflag gesetzt, dann Selektion invertieren (NAs bleiben NAs).
      blnSel <- xor(.self$Invert.log, blnSel)
      #
      return(list(blnSMASK = blnSel, NAS = NA.df[rowSums(is.na(NA.df)) > 0, ]))
    },
  # clear - löscht die Liste ~ "Nullselektion"
    clear = function() { 
      .self$ObAsp.lst      <- list()
      .self$nObAsp.int     <- as.integer(0)
      .self$Invert.log     <- FALSE
      .self$GruaSel.chr    <- ""
      .self$ImpSelPath.chr <- ""
      message("asel.rc-Object bereinigt.")
    }, 
  # findSepa - Zu IdKb, ALLE Listeneintraege in ObAsp.lst zurueckgeben, die diese beinhalten.
    findSepa = asel_find_sepa,
  # metamorph:  '!' | '='  ->  "ist belegt" | "nicht belegt", sortiert zudem Inkonsistenzen/Dopplungen aus
  # wird benutzt in asel$addSepa
  # [a] ist ein "ist belegt" vorhanden, so wird alles andere weggeworfen 
  # gilt [a] nicht:  ist ein Negationsanteil <>(...) vorhanden, so wird außer einem "nicht belegt" alles weggeworfen
  #                     bspw.:  = # <>(.....) # {...}  ->  = # <>(.....)   
    metamorph = asel_metamorph,
  # addSepa - SelektionsParameter hinzufuegen ~ gibt den Index in ObAsp.lst zurueck | bei Fehler 0:
  # function(IdKb = NA, InputStr = "", pInd = 0)
  #               pInd = 0 :   Sepa wird ans Ende von ObAsp.lst angehaengt
  # 0 < pInd <= nObAsp.int :   ObAsp.lst[[pInd]] wird durch den neuen Sepa ersetzt
    addSepa = asel_add_sepa,
  # rmvSepa - remove Sepa - SelektionsParameter entfernen via Index:
    rmvSepa = function(vInd = c()) {
      if (.self$nObAsp.int == 0) { 
        message("Die Selektionsparameterliste ist leer.")
        return(FALSE)
      }
      # Auftreten von Infinty abchecken:
      Infs <- which(vInd == Inf)
      if (length(Infs) >= 1) {
        if (.self$IsInRange(vInd[1]) & vInd[2] == Inf) {
          vInd <- vInd[1]:.self$nObAsp.int
        }
        else {
          warning(paste0("Das Unendlichsymbol muss in der Form c(Index, Inf, ...) benutzt werden, mit '<Index>' zwischen ", 1 ," und ",.self$nObAsp.int , ".\n  Keine Aktion ausgefuehrt!"))
          return(FALSE)
        }
      }
      vInd <- .self$fltrInd(vInd)
      if (is.null(vInd)) {
        warning(paste0("Es werden nur Indizes beruecksichtigt, die zwischen ", 1 ," und ",.self$nObAsp.int , " liegen ~ Keine Aktion ausgefuehrt!"))
        return(FALSE)
      }
      .self$ObAsp.lst <- .self$ObAsp.lst[-vInd]
      .self$nObAsp.int <- length(.self$ObAsp.lst)
      message(paste0("Die folgenden Selektionsparameter wurden entfernt: ", paste(vInd, collapse = ",")))
    },
  # purge - alle SelektionsParameter entfernen, die IdKb beinhalten:
  # ALL=FALSE - die ID/KB wird gepurged:  alle Sepas, die die uebergeben ID/KB enthalten werden removed 
  # ALL=TRUE  - alle zugehoerigen Perioden (IDs|KBs) werden gepurged
    purge = function(IdKb = NULL, ALL = FALSE) {
      if(.self$nObAsp.int == 0) { 
        message("Die Selektionsparameterliste ist leer ~ keine Aktion durchgefuehrt!")
      } else {
        aspInd <- .self$findSepa(IdKb, ALL)
        if(!is.null(aspInd)) {
          .self$rmvSepa(aspInd)
        } else {
          message("Keine Eintraege gefunden ~ keine Aktion durchgefuehrt!")
        }
      }
    },
  # update Sepa - ersetzt den Inhalt von ObAsp.lst via InputStr:  
    updtSepa = function(pInd = 0, InputStr = "") {
      if(.self$nObAsp.int == 0) { 
        message("Die Selektionsparameterliste ist leer.")
        return(FALSE)
      }
      if (!.self$IsInRange(pInd)) {
        warning(paste0("Index muss zwischen ", 1 ," und ",.self$nObAsp.int , "liegen."))
        return(FALSE)
      }
      tmpKB <- .self$ObAsp.lst[[pInd]]$Meta.lst$ID.lst$vKB[1]
      .self$addSepa(tmpKB, InputStr, pInd)  
      return(TRUE)
    },
 #GETTER-METHODEN:
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
  # getGrua - die Grudstuecksart zurueckgeben, die aktuell selektiert wird 
    getGrua = function() {
      if (!.self$checkGrua()) {
        warning("Noch keine GRUA festgelegt: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf'")
        return(NULL)
      }
      return(.self$GruaSel.chr)
    },
  # checkGrua - return:  FALSE, falls keine GRUA festgelegt; ansonsten TRUE
    checkGrua = function() {
      if (meta_check_grua(.self$GruaSel.chr, TYPE = GSLABS)) {
        return(TRUE)
      }
      return(FALSE)
    }, 
  # getSplitlist - Splitlist zurueckgeben
    getSplitList = function() {
      return(.self$ImpSelSplitList.df)
    },
  # getSela - Selektionsansatz zurueckgeben, "printen" falls print = TRUE
    getSela = function() {
      return(.self$ObAsp.lst)
    },
  # get Sepa nach Index
    getSepaInd = function(Index) {
      if (.self$IsInRange(Index)) {
       return(ObAsp.lst[[Index]])
      }
    },
  # get Sepa nach ID|KB
    getSepaIK = function(IdKb, ALL = FALSE) {
      aspInds <- .self$findSepa(IdKb, ALL)
      if (is.null(aspInds)) {
        message(paste0(IdKb, ": ", "Kein Selektionsparameter zugeordnet!"))
      } else {
        retList <- ObAsp.lst[aspInds]
        return(retList)
      }
    },
  # getSepaCnt - Anzahl der Selektionsparameter
    getSepaCnt = function() {
      return(.self$nObAsp.int)
    },
  # getInv - get-InversionFlag
    getInv = function(PRT = FALSE) {
      if (PRT) {
        print("-------------------")
        print(paste0("INVERSION = ", .self$Invert.log, " |"))
        print("-------------------")
      } else {
        return(.self$Invert.log)
      }
    },
 #SETTER-METHODEN:
  # set Inversion - set-Inversionflag - soll nach der Selektion alles invertiert werden?
    setInv = function(inv = FALSE) {
      if (inv %in% c(TRUE,FALSE)) {
        .self$Invert.log <- inv
        message(paste0("Inversionsflag gesetzt: '", toString(inv), "'"))
        
      } else {
        warning(paste0("Ungueltige Eingabe: '", toString(inv), "' ~ Keine Aktion durchgefuehrt."))
      }
    },
  # set Sepa-Inversion - soll mit dem inversen Sepa selektiert werden?  
    setSepaInv = function(pInd = 0, INVERT = FALSE) {
      if(.self$nObAsp.int == 0) { 
        message("Die Seletionsparameterliste ist leer.")
        return(FALSE)
      } else if (!.self$IsInRange(pInd)) {
        warning(paste0("Index muss zwischen ", 1 ," und ",.self$nObAsp.int , "liegen."))
        return(FALSE)
      } else {
        .self$ObAsp.lst[[pInd]]$setInv(INVERT)
      }
    },
  # setGrua - die Grudstuecksart festlegen, auf die selektiert werden soll
    setGrua = function(grua = "") {
      if (!meta_check_grua(grua, TYPE = GSLABS)) {
        warning("Ungueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' - keine AKtion durchgefuehrt!")
      } else {
        # GRUA setzen
        .self$GruaSel.chr <- grua
        # alle Sepas updaten - genauer die ID/KB - Infos, die jetzt auch aus den Sonderkatalogen gezogen werden:
        if (.self$nObAsp.int >= 1) {
          for (i in 1:.self$nObAsp.int) {
            .self$updtSepa(i, .self$ObAsp.lst[[i]]$Meta.lst$PrtSTR) 
          }
        }
      }
    },
 #HELPER-METHODEN:
  # Spaltentausch Data.Frame
      dfm = com_helper_df_mutate,
  # chItoK - # check ID / KB - prueft ob "vecIKs" richtig belegt; und ob ID in KB umgerechnet werden muss: 
    chItoK = com_chItoK,
  # IotoK- "Umrechnung:"   ElementeId  <->  Kurzbezeicnung
    IotoK = com_IotoK,
  # IKtoS - gibt den SP(selektionsparameter)-Typen zurueck, der zur uebergebenen ID bzw. Kurzbez. gehört
    IKtoS = com_IKtoS,
  # ItoP - Nimmt ID, und gibt Periodenelemente-IDs/KBs zurueck:
  #   ID ~ Keine Periode:     gibt IMMER (ID,KB) zurueck
  #      - Hauptperioden-ID:  gibt IMMER IDs/KBs aller Periodenelemente zurueck
  #                           ----------------------
  #                           HID ~ Haupt-ID - Flag:
  #      - Unterperiode:      HID = FALSE (default) - es wird nur list(ID,KB) zurueckgegeben
  #                           - wird in "addSepa" benutzt  
  #                           HID = TRUE            - es werden alle assoziierten Periodenelemente rausgesucht
  #                           - wird in "findSepa" benutzt (im Falle ALL = TRUE)
     ItoP = com_ItoP,
  # IsInRange
    IsInRange = function(vInd) {
      return((1 <= vInd) & (vInd <= .self$nObAsp.int))
    },
  # FltrInd - 0-llen werden ignoriert
    fltrInd = function(vInd = c()) {
      retInd <- vInd[which(.self$IsInRange(vInd))]
      if (length(retInd) >= 1) {
        return(retInd)
      } else {
        warning("Index ausserhalb des Bereiches ~ 'NULL' zurueck gegeben.")
        return(NULL)
      }
    },
  # write to file - Selektionsansatz:
    wtfSela = function(fPath = "") {
                tmpdf <- data.frame(matrix(ncol = 4, nrow = .self$nObAsp.int + 2))
      colnames(tmpdf) <- c("Nummer", "Elementname", "Selektionsansatz", "Entschluesselung")
      #
      for (i in 1:.self$nObAsp.int) {
        #~ list(blnMTK = logical, ID.lst, SelaSTR = char, SPT = char) 
        tmpdf$Nummer[i]             <- .self$ObAsp.lst[[i]]$Meta.lst$ID.lst$vID[1] 
        tmpdf$Elementname[i]        <- .self$ObAsp.lst[[i]]$Meta.lst$ID.lst$vKB[1]
        tmpdf$Selektionsansatz[i]   <- .self$ObAsp.lst[[i]]$Meta.lst$PrtSTR 
        tmpdf$Entschluesselung[i]   <- "" 
      }
      tmpdf$Nummer[.self$nObAsp.int + 1]   <- "yummy"
      tmpdf$Nummer[.self$nObAsp.int + 2]   <- "dummy"
      #
      if (fPath == "") {
        #aktuellen Pfad abspeichern:
        tmpPath <- getwd()
        #Pfad in das Verzeichnis mit den Selektionsansaetzen setzen:
        setwd(m_PathSelektAns)
        #Benutzerauswahl Selektions-csv:
        fPath <- file.choose()
        #Pfad zuruecksetzen
        setwd(tmpPath)
      }
      if(!grepl(.self$GruaSel.chr, fPath)) {
        aStr <- substr(fPath, 1, nchar(fPath)-4)
        fPath <-  paste0(aStr,"_",.self$GruaSel.chr,".csv")
      }
      write.table(x = tmpdf,
                  file = fPath,
                  append = FALSE,
                  quote = FALSE,
                  sep = ";",
                  eol = "\n",
                  na = "",
                  dec = ",", 
                  col.names = TRUE,
                  row.names=FALSE,
                  #qmethod = "escape",
                  fileEncoding = AKS_FILE_ENCODING) 
    },
  # print - Selektionsansatz ~ in der Konsole ausgeben:
    prtSela = function() {
      if (.self$nObAsp.int >= 1) {
        symbol <- "-"
        if (.self$Invert.log) {
          symbol <- ""
        }
        print(paste0("------------------",symbol))
        print(paste0("INVERSION = ", .self$Invert.log, " |"))
        print(paste0("------------------",symbol))
        if (.self$checkGrua()) {
          grua <- .self$getGrua()
        } else {
          grua <- "NA"
        }
        print(paste0("GRUA = ", grua, " |"))
        print("-----------")
        for (i in 1:.self$nObAsp.int) {
          print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          print(paste0("Sepa Nr.: ", i, "  |"))
          print("~~~~~~~~~~~~~~")
          .self$ObAsp.lst[[i]]$prtAsp()
          cat("\n")
        }
      } else {
        message("Keine Selektionsparameter festgelegt!")
      }
    },
  # print Sepa nach Index
    prtSepaInd = function(Index) {
      if (.self$IsInRange(Index)) {
        .self$ObAsp.lst[[Index]]$prtAsp()
      }
    },
  # print Sepa nach ID|KB
    prtSepaIK = function(IdKb, ALL = FALSE) {
      aspInds <- .self$findSepa(IdKb,ALL)
      if (is.null(aspInds)) {
        message(paste0(IdKb, ": ", "Kein Selektionsparameter zugeordnet!"))
      } else {
        for (ind in aspInds) {
          .self$ObAsp.lst[[ind]]$prtAsp()
        }
      }
    }
  )
)

