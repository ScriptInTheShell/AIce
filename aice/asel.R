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
# Version:      1.0.3-beta
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
  #.............................................................................
  ## VARIABLEN_FUER_DEN_IMPORT_EINES_SELEKTIONSANSATZES:
   # Hauptsaechlich zu Wartungszwecken gedacht.
   # Dateipfad zum importierten Selektionsansatzes:
     ImpSelPath.chr = "character",
     # Dateiname des Selektionsansatz:
     ImpSelName.chr = "character",
   # Selektionsansatz.csv:
     ImpSel.df = "data.frame",
   # ImpSel.df gesplittet in: 
     ImpSelSplitList.df = "list", 
   # Anzahl der Teile
     nImpSS.int = "integer",
  #.............................................................................
  ## SELEKTIONSPARAMETER:
   # Soll nach der Selektion invertiert werden?
     Invert.log  = "logical",
   # GRUA die selektiert werden soll:    
     GruaSel.chr = "character",
   # Liste der Selektionsparameter-Objekte (asp.rc-Objekte):
     ObAsp.lst   = "list",
   # Anzahl Selektionsparameter-Objekte:
     nObAsp.int  = "integer",
  #.............................................................................
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
 #KONSTRUKTOR:  wenn NULL         uebergeben, so wird leerer asel erzeugt
    #           wenn "" ~ default uebergeben, so wird Selektionsansatz via Dateimanager-Benutzer-Auswahl geladen  
    #           wenn "<Pfad>"     uebergeben, so wird Selektionsansatz unter "<Pfad>" geladen.  
    #           wenn "<grua>"     uebergeben, so wird leerer asel mit GRUA = <grua> erzeugt  
    initialize = function(entry = NULL) {
      .self$ObAsp.lst       <- list()
      .self$nObAsp.int      <- as.integer(0)
      .self$Invert.log      <- FALSE
      .self$GruaSel.chr     <- NA_character_
      .self$ImpSelPath.chr  <- ""
      .self$ImpSelName.chr  <- "" 
      #
      .self$ImpSel.df          <- data.frame()
      .self$ImpSelSplitList.df <- list() 
      .self$nImpSS.int         <- as.integer(0)
      #
      .self$ElHptKTG.df     <- data.frame()
      .self$ElSndKtgList.df <- list()
      .self$loadKtgs(m_PathMetaDat)
      #
      # NULL und NA bewirken dasselbe:
      if (is.null(entry)) { entry <- NA }
      if (is.na(entry)) {
        message("Leeres asel.rc-Objekt erzeugt; keine GRUA festgelegt ~ nutze: asel$setGrua().")
        .self$GruaSel.chr <- NA_character_    # redundant ~ aus Dokumentationszwecken
      } else if (class(entry) == "character"){
        if (meta_check_grua(entry, TYPE = GSLABS)) {
          .self$GruaSel.chr <- entry  
        } else {
          # wenn character aber keine GRUA, dann entweder "" (Open-Dialog) oder "<path>" (direkter Pfad)
          .self$loadSela(entry)
        }
      } else {
        # Falls bspw. numerische Werte uebergeben ... .
        warning(paste0("Ungueltige Parameteruebergabe: ", entry))
        message("Leeres asel.rc-Objekt erzeugt; keine GRUA festgelegt ~ nutze: asel$setGrua().")
        .self$GruaSel.chr <- NA_character_    # redundant ~ aus Dokumentationszwecken
      }
    },
 #DESTRUKTOR:
    finalize = function() {
      # print("Aufruf des asel.rc Destruktors.")
    },
 #ELEMENTE-KATALOG-LOADER:  wegen Umfang in eine Datei ausgelagert:
    loadKtgs = com_load_ktgs,
 #HELPER:
    checkSelaFilePath = asel_check_Sela_File_Path,
 #AKS-Selektionsansatz-LOADER:  wegen Umfang in eigene Datei ausgelagert (aice_func_load_sela.R)   
    loadSela = asel_load_sela,
 #Selektionstabellen-LOADER:  
    loadStab = function(SelPath = "", MERGE = TRUE) {
      if (!MERGE) {
        suppressMessages(.self$clear())
        message(paste0("GRUA zurueckgesetzt: '", .self$GruaSel.chr,"'"))
      }
      .self$ImpSelPath.chr <- .self$checkSelaFilePath(SelPath)
      if (!is.null(.self$ImpSelPath.chr)) {
        .self$ImpSel.df <- read.table(.self$ImpSelPath.chr,
                                      colClasses = "character",
                                      comment.char = "",
                                      header = TRUE,
                                      na.strings = "",
                                      dec = ",",  # obsolet wegen colClasses = "character"
                                      sep = ";",
                                      fill = TRUE,
                                      fileEncoding = STD_FILE_ENCODING)
        .self$merge(.self$ImpSel.df)
      }
    },
 #ELEMENTARE OPERATIONEN:
  # select - liefert die zutreffenden Zeilenindices des uebergebenenen Dataframes zurueck:
  # Zur NA-Liste tragen nur die Selektionselemente derjenigen Sepas bei, die KEIN Metakriterium ('!'/'=') enthalten.
    select = function(dataFrame) {
      # Selektionstabelle
      blnSel <- rep(TRUE, times = nrow(dataFrame))
      # Falls kein Kriterium vorhanden, so wird alles selektiert:
      if (.self$nObAsp.int == 0) {
        return(blnSel)
      }
      # Schleife ueber die asp-Objekte:
      for (i in 1:.self$nObAsp.int) {
        # selektieren:
        blnRetSel <- .self$ObAsp.lst[[i]]$select(dataFrame)
        # Seleketionsdaten updaten:
        blnSel <- blnSel & blnRetSel
      }
      # Wenn Inversionflag gesetzt, dann Selektion invertieren (NAs bleiben NAs).
      blnSel <- xor(.self$Invert.log, blnSel)
      #
      return(blnSel)
    },
  # reduce - gibt ein Sepa-reduziertes ASel zurueck:
  #        + Wenn 'NULL' uebergeben, dann wird geklont.
  #        + Die (redundanten) Loader-Variablen:  ImpSelPath.chr - nImpSS.int  werden nicht kopiert.
  # Anmerkung ~ Warum benutze ich nicht die R5-class-standard-copy-Methoden, also
  #             asel.rc/asp.rc$copy(shallow = TRUE/FALSE):
  #   + asel.rc$copy(shallow = FALSE) scheint die enthaltenen asp.rc-Objekte nicht verlaesslich deep-zu-kopieren ... 
  #   + copys rufen "initialize" im default-Modus auf: 
  #     - bei asp.rc wird dann ".self$convert" aufgerufen, sodass ich das separieren muesste ~ eas ich aber nicht will. 
    reduce = function(vInds = c()) {
      if (!is.null(vInds)) {
        # Wenn nicht 'NULL' uebergeben, dann ungueltige Indizes herausfiltern:
        vInds <- .self$fltrInd(vInds)
        if (is.null(vInds)) {
          warning(paste0("Es werden nur Indizes beruecksichtigt, die zwischen ", 1 ," und ",.self$nObAsp.int , " liegen ~ 'NULL' zurueckgegeben!"))
          return(NULL)
        }
      } else {
        # Wenn 'NULL' uebergeben, dann klonen ~ alle Indizes:
        vInds <- 1:.self$nObAsp.int
      }
      # Neues ASel erschaffen, und Metadaten kopieren:
      # Copy on modify hier unschaedlich:
                       ret_asel <- suppressMessages(asel.rc$new(NULL))
      suppressMessages(ret_asel$setGrua(.self$getGrua())) 
      suppressMessages(ret_asel$setInv(.self$getInv()))
      # In folgender Form wird tatsaechlich eine echte Kopie der ObAsp.lst erstellt:
      # Kopiert" man die ObAsp.lst via "<-", so werden die asp-Objekten nicht 
      # wirklich kopiert - zwar verhalten sich die Objekte dann bei updtSepa() wie 
      # getrennte Objekte, aber bei brute-force-Zuweisungen in den Felder nicht mehr so sehr ... .
      # Man koennte hier convert aus asp.rc$initialize rausnehmen, und dann asp.rc$copy(...) benutzen;
      # das will ich aber nicht.     
      for (ind in vInds) {  
        tmpKB       <- .self$ObAsp.lst[[ind]]$Meta.lst$ID.lst$vKB[1]
        tmpInputStr <- .self$ObAsp.lst[[ind]]$Meta.lst$PrtSTR
        ret_asel$addSepa(tmpKB, tmpInputStr)
      }
      #
      return(ret_asel)
    },
  # clone - gibt einene Klon des asels zurueck (ohne die Loader-Variablen): 
    clone = function() {
      return(.self$reduce(vInds = c()))
    },
  # merge - Ungeachtet der GRUAs:  
  # - asel.rc-Objekt:   Fuegt die Sepas von aselObj den Eigenen hinzu.
  # - data.frame:       Pro Spalte ~ ein Sepa mit KB = Spaltenname  sowie Ge-#-derte Zeileneintraege     
    merge = function(Obj = NULL) {
      if (!(class(Obj) %in% c("AKS_Selektionsansatz", "data.frame"))) {
        warning("Kein gueltiges Objekt uebergeben (asel.rc-Objekt oder data.frame) ~ keine Aktion ausgefuehrt!")
        return(NULL)
      }
      if (class(Obj) == "AKS_Selektionsansatz") {
        if (.self$getGrua() != Obj$getGrua()) {
          message(paste0("asel.rc$merge: Unterschiedliche Grundstücksarten: .self hat GRUA '", .self$getGrua()," (beibehalten)' | Integrand hat GRUA '", Obj$getGrua(),"' (ignoriert)"))
        }
       .self$ObAsp.lst <- append(.self$ObAsp.lst, Obj$ObAsp.lst)
      } else if (class(Obj) == "data.frame") {
        #
        if (ncol(Obj) == 0 | nrow(Obj) == 0) {
          warning("Uebergebenes 'data.frame' enthaelt keine Informationen ~ keine Aktion ausgefuehrt!")
          return(NULL)
        } else {  
          colNms <- colnames(Obj)
          for (col in colNms) {
            .self$addSepa(col, Obj[[col]])
          }
        }
      }
      .self$nObAsp.int <- length(.self$ObAsp.lst)
      #
      .self$ImpSelPath.chr     <- ""
      .self$ImpSel.df          <- data.frame()
      .self$ImpSelSplitList.df <- list() 
      .self$nImpSS.int         <- as.integer(0)
      #
      message(paste0("Neue Anzahl Selektionsparameter: ", .self$getSepaCnt()))
    },    
  # reset
    reset = function(entry = NULL) {
      .self$initialize(entry = entry)
    },
  # clear - löscht die Grua und die SelaListe und setzt das Inverisonsflag auf FALSE ~ "Nullselektion"
    clear = function() { 
      .self$purge(IdKb = NULL, ALL = FALSE)
      .self$setInv(FALSE)
      .self$setGrua(NA_character_)
    },
  # purge - alle SelektionsParameter entfernen, die IdKb beinhalten:
  # ALL=FALSE - die ID/KB wird gepurged:  alle Sepas, die die uebergeben ID/KB enthalten werden removed 
  # ALL=TRUE  - alle zugehoerigen Perioden (IDs|KBs) werden gepurged
  # Bei IdKb = NULL werden alle Sepas entfernt.
    purge = function(IdKb = NULL, ALL = FALSE) {
      if (is.null(IdKb)) {
        .self$ObAsp.lst          <- list()
        .self$nObAsp.int         <- as.integer(0)
        #
        .self$ImpSelPath.chr     <- ""
        .self$ImpSelName.chr     <- "" 
        .self$ImpSel.df          <- data.frame()
        .self$ImpSelSplitList.df <- list() 
        .self$nImpSS.int         <- as.integer(0)
        #
        message("asel.rc$purge: Alle Selektionsparameter entfernt.")
      } else if(.self$nObAsp.int == 0) {
        message("asel.rc$purge: Die Selektionsparameterliste ist leer ~ keine Aktion durchgefuehrt!")
      } else {
        aspInd <- .self$findSepa(IdKb, ALL)
        if(!is.null(aspInd)) {
          .self$rmvSepa(aspInd)
        } else {
          message("asel.rc$purge: Keine Eintraege gefunden ~ keine Aktion durchgefuehrt!")
        }
      }
    },
 # findSepa - Zu IdKb, ALLE Listeneintraege in ObAsp.lst zurueckgeben, die diese beinhalten.
    findSepa = asel_find_sepa,
  # AKS_Syntax_Converter:
  #
  #   [A] numerische Felder:
  #    a] Sobald ein '<'<= auftaucht, wird ein '= # ' angefuegt:   
  #                 "X #  <B # Y"  ->  "= # X #  <B # Y"  
  #                 "X # <=B # Y"  ->  "= # X # <=B # Y"  
  #                 "X # <>B # Y"  ->  "= # X # <>B # Y"  
  #    b] Negationskonverter:  
  #                   <>A<>B<>C    ->  <>(A # B # C)
  #
  #   [B] To be continued ... 
  #
    AKS_Syntax_Converter = asel_aks_syntax_converter,
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
  # "wrapper"
    aksSepa = function(IdKb = NA, InputStr = "", pInd = 0) {
      .self$addSepa(IdKb, InputStr, pInd, AKS = TRUE)
    },
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
  # update Sepa - ersetzt den Inhalt von ObAsp.lst[[pInd]] via InputStr:  
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
    getHptKat = com_getHptKat,
  # getSonderkatalog:
    getSndKat = com_getSndKat,
  # getGrua - die Grudstuecksart zurueckgeben, die aktuell selektiert wird 
    getGrua = function(SILENT = FALSE) {
      if (!.self$checkGrua()) {
        if (!SILENT) {
          message("Noch keine GRUA festgelegt: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf'")
        }
        if (!is.na(.self$GruaSel.chr)) {
          warning(paste0("Wert war: '",.self$GruaSel.chr,"', wurde nun auf 'NA' gesetzt."))
          .self$setGrua(NA_character_)          
        }
      }
      return(.self$GruaSel.chr)
    },
  # checkGrua - return:  FALSE, falls keine GRUA festgelegt; ansonsten TRUE
    checkGrua = function() {
      return(meta_check_grua(.self$GruaSel.chr, TYPE = GSLABS))
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
    setInv = function(INV = FALSE, SILENT = TRUE) {
      if (INV %in% c(TRUE,FALSE)) {
        .self$Invert.log <- INV
        if (!SILENT) { 
          message(paste0("Inversionsflag gesetzt: '", INV, "'"))
        }
      } else {
        warning(paste0("Ungueltige Eingabe: '", INV, "' ~ Keine Aktion durchgefuehrt."))
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
    setGrua = function(grua = NA_character_) {
      if (is.na(grua)) {
        .self$GruaSel.chr <- NA_character_
      } else if (meta_check_grua(grua, TYPE = GSLABS)) {
        .self$GruaSel.chr <- grua
      } else {
        warning("Ungueltige GRUA uebergeben: NA, 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' - keine Aktion durchgefuehrt!")
        return(NULL)
      } 
      if (.self$nObAsp.int >= 1) {
        for (i in 1:.self$nObAsp.int) {
          .self$updtSepa(i, .self$ObAsp.lst[[i]]$Meta.lst$PrtSTR) 
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
      if (.self$checkGrua()) {
        grua <- .self$getGrua()
      } else {
        warning("Noch keine GRUA festgelegt ~ 'NA' angenommen.")
        grua <- "NA"
      } 
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
      tmpdf$Elementname[.self$nObAsp.int + 1]   <- "dummy"
      tmpdf$Elementname[.self$nObAsp.int + 2]   <- "AICE"
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
      if(!grepl(grua, fPath)) {
        aStr <- substr(fPath, 1, nchar(fPath)-4)
        fPath <-  paste0(aStr,"_",grua,".csv")
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
      AnzSepas <- .self$getSepaCnt()
      nDez <- nchar(toString(AnzSepas))
      if (AnzSepas >= 1) {
        for (i in 1:AnzSepas) {
          print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          print(paste0("Sepa Nr.: ", i, "/",AnzSepas, " |"))
          print(strrep("~",13 + nchar(toString(i)) + nDez))
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

