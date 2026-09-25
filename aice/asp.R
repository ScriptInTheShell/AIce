###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
# -------------------------                          
# AKS-SELEKTIONSPARAMETER |
#-------------------------
# Version:      1.0.0-beta
# Author:       Dr. Maximilian Hanusch
# Maintainer:   Dr. Maximilian Hanusch  ~  "maximilian.hanusch@lkprignitz.de"
#
###########################################################################################################################
#
source("aice/functions/com_func/func_com_meta.R")
source("aice/functions/asp_func/func_asp_meta.R")
#
###########################################################################################################################
#  ERLAEUTERUNGEN #
###########################################################################################################################
#
###########
# FELDER: #
###########
#
#------------
# Meta.lst  |        ~ list(ID.lst = list(vID = c(ID_1,...,ID_n), vKB = c(KB_1,...,KB_n)), SPT = char, PrtSTR = char, blnMTK = logical, chrMTK = char, blnINV = logical, SepaSTR = char) 
#------------
#
# blnINV: Soll das Nicht-Meta-Kriterium vor dem ODERn mit dem Metakriterium invertiert werden? 
#                      ----                   ---
# blnMTK: blnMeTaKriterium
# blnMTK:   TRUE    <=>  PrtSTR enthaelt '!','=' ("ist belegt", "nicht belegt")         (Metakriterium)
#                    ->  KritList.lst ~ 1elementige Liste:        list('TYPE = !'/'0') 
#           FALSE   <=>  PrtSTR enthaelt keine Metakriterium
#                    ->  KritList.lst ~ entsprechend befuellt   
#
# BEACHTE: 
# -------
# [blnMTK] *Falls blnMTK=TRUE, dann kann nur im Falle '=' length(KritList.lst) > 1 sein.
#          *Per addSepa() kann man bspw. setzen:   addSepa(...,"= # <weitere #-Kriterien>",...)              
#             (die AKS selbst erlaubt sowas nicht)
#          *addSepa(...,"! # <weitere #-Kriterien>",...) toetet einfach den " # <weitere #-Kriterien>" - Teilstring
#         
#          *Setzt man blnINV = TRUE, so wird NUR die Selektionslogik des NICHT-META-Teils von KritList.lst negiert. 
#          *die Selektonslogik vom Metateil wird dann ran-geODERt
#
# [blnINV] *Im Nachgang setzen via <asp>$setInv(IF); oder uber den zugeh. ASEL via "setSepaInv()"
#          *Per asel~addSepa() kann man bspw. uebergeben:   
#                     addSepa(...,"= # <>(<weitere #-Kriterien>)",...)              
#                     (die AKS selbst erlaubt sowas nicht)
#          *In metamorph wird dann "<>(<weitere #-Kriterien>)" durch "<weitere #-Kriterien>" ersetzt; 
#           und das INV-Flag wird gesetzt an asel~addSepa() gegeben, welches das FLAG dann das blnINV - Flag 
#           in Meta.lst setzt
#
# ANMERKUNGEN blnINV:
# ------------------
#   [A] Die AKS erlaubt im Kontext int|dez|dat (siehe unten) den Operator
#
#                           <>wert1:wert2
#
#       der dann eben auch in den exportierten Selektionsansaetzen auftreten kann.
#       Daher wurde dieser als zusaetzlicher Operator (neben ':', etc.) eigefuehrt.
#
#       Die Selektion mit dem NEUEN Operator:
#
#                   <>(wert1:wert2) 
#
#       liefert das gleiche Ergebniss; aber es ist <>() vielfaeltiger auch in der Form 
#
#                       <>(... #...#...#...) 
#
#       und auch auf Strings anwendbar:  
#
#                       <>(*SW23* # *WEITERES* )
#
#   [B] Der blnINV-"Operator" ignoriert die rangeODERten Metazeichen '!','=', wirkt also nur auf den NICHT-METATEIL:
#
#             bnlINV(!/= # AUSDRUCK)    ->   !/= # blnINV(AUSDRUCK)      
#
#
# LOGIKREGELN:                    !NA = NA
# -----------     xor(TRUE|FALSE, NA) = NA  - tritt bei Inversion auf
#
#           \  | NA  \  & NA
#    -------\--------\-------
#    TRUE   \  TRUE  \   NA
#    FALSE  \   NA   \  FALSE
#
# BEACHTE:  
#     [A]   der Fall "TRUE | NA = TRUE" kann nur innerhalb eines asp-Objektes auftreten
#           naemlich im Fall 
#                 "= # <Wert>", wobei dann  '=' ~ 'TRUE'  und  <Wert> ~ 'NA'  gilt, was dann auch genau so gewollt ist!!!
#           denn es wird 
#                 "! # <Wert>" von metamorph() in asel.rc zu "!" konvertiert
#
#
#     [B]   der Fall "FALSE & NA = FALSE" kann nur bei der Verknuepfung der - asp-Objekte in asel.rc auftreten
#           naemlich im Fall wo die Selektion 
#                   bzgl. (mindestens) eines Merkmals 'FALSE' ergibt
#                                      sowie
#                   bzgl. (mindestens) eines Merkmals 'NA' ergibt ~ Wert nicht belegt
#     
#
# PERIODEN:  
#   Wird nach PERIODENHAUPTNUMMER <PHN> selektiert, so gilt:
#   !:  TRUE, falls mind. eine Unterperiode belegt  
#   =:  TRUE, falls keine Unterperiode belegt
#
#   Wird nach PERIODENHAUPTNUMMER <PHN> selektiert, so gilt:
#     <Bedingung> TRUE, falls <Bedingung> fuer mind. eine Unterperiode TRUE  
#
#-------------------------------------------
# KritList.lst  fuer Nicht-Meta-Kriterien  |
#-------------------------------------------
#------------------
# INT | DEZ | DAT |   DAT(Datum), DEZ(Dezimal), KINT(Kategorial-INT), NINT(Numerisch-INT) JINT((Bau)JAHR-INT)            
#-----------------
# KritList.lst ~ Liste mit Elementen der Form:
#                  list(TYPE = ':' / '<>:',  KRITERIUM = c(MIN = as.integer | as.date (minWert),
#                                                          MAX = as.integer | as.date (maxWert)))
#                  list(TYPE = <OP>, KRITERIUM = Wert)
#                          mit <OP> in '<>','<=','>=','<','>','=' 
#
# Anmerkung zu '<>:'   ~   bspw.  <>01.01.2024:07.09.2025
# 
#
# INT spezialisiert zu:
#     ~ KINT - kategorial INT
#     ~ NINT - numerisch  INT
#     ~ JINT - Jahr       INT
#
#-------------
# STR, FSTR  |            
#------------
# KritList.lst ~ Liste mit Elementen der Form:
#                  list(TYPE =  "~",  KRITERIUM = "<Voll-Vergleichsstring>"  -  normalerweise dann nur dieser eine Listeneintrag
#                  list(TYPE = "*~*", KRITERIUM = "Teil-Vergleichsstring" 
#                 
#-------------
# NSTR|KSTR  |
#------------
# KritList.lst ~ Liste mit Elementen der Form:
#                  list(TYPE =  "",  KRITERIUM = "<IDENTIFIER>"
#
#--------
# BOOL  |
#-------
# KritList.lst ~ Liste mit Elementen der Form:
#                  list(TYPE =  "",  KRITERIUM = TRUE | FALSE)
#
#
###########################################################################################################################
###########################################################################################################################
#--------------------------------------------------------------------------------------------------------------------------
#  AKS-SELEKTIONSPARAMETER.REFERENCE-CLASS |
#-------------------------------------------
#
asp.rc <- setRefClass(
  # Name der RC-Klasse
  "AKS_Selektionsparameter",
  #------------------------------------ 
  #  DATENFELDER  |     (typenlose Aufzaehlung)
  #---------------
  fields = c(
  ## Alle Daten im Ueberblick:
    Meta.lst         = "list",      # list(ID.lst = list(vID = c(ID_1,...,ID_n), vKB = c(KB_1,...,KB_n)), 
                                    #                    SPT = char, PrtSTR = char, blnMTK = logical, chrMTK = char, blnINV = logical, SepaSTR = char) 
  ## Selektionskriterium Rohdaten-Kopie (redundant):
    SelKrit.df       = "data.frame",
  ## Selektionskriterium aufbereitet:
    # Metakariterium, falls vorhanden:
    MetaKrit.lst     = "list",
    # Liste mit Teilkriterien ~ ODER - Verknuepft - NICHT-META-Anteil
    KritList.lst    = "list",
    # Anzahl der Teilkriterien
    nTeilKrits.int   = "integer"  
  ),
  #------------------------------------ 
  #  METHODEN:  |
  #-------------
  methods = list(
 #KONSTRUKTOR:
    initialize = function(iID.lst = list(as.integer(0),""), iSPT = "", iPrtSTR = "", iSepaSTR = "", iInvert = FALSE) {
      #
            .self$Meta.lst <- list()
        .self$MetaKrit.lst <- list() 
       .self$KritList.lst <- list()
      .self$nTeilKrits.int <- as.integer(0)
      #
      .self$convert(iID.lst, iPrtSTR, iSepaSTR, iSPT, iInvert)
    },
 #DESTRUKTOR:
    finalize = function() {
      # print("Aufruf des asp.rc Destruktors.")
    },
 #ELEMENTARE OPERATIONEN:
  # convert ~ SelektionsString etc. in ODER-Kriterienliste umwandeln:  
    convert = function(ID.lst, PrtSTR = "", SepaSTR = "", SPT = "", INVERT.log = FALSE) {
      # Meta.list befuellen | Wenn Metakriterium, dann zudem "MetaKrit.lst" setzen ("0ll-tes Teilkriterium") 
      SepaSTR <- .self$checkMeta(ID.lst, PrtSTR, SepaSTR, SPT, INVERT.log)
      # Sind noch weitere Kkriterien vorhanden ?
      if(SepaSTR != "") {
        #
        .self$KritList.lst <- append(.self$KritList.lst, 
                                      switch(SPT,
                                             "DAT"  = asp_converter_InpStrIntDezDat(SepaSTR, function(str) {as.Date(str, format = "%d.%m.%Y")} ),
                                             "DEZ"  = asp_converter_InpStrIntDezDat(SepaSTR, function(str) {as.numeric(gsub(",", ".", str))} ),
                                             "INT"  = asp_converter_InpStrIntDezDat(SepaSTR, as.integer),
                                             "KINT" = asp_converter_InpStrIntDezDat(SepaSTR, as.integer),
                                             "NINT" = asp_converter_InpStrIntDezDat(SepaSTR, as.integer),
                                             "JINT" = asp_converter_InpStrIntDezDat(SepaSTR, as.integer),
                                             "NSTR" = asp_converter_InpStrNStr(SepaSTR),
                                             "STR"  = asp_converter_InpStrSubStr(SepaSTR),
                                             "FSTR" = asp_converter_InpStrSubStr(SepaSTR),
                                             "KSTR" = asp_converter_InpStrSubStr(SepaSTR),
                                             "BOOL" = asp_converter_InpStrBln(SepaSTR)
                                      )
        )
      }
      if (is.null(.self$KritList.lst)) {
        warning("NULL-Kriterium in asp-extract aufgetreten (verworfen).")
      } else {
        .self$nTeilKrits.int <- length(.self$KritList.lst)
      }
    },
  # checkMeta - Meta.lst befuellen und pruefen, ob Meta-Krit "ist belegt" ('!'), "nicht belegt" ('=') gesetzt wurde 
  # Wenn ja, dann Teilkriterium entsprechend setzen
  # das "Invert.log"-Flag wird bereits durch metamorph gesetzt
  # Gibt den Meta - bereinigten Selektionsstring zurueck
    checkMeta = function(cID.lst, cPrtSTR, cSepaSTR, cSPT, cInvert) {
      # cSepaSTR and den '#' auseinanderreißen:
      tmpVec <- strsplit(cSepaSTR, split = "#")[[1]]
      isMetaKrit <- FALSE
      MetaStr    <- ""
      retStr     <- ""
      if (grepl("ist belegt", tmpVec[1])) {
                  isMetaKrit <- TRUE
                     MetaStr <- "ist belegt"
          .self$MetaKrit.lst <- list(list(TYPE = "!", KRITERIUM = "META ~ 'ist belegt'"))
      } else if (grepl("nicht belegt", tmpVec[1])) {
                  isMetaKrit <- TRUE
                     MetaStr <- "nicht belegt"
          .self$MetaKrit.lst <- list(list(TYPE = "=", KRITERIUM = "META ~ 'nicht belegt'"))
      }
      #
      if (!isMetaKrit) {
        retStr <- cSepaSTR
      } else {
        # retStr via '#' ohne das Metakrit - zusammentackern:
        blnFirst <- TRUE
        for (str in tmpVec[-c(1)]) {
          if (blnFirst) { retStr <- str 
          } else        { retStr <- paste0(retStr, "#", str) }
          #
          blnFirst <- FALSE
        }
      }
      # Meta.lst erschaffen:
      .self$Meta.lst <- list( ID.lst = cID.lst,
                                 SPT = cSPT,
                             PrtSTR  = cPrtSTR,
                             blnMTK  = isMetaKrit,
                             chrMTK  = MetaStr,
                             blnINV  = cInvert,
                             SepaSTR = retStr)
      return(retStr)
    },
  #SELEKTOR:
  # select - Die uebergebenen KF-Daten via .self$KritList.lst selektieren und die entsprechende Selektionsmaske zurueckgeben 
    select = function(dataFrame) {
        naList <- NULL
           SPT <- .self$Meta.lst$SPT
      # Hier dann mit ODER-Verknuepfung
      blnSepa <- rep(FALSE, times = nrow(dataFrame))
      blnMeta <- rep(FALSE, times = nrow(dataFrame))
      # Spaltenindizes in dataFrame der zu selektierenden Eigenschaften:
      vIndices <- c()
      vKBs <- .self$Meta.lst$ID.lst$vKB
      if (length(vKBs) > 1) {
        vKBs <- vKBs[2:length(vKBs)]
      }
      for (kb in vKBs) {
        vIndices <- c(vIndices, which(colnames(dataFrame) == kb))
      }
      #
      if (.self$Meta.lst$blnMTK) {
        if (.self$MetaKrit.lst[[1]]$TYPE == '!') {
          for (cind in vIndices) {
            blnMeta <- blnMeta | asp_selector_Meta(cind, '!', dataFrame)
          }
        } else if (.self$MetaKrit.lst[[1]]$TYPE == '=') {
          blnMeta <- rep(TRUE, times = nrow(dataFrame))
          for (cind in vIndices) {
            blnMeta <- blnMeta & asp_selector_Meta(cind, '=', dataFrame)
          }
        }
      }
      #(*)Hinweis:  Falls blnMeta = TRUE, dann kann nur im Falle '=' .self$nTeilKrits.int >= 1 sein, 
      #             da andernfalls ('!') 'asel.rc$metamorph()' den Rest wegschreddert
      #             daher muss das bei der Formel unten fuer blnRet auch nicht explizit unterschieden werden.
      #                                                                blnSepa ist per per Default 'FALSE^n'
      if (.self$nTeilKrits.int >= 1) {
        #
        for (i in 1:.self$nTeilKrits.int) {
          for (cind in vIndices) {
            blnSepa <- blnSepa | switch(SPT,
                                       "DAT" = asp_selector_IntDezDat(.self$KritList.lst[[i]], cind, dataFrame, function(str) {as.Date(str, format = "%d.%m.%Y")} ),
                                       "DEZ" = asp_selector_IntDezDat(.self$KritList.lst[[i]], cind, dataFrame, function(str) {as.numeric(gsub(",", ".", str))} ),
                                      "KINT" = asp_selector_IntDezDat(.self$KritList.lst[[i]], cind, dataFrame, as.integer),
                                      "NINT" = asp_selector_IntDezDat(.self$KritList.lst[[i]], cind, dataFrame, as.integer),
                                      "JINT" = asp_selector_IntDezDat(.self$KritList.lst[[i]], cind, dataFrame, as.integer),
                                      "NSTR" = asp_selector_NStr(.self$KritList.lst[[i]], cind, dataFrame),
                                       "STR" = asp_selector_Str(.self$KritList.lst[[i]], cind, dataFrame),
                                      "FSTR" = asp_selector_Str(.self$KritList.lst[[i]], cind, dataFrame),
                                      "KSTR" = asp_selector_Str(.self$KritList.lst[[i]], cind, dataFrame),
                                      "BOOL" = asp_selector_Bln(.self$KritList.lst[[i]], cind, dataFrame, as.logical)
                                      )
          }
          
        }
        if (!.self$Meta.lst$blnMTK) {
                 naList <-  subset(dataFrame, select = vIndices)
        }
      }
      # Wenn Inversionflag gesetzt, dann Selektion invertieren (NAs bleiben NAs).
      blnRet <- blnMeta | xor(.self$Meta.lst$blnINV, blnSepa)
      # siehe zudem (*)Hinweis oben. 
      #
      return(list(blnSELECT = blnRet, naLIST = naList))
    },
  #SETTER-METHODEN:
  # set Inversion - set-Inversionflag - soll nach der Selektion alles invertiert werden?
    setInv = function(IF = NA) {
      if(IF %in% c(TRUE, FALSE)) {
        .self$Meta.lst[["blnINV"]] <- IF
        message(paste0("Inversionsflag gesetzt: '", toString(IF), "'"))
      } else {
        warning("Ungueltiger Wert uebergeben ~ keine AKtion durchgefuehrt!")
      }
    },
  #HELPER-METHODEN:
  # print - Selektionsparameter in der Konsole ausgeben:
    prtAsp = function() {
      tmpdf <- data.frame(.self$Meta.lst)
      nPer <- length(.self$Meta.lst$ID.lst$vID)
      if (nPer > 1) {
        tmpdf[2:nPer,"blnINV"] <- FALSE
      }
      print(tmpdf)
      print("----------------------------------------------------------------------------------------")
      if(.self$.self$Meta.lst[["blnMTK"]]) {
        print(.self$MetaKrit.lst)
      }
      if(.self$nTeilKrits.int > 0) {
        print(.self$KritList.lst)
      }
    }
  )
)