###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
# asp
#
################################################################################
# INT|DEZ|DAT-INPUTSTRING-KONVERTER #
#####################################
#
# splittet Input String an den '#' - Zeichen, und separiert nach "single" und "range" (':')
asp_converter_InpStrIntDezDat = function(InputStr = "", funcTypeCast) {
  #
  # etwaige Leerzeichen und Tabstopps entfernen:
  tmpStr <- com_helper_str_cleanST(InputStr)
  #
  # String an den '#'s aufsplitten:
  tmpVec <- strsplit(tmpStr, split="#")[[1]]
  #
  # Nach den verschiedenen Operatortypen sortieren:
  pos_ungl_range  <-  (grepl( ':', tmpVec) & grepl('<>', tmpVec) & regexpr('<>', tmpVec)[[1]] < regexpr(':', tmpVec)[[1]])
  pos_range       <-  (grepl( ':', tmpVec) & !pos_ungl_range)
  pos_ungl        <-  (grepl('<>', tmpVec) & !pos_ungl_range)
  pos_klgl        <-   grepl('<=', tmpVec)
  pos_grgl        <-   grepl('>=', tmpVec)
  # Ueberlapp vehindern:
  pos_kl     <-  (grepl('<' , tmpVec) & !pos_ungl_range & !pos_ungl & !pos_klgl)
  pos_gr     <-  (grepl('>' , tmpVec) & !pos_ungl_range & !pos_ungl & !pos_grgl)  
  pos_gl     <- !(pos_ungl_range | pos_range  | pos_ungl | pos_klgl | pos_grgl | pos_kl | pos_gr)
  #
  #
  indices <- 1:length(tmpVec)
  retList <- list()
  cind <- 0
  # <> ?:?
  for (i in which(pos_ungl_range)) {
    cind <- cind + 1
    tmpMinMax <- strsplit(str_eraseT(tmpVec[i], "<>"), split=":")[[1]]
    retList[[cind]] <- list(TYPE = "<>:",  KRITERIUM = c(MIN = funcTypeCast(tmpMinMax[1]),
                                                         MAX = funcTypeCast(tmpMinMax[2])))
  }
  # :
  for (i in which(pos_range)) {
    cind <- cind + 1
    tmpMinMax <- strsplit(tmpVec[i], split=":")[[1]]
    retList[[cind]] <- list(TYPE = ":",  KRITERIUM = c(MIN = funcTypeCast(tmpMinMax[1]),
                                                          MAX = funcTypeCast(tmpMinMax[2])))
  }
  # <>
  for (i in which(pos_ungl)) {
    cind <- cind + 1
    retList[[cind]] <- list(TYPE = "<>", KRITERIUM = funcTypeCast(str_eraseT(tmpVec[i], "<>")))
  }
  # >=
  for (i in which(pos_klgl)) {
    cind <- cind + 1
    retList[[cind]] <- list(TYPE = "<=", KRITERIUM = funcTypeCast(str_eraseT(tmpVec[i], "<=")))
  }
  # <=
  for (i in which(pos_grgl)) {
    cind <- cind + 1
    retList[[cind]] <- list(TYPE = ">=", KRITERIUM = funcTypeCast(str_eraseT(tmpVec[i], ">=")))
  }  
  # <
  for (i in which(pos_kl)) {
    cind <- cind + 1
    retList[[cind]] <- list(TYPE = "<", KRITERIUM = funcTypeCast(str_eraseT(tmpVec[i], "<")))
  }
  # >
  for (i in which(pos_gr)) {
    cind <- cind + 1
    retList[[cind]] <- list(TYPE = ">", KRITERIUM = funcTypeCast(str_eraseT(tmpVec[i], ">")))
  }
  # =
  for (i in which(pos_gl)) {
    cind <- cind + 1
    retList[[cind]] <- list(TYPE = "=", KRITERIUM = funcTypeCast(str_eraseT(tmpVec[i], "=")))
  }  
  return(retList)
}
#
#
################################################################################
# STRING--KONVERTER #
#####################
#-------------------------------------------------------------------------------
# STRING-INPUTSTRING-KONVERTER |
#------------------------------
#  Erlaubte InputStr - Formate:
#
#  1] "<Vergleichstext>"        
#     ~ wird 1-1 uebernommen ~ es wird genau nach "<Vergleichstext>" selektiert 
#   
#  2] "*<Token.1>* # *<Token.2>* # ... # *<Token.n>*"  oder  ("*<Token.1>*# *<Token.2>* # ... #*<Token.n>*")  etc.  
#     ~ Es wird via ODER (#) nach den "<Token.i>" als Teilstrings selektiert 
#     ~! Leerzeichen (oder anderer "Zeichenmuell") zwischen den * # , # * - Trennern werden ignoriert/entfernt  
#     ~ Bei fehlerhaftem Format wird NULL zurueckgegeben
#
asp_converter_InpStrSubStr = function(InputStr = "") {
  #
  StarPos <- gregexpr("\\*", InputStr)[[1]]
  #
  ## Vollstringkriterium, falls keine * vorhanden:
  ## ich erlaube Volltextsuche mit ODER-Verknüpfung
  if (StarPos[1] == -1) {
    # String an den '#'s aufsplitten:
    tmpVec <- strsplit(InputStr, split="#")[[1]]
    retList <- list()
    for (i in 1:length(tmpVec)) {
      retList[[i]] <- list(TYPE = "~", KRITERIUM = tmpVec[i])
    }      
    return(retList)
  }
  ## Nun ist klar, dass AnzStar > 0 gilt:
  AnzStar   <- length(StarPos)
  ## Anzahl der # bestimmen:
  RautePos   <- gregexpr("#", InputStr)[[1]]
  if (RautePos[1] == -1) { AnzRaute <- 0
  } else                 { AnzRaute <- length(RautePos) }
  #
  msg_ValidInput <- "Erlaubte Formate:\n
                      1] <Vergleichstext>\n
                      2] ???*<Token.1>*??? # ???*<Token.2>*??? # ... # ???*<Token.n>*???"
  #
  # Kriterium verwerfen, wenn AnzStar ungerade:
  if (AnzStar %% 2 == 1) {
    message(msg_ValidInput)
    warning("Kriterium verworfen - Anzahl der '*' ungerade.")
    return(NULL)
  }  
  ## Nun ist klar, dass AnzStar > 0 und gerade ist:
  # Kriterium verwerfen, wenn AnzStar/2 != AnzRaute + 1:
  if ((AnzStar/2) != AnzRaute + 1) {
    message(msg_ValidInput)
    warning("Kriterium verworfen - Anzahl der '*~*' und der '#' inkonsistent.")
    return(NULL) 
  }
  ## Der Fall mehrerer Tokens *~* - es stimmen bereits formal die Anzahlen an *~* und #. 
  # String an den '#'s aufsplitten:
  tmpVec <- strsplit(InputStr, split="#")[[1]]
  # Kriterium verwerfen, falls in einem Block keine 2 '*' zu finden sind:
  tmpPos <- gregexpr("\\*", tmpVec)
  #
  for (i in 1:length(tmpVec)) {
    if (length(tmpPos[[i]]) != 2) {
      message(msg_ValidInput)
      warning("Kriterium verworfen - Inkorrekte Verteilung der '*' und '#'.")  
      return(NULL)
    }
  }
  # Zeichenmuell entfernen:
  tmpVec <- str_resolveT(tmpVec)
  tokens <- tmpVec[which(tmpVec != "")]
  # Pruefen, ob etwas uebrig geblieben ist:
  if(length(tokens) == 0) {
    message(msg_ValidInput)
    warning("Kriterium verworfen - Teilkriterien '*~*' waren alle leer '**'.")
    return(NULL)
  }
  # Kriteriumsliste zurueckgeben:
  retList <- list()
  for (i in 1:length(tokens)) {
    retList[[i]] <- list(TYPE = "*~*", KRITERIUM = tokens[i])
  }
  return(retList)
} 
#
#-------------------------------------------------------------------------------
# NSTR-INPUTSTRING-KONVERTER |
#----------------------------
# Wie INT-KONVERTER:
#
asp_converter_InpStrNStr = function(InputStr = "") {
  return(asp_converter_InpStrIntDezDat(InputStr, as.integer))
}
#
#----
# #
# # 
# #-------------------------------------------------------------------------------
# # KSTR-INPUTSTRING-KONVERTER |
# #----------------------------
# # splittet Input String an den '#' - Zeichen auf (und loescht Leerzeichen und Tabstopps)
# asp_converter_InpStrNKStr = function(InputStr = "") {
#   #
#   # etwaige Leerzeichen und Tabstopps entfernen:
#   tmpStr <- com_helper_str_cleanST(InputStr)
#   #
#   # String an den '#'s aufsplitten:
#   tmpVec <- strsplit(tmpStr, split="#")[[1]]
#   #
#   retList <- list()
#   #
#   for (i in 1:length(tmpVec)) {
#     retList[[i]] <- list(TYPE = "", KRITERIUM = tmpVec[i]) 
#   }
#   return(retList)
# }
# #
#
#-------------------------------------------------------------------------------
# BOOL-INPUTSTRING-KONVERTER |
#---------------------------------
# splittet Input String an den '#' - Zeichen auf (und loescht Leerzeichen und Tabstopps)
asp_converter_InpStrBln = function(InputStr = "") {
  # etwaige Leerzeichen und Tabstopps entfernen:
  tmpStr <- com_helper_str_cleanST(InputStr)
  #
  return(list(list(TYPE = "", KRITERIUM = as.logical(as.integer(tmpStr)))))
}

