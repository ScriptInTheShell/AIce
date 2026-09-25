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
#-------------------------------------------------------------------------------
# META-SELECTOR |
#---------------
#
asp_selector_Meta = function(cIndex, mFlag, dataFrame = NULL) {
  if (is.null(dataFrame)) {
    stop("Keine Daten uebergeben.") 
  }
  return(switch(mFlag, 
                '!' = !is.na(dataFrame[ ,cIndex]),
                '=' =  is.na(dataFrame[ ,cIndex])
                ))
}
#
#-------------------------------------------------------------------------------
# INT|DEZ|DAT-INPUT-STRING-SELECTOR |
#-----------------------------------
#
asp_selector_IntDezDat = function(Kriterium = NULL, cIndex, dataFrame = NULL, funcTypeCast) {
  if (is.null(dataFrame)) {
    stop("Keine Daten uebergeben.") 
  }
  if (is.null(Kriterium)) {
    stop("Kein Kriterium uebergeben.") 
  }
  #
  return(switch(Kriterium$TYPE,
                '='   =    #(!is.na(dataFrame[ ,cIndex])) & 
                                funcTypeCast(dataFrame[ ,cIndex]) == Kriterium$KRITERIUM,
                #
                ':'   =    #(!is.na(dataFrame[ ,cIndex])) &
                               (funcTypeCast(dataFrame[ ,cIndex]) >= Kriterium$KRITERIUM["MIN"]) &
                               (funcTypeCast(dataFrame[ ,cIndex]) <= Kriterium$KRITERIUM["MAX"]),
                #
                '<>:' =    #(!is.na(dataFrame[ ,cIndex])) &
                               (funcTypeCast(dataFrame[ ,cIndex]) <= Kriterium$KRITERIUM["MIN"]) 
                            |  (funcTypeCast(dataFrame[ ,cIndex]) >= Kriterium$KRITERIUM["MAX"]),
                #
                '<>'  =    #(!is.na(dataFrame[ ,cIndex])) &
                                funcTypeCast(dataFrame[ ,cIndex]) != Kriterium$KRITERIUM,
                #
                '<='  =    #(!is.na(dataFrame[ ,cIndex])) &
                                funcTypeCast(dataFrame[ ,cIndex]) <= Kriterium$KRITERIUM,
                #
                '>='  =    #(!is.na(dataFrame[ ,cIndex])) &
                                funcTypeCast(dataFrame[ ,cIndex]) >= Kriterium$KRITERIUM,
                #
                '<'   =    #(!is.na(dataFrame[ ,cIndex])) &
                                funcTypeCast(dataFrame[ ,cIndex]) <  Kriterium$KRITERIUM,
                #
                '>'   =    #(!is.na(dataFrame[ ,cIndex])) &
                                funcTypeCast(dataFrame[ ,cIndex]) >  Kriterium$KRITERIUM    
                ))
}
#
#-------------------------------------------------------------------------------
# STR-INPUT-STRING-SELECTOR |
#---------------------------
#
asp_selector_Str = function(Kriterium = NULL, cIndex, dataFrame = NULL) {
  if (is.null(dataFrame)) {
    stop("Keine Daten uebergeben.") 
  }
  if (is.null(Kriterium)) {
    stop("Kein Kriterium uebergeben.") 
  }
  #
  return(switch(Kriterium$TYPE,
                  "~" =   dataFrame[ ,cIndex] == Kriterium$KRITERIUM,
                "*~*" =   grepl(Kriterium$KRITERIUM, dataFrame[ ,cIndex])   
  ))  
}
# 
#-------------------------------------------------------------------------------
# NSTR-INPUTSTRING-SELEKTOR |
#---------------------------
# Wie INT-SELEKTOR:
#
asp_selector_NStr = function(Kriterium = NULL, cIndex, dataFrame = NULL) {
  if (is.null(dataFrame)) {
    stop("Keine Daten uebergeben.")
  }
  if (is.null(Kriterium)) {
    stop("Kein Kriterium uebergeben.")
  }
  #
  return(asp_selector_IntDezDat(Kriterium, cIndex, dataFrame, as.integer))
}
# 
#
# #-------------------------------------------------------------------------------
# # KSTR-INPUTSTRING-SELEKTOR |
# #---------------------------
# #
# asp_selector_NKStr = function(Kriterium, cIndex, dataFrame = NULL) {
#   if (is.null(dataFrame)) {
#     stop("Keine Daten uebergeben.") 
#   }
#   if (is.null(Kriterium)) {
#     stop("Kein Kriterium uebergeben.") 
#   }
#   #
#   return(paste(dataFrame[ ,cIndex]) == Kriterium$KRITERIUM)
# }
# #
#
#-------------------------------------------------------------------------------
# BOOL-INPUTSTRING-KONVERTER |
#---------------------------------
#
asp_selector_Bln = function(Kriterium = NULL, cIndex, dataFrame = NULL, funcTypeCast) {
  if (is.null(dataFrame)) {
    stop("Keine Daten uebergeben.") 
  }
  if (is.null(Kriterium)) {
    stop("Kein Kriterium uebergeben.") 
  }
return(as.logical(as.integer(dataFrame[ ,cIndex])) == Kriterium$KRITERIUM)
}
#




