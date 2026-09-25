###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
# com
#
#-------------------------------------------------------------------------------
## DataFrame - SpaltenTauscher
com_helper_df_mutate <- function(df, order) {
  if (ncol(df) < length(order)) {
     stop("Weniger Spalten im Dataframe als uebergebene Positionsparameter!")
  }
     rwnms <- rownames(df)
      cnms <- colnames(df)
   tmpcnms <- rep("",ncol(df))
     tmpdf <- data.frame(matrix(ncol = ncol(df), nrow = nrow(df))) 
  for (i in 1:length(order)) {
    tmpcnms[i] <- cnms[order[i]]
      tmpdf[i] <- df[order[i]]   
  }
  colnames(tmpdf) <- tmpcnms
  rownames(tmpdf) <- rwnms
  return(tmpdf)
}
df_mutate = com_helper_df_mutate   
#
#-------------------------------------------------------------------------------
## Stringumkehrer
com_helper_str_reverse = function(STR) {
  zeichen <- strsplit(STR, split = "")[[1]]
   revStr <- paste(rev(zeichen), collapse = "")
  return(revStr)
}
str_rev = com_helper_str_reverse
#
#-------------------------------------------------------------------------------
## String ~ Tokenlöscher
com_helper_str_erase_token <- function(Str, Tok) {
  return(gsub(pattern = Tok, replacement = "", x = Str))
}
str_eraseT = com_helper_str_erase_token  
#
#-------------------------------------------------------------------------------
## Positions-Splitter
com_helper_str_pos_split = function(STR, POS) {
  #
  POS <- POS[which((POS >= 1) & (POS <= nchar(STR)))]
  retVec <- rep("",length(POS) + 1)
  #
  retVec[1] <- substr(STR, 1, POS[1]-1)
  #
  for (i in 1:length(POS)) {
    retVec[i+1] <- substr(STR, POS[i]+1, POS[i+1]-1)
  }
  retVec[length(POS)+1] <- substr(STR, POS[length(POS)]+1, nchar(STR))
  return(retVec)
}
str_pos_splt = com_helper_str_pos_split
#
#-------------------------------------------------------------------------------
## String ~ Tokenlöscher
com_helper_str_erase_token <- function(Str, Tok) {
  return(gsub(pattern = Tok, replacement = "", x = Str))
}
str_eraseT = com_helper_str_erase_token  
#
#-------------------------------------------------------------------------------
## String ~ Leerzeichen und Tabstopp - Bereiniger
com_helper_str_cleanST <- function(Str) {
  # etwaige Leerzeichen entfernen:
  tmpStr <- str_eraseT(Str, " ")  #gsub(pattern = " ", replacement = "", x = Str)
  # etwaige Tabstopps entfernen:
  tmpStr <- str_eraseT(tmpStr, "\t") #gsub(pattern = "\t", replacement = "", x = tmpStr)
  #
  return(tmpStr)
}
str_clnST = com_helper_str_cleanST  
#
#-------------------------------------------------------------------------------
## String ~ *~*-Token - Aufloeser:     "...*~*..."   ->   "~" 
com_helper_str_resolve_token <- function(StrV) {
  posV    <- gregexpr("\\*", StrV)
  retStrV <- StrV 
  for (i in 1:length(StrV)) {
    if ((posV[[i]][length(posV[[i]])]-posV[[i]][1]) == 1) { 
      retStrV[i] <- ""
    } else {
      retStrV[i] <- substr(StrV[i], posV[[i]][1] + 1, posV[[i]][length(posV[[i]])] -1)
    }
  }
  return(retStrV)
}
str_resolveT = com_helper_str_resolve_token 





