###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
# asel
#
asel_helper_check_valid_sepa = function(ID, KB, SelaStr, SPT, INV) {
  #
  if (is.na(ID) | is.na(KB) | (com_helper_str_cleanST(SelaStr) == "") | (SPT == "") | is.na(SPT) | is.na(ID)) {
    warning(paste0("Uebergabeparameter nicht komplett: ",
                   " | ID: ",      toString(ID),
                   " | KB: ",      KB,
                   " | SelaSTR: ", SelaStr,
                   " | SPT: ",     SPT,
                   " | INV: ",     INV,
                   " |",
                   "\n",
                   "Kein Selektionsparameter festgelegt!"))
    return(FALSE)
  } else if (!(SPT %in% c("DAT", "KINT", "NINT", "JINT", "DEZ", "STR", "FSTR", "NSTR", "KSTR", "BOOL"))) {
    warning(paste0("func_asel_add_Sela.R: Ungueltiger SPT aufgetreten: ", toString(ID), " - ", SPT, " ~ SEPA verworfen."))
    return(FALSE)
  } else {
    return(TRUE)
  }
}