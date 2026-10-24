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
## Loader
com_load_ktgs <- function(.self, pathMetaDat) {
  .self$ElHptKTG.df <- read.table(paste0(pathMetaDat,"/HauptKTG.csv"),
                                  colClasses = c("integer", "character", "character", "character", "character"),
                                  header=TRUE,
                                  dec=",",
                                  sep=";",
                                  fill=TRUE,
                                  fileEncoding = STD_FILE_ENCODING)
  for (grua in GSLABS) {
    .self$ElSndKtgList.df[[grua]] <- read.table(paste0(pathMetaDat,"/SonderKTG_",grua,".csv"),
                                                 colClasses = c("integer", "character", "character", "character", "character"),
                                                 header=TRUE,
                                                 dec=",",
                                                 sep=";",
                                                 fill=TRUE,
                                                 fileEncoding = STD_FILE_ENCODING)
  }
}
#-------------------------------------------------------------------------------
## Getter fue AIce und ASel
# getHauptkatalog:
com_getHptKat = function() {
  return(.self$ElHptKTG.df)
}  
# getSonderkatalog:
com_getSndKat = function(grua = NA_character_, SILENT = TRUE) {
  if (!meta_check_grua(grua, TYPE = GSLABS)) {
    if (!SILENT) {
      warning("Keine gueltige GRUA uebergeben: 'bb', 'ei', 'ub', 'lf', 'gf', 'sf' ~ 'NULL' zurueckgegeben.")
    }
    return(NULL)
  }
  return(.self$ElSndKtgList.df[[grua]])
}