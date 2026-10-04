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