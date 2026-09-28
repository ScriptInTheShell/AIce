###########################################################################################################################
#
#  Copyright (c) 2026 Dr. Maximilian Hanusch / Landkreis Prignitz.
#  
#  All rights reserved. This software is not licensed for distribution, modification, 
#  or commercial use without explicit written permission from the author.
#
###########################################################################################################################
#
#------------------------------------------------------------------------------- 
#  LIBS:  |
#---------
# Hier werden zusätzlich benötigte Libraries eingebunden:
#
# NICHTS
#
################################################################################
#------------------------------------------------------------------------------- 
#  PATHS:  |
#----------
# Hier werden die wichtigsten Dateipfade festegelegt:
{
  # Aktueller Pfad  ->  Projektpfad
  m_PathProject <- getwd()
  # Datenverzeichniss:  Hauptverzeichnis, in dem die Daten (KF-Daten und Selektionsansätze) liegen:. 
  m_PathRootDat <- paste0(m_PathProject,"/aice/data")
  # Verzeichnis mit den Kauffalldaten:
  m_PathKaufDat <- paste0(m_PathRootDat,"/Kauffalldaten")
  # Verzeichnis mit den Selektionsansätzen:
  m_PathSelektAns <- paste0(m_PathRootDat,"/Selektionsansaetze")
  # Verzeichnis mit abgespeicherten selektierten Daten:
  m_PathSelektionen <- paste0(m_PathRootDat,"/Selektionen")
  # Verzeichnis mit den Metadaten:
  m_PathMetaDat <- paste0(m_PathProject,"/aice/meta")
  # Pfad zum Codeverzeichniss:
  m_PathAice<-  paste0(m_PathProject,"/aice")
}
#
################################################################################
#------------------------------------------------------------------------------- 
#  KONSTANTEN:  |
#---------------
#
  AKS_FILE_ENCODING <- "Windows-1252"
  STD_FILE_ENCODING <- "" # (UTF-8)
#
# GRUA-LABELS:
  # Hauptkategorien:
  GLABS  <- c("bb","ei","uf")
  # Untergkategorien von "uf":
  GUFLABS  <- c("ub","lf","gf","sf")
  # Kategorien aufgeschlüsselt:
  GSLABS <- c("bb","ei","ub","lf","gf","sf")
  # Alle Kategorien
  GALABS <- c(GLABS, GUFLABS)
  #
  meta_check_grua = function(label = "", TYPE = GALABS) return(label %in% TYPE)
#
#   
#
#
# ### !!!!!! Das hier als Vektor mit STD_... als Elementbezeichnung bzw. !!!!!!!!!!  als Faktor 
# 
# # #
# # SDT_INT     <- 0  # Integer
# # SDT_FLK     <- 1  # Fliesskomma
# # 
# # SDT_DATUM   <- 2  # Datum
# # SDT_KOORD   <- 3  # Koordinaten ~ c(x,y)


