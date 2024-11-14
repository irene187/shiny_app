library(haven) # spss amb la funcio  read_sav per obrir pq manté les etiquetes




# Abrir fichero en spss

llegir_dades <- if(extensio.arxiu="xls"
  switch(ext,
         csv = read.csv(file$datapath),
         rds = readRDS(file$datapath),
         rds = readRDS(file$datapath),
         stop("Invalid file; Please upload a .csv or .rds file")
  )



data <- read_sav("20240903_Encuesta relevo generacional_def.sav")  # manté majuscules i minusculas en el nom de les variables
#dataset<-readSPSS("20240903_Encuesta relevo generacional_def.sav") # si llegeixo la base d'aquesta manera tinc les etiquetes, tot minuscules


View(data)

library(Rcmdr)
library(data.table)
library("expss") # Llibreria semblant en codi al spss


llegir_dades <- ({  # Added tryCatch for error handling
  switch(ext,
         csv = read.csv(file$datapath),
         rds = readRDS(file$datapath),
         stop("Invalid file; Please upload a .csv or .rds file")
  )