library(haven) # spss amb la funcio  read_sav per obrir pq manté les etiquetes


# Creem una funció que llegeix les dades, les diferents extensions contemplades son: csv, rds, sav

llegir_dades(fitxer) <-  tryCatch({  # Added tryCatch for error handling
              switch(ext,
              csv = read.csv(file$datapath),
              rds = readRDS(file$datapath),
              sav = read_sav(file$datapath),
              stop("Invalid file; Please upload a .csv or .rds or a .sav file")
  )}, error = function(e) {
    showNotification("Error reading file: Please upload a valid .csv or .rds or .sav file", type = "error de lectura de dades")  # Added notification for errors
    return(NULL)
  })



data <- llegir_dades("fitxer")  


View(data)