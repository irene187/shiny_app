# Crea una taula d'estadístics descriptius per una variable numèrica

taula_estad <- function(var_num){
  
  # Verifica que la variable és numèrica
  if(!is.numeric(var_num)) {
    stop("La variable ha de ser numèrica.")
  }
  
  # Calcular els estadístics
  estadistics <- data.frame(
    estadistic = c("Mitjana", "Variança", "dt", "Q1", "Q2", "Q3", "Màxim", "Mínim", "N", "NA", "Coeficient variació"),
    valor = c(
      mean(var_num, na.rm = TRUE),
      var(var_num, na.rm = TRUE),
      sd(var_num, na.rm = TRUE),
      quantile(var_num, 0.25, na.rm = TRUE),
      quantile(var_num, 0.50, na.rm = TRUE),
      quantile(var_num, 0.75, na.rm = TRUE),
      max(var_num, na.rm = TRUE),
      min(var_num, na.rm = TRUE),
      sum(!is.na(var_num)),
      sum(is.na(var_num)),
      (100 * sd(var_num, na.rm = TRUE)/mean(var_num, na.rm = TRUE)) 
    )
  )
  
  return(estadistics)
}


