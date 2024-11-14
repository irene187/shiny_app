
# Función de estadística descriptiva
estadisticas_descriptivas <- function(tabla) {
  # Seleccionamos solo las columnas numéricas
  datos_numericos <- tabla[sapply(tabla, is.numeric)]
  
  # Calculamos las estadísticas descriptivas para cada columna numérica
  resumen <- data.frame(
    Variable = names(datos_numericos),
    Media = sapply(datos_numericos, mean, na.rm = TRUE),
    Mediana = sapply(datos_numericos, median, na.rm = TRUE),
    Minimo = sapply(datos_numericos, min, na.rm = TRUE),
    Maximo = sapply(datos_numericos, max, na.rm = TRUE),
    DesviacionEstandar = sapply(datos_numericos, sd, na.rm = TRUE),
    PrimerCuartil = sapply(datos_numericos, quantile, probs = 0.25, na.rm = TRUE),
    TercerCuartil = sapply(datos_numericos, quantile, probs = 0.75, na.rm = TRUE),
    Conteo = sapply(datos_numericos, function(x) sum(!is.na(x)))
  )
  
  return(resumen)
}

# Ejemplo de uso
# Supongamos que tienes un data frame llamado 'mi_tabla'
# resultado <- estadisticas_descriptivas(mi_tabla)
# print(resultado)
