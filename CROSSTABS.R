fes_summary <- function(dades, variable){
  # Verifica que la variable per la taula sigui un factor
  if(is.factor(dades[[variable]])){
    # crea una taula de freqüències relatives
    summary(dades$variable)
      } else {
    # Si la variable no és factor, atura i explica
    stop("L'argument 'variable' ha de ser un factor")
  }
}