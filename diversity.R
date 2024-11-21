

# Entropia negativa de Shannon
# Max: log(nombre_de_grups)

entropy <- function(x){
  
  # La variable ha de ser qualitativa
  if(is.character(x) | is.factor(x)) {
    
    # Calcula la diversitat de Shannon
    groups <- unique(x)
    div_k <- c()
    for(k in seq_along(groups)){
      
      pr_k <- sum(x==groups[k]) / length(x)
      div_k[k] <- (pr_k * log(pr_k))
      
    }
    
    div <- -1 * sum(div_k)
    return(div)
    
  } else {
    
    # Atura
    stop("La variable ha de ser qualitativa (character or factor)")
    
  }
  
}

# Check
# entropy(c("a", "a", "b", "b"))
# entropy(c(1, 1, 2, 2))
# entropy(c("1", "1", "2", "2"))
