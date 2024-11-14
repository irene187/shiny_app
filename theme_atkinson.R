### Theme Atkinson


my_set_theme <- function(familia = "Atkinson Hyperlegible", mida = 10, angle_text_x, angle_text_y){
  
  install.packages("showtext")
  
  library(showtext)
  
  font_add_google("Atkinson Hyperlegible", "Atkinson Hyperlegible")
  showtext_auto()
  
  mytheme <- ggplot2::theme_minimal() + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family =familia, 
        size   = mida+5,
        face   = "bold", 
        color  = "black"),
      plot.subtitle = ggplot2::element_text(
        family = familia, 
        size   = mida+2, 
        color  = "black"),
      axis.title = ggplot2::element_text(
        family = familia, 
        size   = mida, 
        color  = "black"),
      axis.text = ggplot2::element_text(
        family = familia, 
        size   = mida,
        face   = "bold", 
        color  = "black"),
      axis.text.x = ggplot2::element_text(
        angle = angle_text_x, 
        color = "black"),
      axis.text.y = ggplot2::element_text(
        angle = angle_text_y, 
        color = "black"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = familia, 
        size   = mida-1, 
        color  = "black"),
      strip.text = ggplot2::element_text(
        family = familia, 
        size   = mida+2, 
        colour = "black"),
      strip.background = ggplot2::element_rect(
        colour   = "black", 
        fill     = "white", 
        #linewidth = 3.5, 
        linetype = "solid"),
      legend.position = "bottom"
    )
  return(mytheme)
}