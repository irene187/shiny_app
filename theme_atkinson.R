#trying to make grafics


my_set_theme <- function(familia = "Atkinson Hyperlegible", mida = 10, angle_text_x, angle_text_y){
  
 
  
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


make_base_plot <- function(taula, 
                           x, 
                           y, 
                           fill, 
                           color, 
                           group, 
                           data_id,
                           title, 
                           title.axis.x, 
                           title.axis.y, 
                           angle_text_x, 
                           angle_text_y, 
                           facet_horitzontal,
                           facet_vertical, 
                           facet_interactive_horitzontal,
                           facet_interactive_vertical,
                           nrow_facet_wrap,
                           ncol_facet_wrap,
                           scales_facet_wrap
){
  
  
  ## Transform Tipificacio into rlang object
  
  x <- rlang::parse_expr(x)
  y <- rlang::parse_expr(y)
  
  
  base_plot <- ggplot(taula, 
                      aes(
                        x = {{x}}, 
                        y = {{y}})
  )+
    ggplot2::labs(
      title = title,
      x = title.axis.x,
      y = title.axis.y
    )+
    my_set_theme("Atkinson Hyperlegible", 10, angle_text_x, angle_text_y)
  
  #### Optional aesthetic ####
  
  # Add fill aesthetic if specified
  if (!is.na(fill)) {
    base_plot <- base_plot + aes(fill = fct_rev(get(fill)))
  }
  
  # Add color aesthetic if specified
  if (!is.na(color)) {
    base_plot <- base_plot + aes(color = fct_rev(get(color)))
  }
  
  # Add group aesthetic if specified
  if (!is.na(group)) {
    base_plot <- base_plot + aes(group = get(group))
  } 
  
  # Add data_id aesthetic if specified (per interactive plot)
  if (!is.na(data_id)) {
    base_plot <- base_plot + aes(data_id = get(data_id))
  } 
  
  
  #### Optional facet_wrap ####
  # Add facet_horitzontal
  if (!is.na(facet_horitzontal) & is.na(facet_vertical)) {
    base_plot <- base_plot + 
      facet_wrap(as.formula(paste(". ~", facet_horitzontal)),  nrow = nrow_facet_wrap, ncol = ncol_facet_wrap, scales= scales_facet_wrap)
    
  } 
  
  # Add facet_vertical
  if (is.na(facet_horitzontal) & !is.na(facet_vertical)) {
    base_plot <- base_plot + 
      facet_wrap(as.formula(paste("~ .", facet_vertical)), nrow = nrow_facet_wrap, ncol = ncol_facet_wrap, scales= scales_facet_wrap)
    
  } 
  
  # Add facet_wrap with both
  if (!is.na(facet_horitzontal) & !is.na(facet_vertical)) {
    base_plot <- base_plot + 
      facet_wrap(as.formula(paste(facet_vertical, "~", facet_horitzontal)), nrow = nrow_facet_wrap, ncol = ncol_facet_wrap, scales= scales_facet_wrap)
    
  } 
  
  
  #### Optional interactive facet_wrap ####
  # Add facet_horitzontal
  if (!is.na(facet_interactive_horitzontal) & is.na(facet_interactive_vertical)) {
    base_plot <- base_plot + 
      facet_wrap_interactive(as.formula(paste(". ~", facet_interactive_horitzontal)),  nrow = nrow_facet_wrap, ncol = ncol_facet_wrap, scales= scales_facet_wrap)
    
  } 
  
  # Add facet_vertical
  if (is.na(facet_interactive_horitzontal) & !is.na(facet_interactive_vertical)) {
    base_plot <- base_plot + 
      facet_wrap_interactive(as.formula(paste("~ .", facet_interactive_vertical)), nrow = nrow_facet_wrap, ncol = ncol_facet_wrap, scales= scales_facet_wrap)
    
  } 
  
  # Add facet_wrap with both
  if (!is.na(facet_interactive_horitzontal) & !is.na(facet_interactive_vertical)) {
    base_plot <- base_plot + 
      facet_wrap_interactive(as.formula(paste(facet_interactive_vertical, "~", facet_horitzontal)), nrow = nrow_facet_wrap, ncol = ncol_facet_wrap, scales= scales_facet_wrap)
    
  } 
  
  
  return(base_plot)
  
}


stacked_bar_plot <- function(myplot){
  
  
  myplot <- myplot +  
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip()
  
  return(myplot)
  
}

