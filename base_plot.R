make_base_plot <- function(taula, 
                           x, 
                           y, 
                           fill = NULL, 
                           color = NULL, 
                           group = NULL, 
                           data_id = NULL,
                           title = NULL, 
                           title.axis.x = NULL, 
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