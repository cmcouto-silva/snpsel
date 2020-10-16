#' @title Make a Tri Venn Diagram Plot
#' @param data List. A list with values as input.
#' @param n_cat Integer. Indicates the number of categories (i.e. circles) to be drawn.
#' @param internal_label Character scalar. Either "count", "percent" of "label" (both) for internal labels. 
#' Default set to "label" (i.e. both).
#' @param external_label Character vector. Main group names (default set to names of the list).
#' @param internal_annot_size Integer. Size for the internal labels.
#' @param external_annot_size Integer. Size for the external labels (main group names).
#' @param color Character scalar. Contour color.
#' @param fill Character vector. Circle colors.
#' @param size Integer. Contour line size.
#' @param alpha Numeric. Alpha value to circle colors. 
#' @import ggplot2
#' @return Venn Diagram Plot
#' @author Cainã Max Couto-Silva
#' @export

ggven_plot <- function(data, n_cat = 3, internal_label = "label", external_label = names(data),
                       internal_annot_size = 5, external_annot_size = 5,
                       color = "black", fill = NULL,
                       size = 1, alpha = 0.25) {
  
  if(!n_cat %in% 2:3) {
    stop("Only 2 and 3 categories have been implemented so far.")
  }
  
  if(n_cat == 2) {
    
    if(is.null(fill)) fill <- c("#699BFC", "#F5736E")
    
    vendata <- data.frame (
      x = c(-1, 1),
      y = c(1, 1), 
      cat = factor(external_label)
    )
    
    p <- ggplot(vendata, aes(x0 = x , y0 = y, r = 1.5, fill = cat)) + 
      ggforce::geom_circle(alpha = alpha, size = size, color = color, show.legend = FALSE) +
      scale_fill_manual(values = fill)
    
    annot_df <- ggVennDiagram::ggVennDiagram(data)$layers[[3]]$data
    annot_df$x <- c(A=-1.5, AB=0, B=1.5)
    annot_df$y <- c(A=1, AB=1, B=1)
    
    method_df <- data.frame (
      label = external_label,
      x = c(-1, 1),
      y = c(2.7, 2.7)
    )
    
    p <- p + 
      annotate(geom = "text", x = annot_df$x, y = annot_df$y, label = annot_df[[internal_label]], size = internal_annot_size) +
      annotate(geom = "text", x = method_df$x, y = method_df$y, label = method_df$label, size = external_annot_size, fontface = "bold") +
      coord_fixed() +
      theme_void()
    
  } else {
    
    if(is.null(fill)) fill <- c("#699BFC", "#22BB44", "#F5736E")
    
    vendata <- data.frame (
      x = c(0, -1, 1),
      y = c(2.5, 1, 1), 
      cat = factor(external_label)
    )
    
    p <- ggplot(vendata, aes(x0 = x , y0 = y, r = 1.5, fill = cat)) + 
      ggforce::geom_circle(alpha = alpha, size = size, color = color, show.legend = FALSE) +
      scale_fill_manual(values = fill)
    
    annot_df <- ggVennDiagram::ggVennDiagram(data)$layers[[3]]$data
    annot_df$x <- c(A=0, AB=-0.75, ABC=0, AC=0.75, B=-1.5, BC=0, C=1.5)
    annot_df$y <- c(A=3, AB=2, ABC=1.5, AC=2, B=0.75, BC=0.65, C=0.75)
    
    method_df <- data.frame (
      label = external_label,
      x = c(0, -1, 1),
      y = c(4.2, -0.7, -0.7)
    )
    
    p + 
      annotate(geom = "text", x = annot_df$x, y = annot_df$y, label = annot_df[[internal_label]], size = internal_annot_size) +
      annotate(geom = "text", x = method_df$x, y = method_df$y, label = method_df$label, size = external_annot_size, fontface = "bold") +
      coord_fixed() +
      theme_void()
  }
  
  return(p)

}
