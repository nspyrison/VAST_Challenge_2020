
    gg <- 
      ## ggplot settings
      ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::xlim(x_min, x_max) +
      ggplot2::ylim(y_min, y_max) +
      ## Projected data points
      suppressWarnings( # Suppress for unused aes "frame".
        ggplot2::geom_point( 
          data = data_slides,
          shape = pch, color = col, fill = col, size = cex, alpha = alpha,
          mapping = ggplot2::aes(x = x, y = y, frame = slide)
        )
      )
    
    if (axes != "off"){
      gg <- gg +
        ## Circle path 
        ggplot2::geom_path(
          data = circ, color = "grey80", size = .3, inherit.aes = F,
          mapping = ggplot2::aes(x = x, y = y)
        ) +
        ## Basis axes segments
        suppressWarnings( # Suppress for unused aes "frame".
          ggplot2::geom_segment( 
            data = basis_slides, size = axes_siz, colour = axes_col,
            mapping = ggplot2::aes(x = x,
                                   y = y, 
                                   xend = zero[, 1], yend = zero[, 2], 
                                   frame = slide)
          )
        ) +
        ## Basis axes text labels
        suppressWarnings( # Suppress for unused aes "frame".
          ggrepel::geom_text_repel(
            data = basis_slides, 
            mapping = ggplot2::aes(x = x, y = y, 
                                   frame = slide, label = lab),
            colour = axes_col, size = 4, vjust = "outward", hjust = "outward")
        )
    }
    
    
    gg <- render_(...) + 
      ggplot2::coord_fixed()
    
    gganimate::transition_states(slide, 
                                 transition_length = 0, 
                                 state_length = 1 / fps)
    
  