# ------------------------------------------------------------------------------
# tema_tp()
# Tema común de ggplot para todos los gráficos del trabajo: fondo blanco,
# grilla suave, título en negrita y fuente al pie alineada a la izquierda.
# ------------------------------------------------------------------------------

tema_tp <- function(tamanio_base = 10.5) {
  ggplot2::theme_minimal(base_size = tamanio_base) +
    ggplot2::theme(
      plot.title            = ggplot2::element_text(face = "bold", size = tamanio_base + 4),
      plot.subtitle         = ggplot2::element_text(colour = "grey30", lineheight = 1.1,
                                                    margin = ggplot2::margin(b = 8)),
      plot.caption          = ggplot2::element_text(colour = "grey40", size = tamanio_base - 3,
                                                    hjust = 0, lineheight = 1.1),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor      = ggplot2::element_blank(),
      panel.grid.major      = ggplot2::element_line(colour = "grey92"),
      legend.position       = "bottom",
      plot.background       = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin           = ggplot2::margin(12, 14, 10, 12)
    )
}

