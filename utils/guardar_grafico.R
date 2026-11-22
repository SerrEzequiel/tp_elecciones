# ------------------------------------------------------------------------------
# guardar_grafico()
# Exporta un gráfico de ggplot a output/graficos/ con los criterios de la
# materia: ggsave(), JPG, 300 dpi y unos 22 cm de ancho.
# ------------------------------------------------------------------------------

guardar_grafico <- function(grafico, nombre_archivo, ancho = 22, alto = 14) {
  ggplot2::ggsave(
    filename = file.path("output", "graficos", nombre_archivo),
    plot     = grafico,
    width    = ancho,
    height   = alto,
    units    = "cm",
    dpi      = 300,
    bg       = "white",
    device   = grDevices::jpeg,
    quality  = 95
  )
}
