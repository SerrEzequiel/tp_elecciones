# ------------------------------------------------------------------------------
# fuente_tp()
# Devuelve el texto de fuente que va al pie de todos los gráficos. Se le puede
# sumar una aclaración propia de cada gráfico.
# ------------------------------------------------------------------------------

fuente_tp <- function(aclaracion = NULL) {
  texto <- paste0(
    "Fuente: elaboración propia en base a la Dirección Nacional Electoral (DINE): ",
    "escrutinio definitivo 2019 (compilado por electorAr) y provisorio 2023.\n",
    "Elecciones generales, cargo presidente, primera vuelta. Porcentajes sobre votos positivos."
  )
  if (!is.null(aclaracion)) texto <- paste0(texto, "\n", aclaracion)
  texto
}
