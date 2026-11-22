# ------------------------------------------------------------------------------
# limpiar_nombre_lista()
# Lleva los nombres de las listas a un formato único: sin puntos, sin tildes,
# en mayúsculas y con los guiones espaciados igual. La misma lista aparece
# escrita de formas distintas según la provincia y la fuente, por ejemplo:
#   "Fernández.-.Fernández.(FRENTE.DE.TODOS)"
#   "Fernández - Fernández De Kirchner (Frente De Todos)"
# ------------------------------------------------------------------------------

limpiar_nombre_lista <- function(x) {
  x |>
    stringr::str_replace_all("\\.", " ") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("\\s*-\\s*", " - ") |>
    stringr::str_squish()
}
