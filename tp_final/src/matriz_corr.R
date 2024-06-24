library(GGally)

base <- data.frame(df$lh_subcx_hippocampus_volume, df$xh_general_etiv_volume, df$lh_cortex_superiorfrontal_thickness, df$lh_cortex_fusiform_volume)

# Cambiar los nombres de las variables en el dataframe base
colnames(base) <- c("Volumen del Hipocampo", "Volumen intracraneal", "Espesor de la corteza superior frontal", "Volumen de la corteza fusiforme")

# Función para agregar línea ajustada con color personalizado
add_smooth <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +  # Puntos de dispersión en color "#027368"
    geom_smooth(method = "lm", se = FALSE, color = "#027368", ...)+  # Línea ajustada en color "#027368"
    scale_x_continuous(labels = scales::comma) +  # Formato de etiquetas x sin notación científica
    scale_y_continuous(labels = scales::comma)    # Formato de etiquetas y sin notación científica
  
  return(p)
}
options(scipen=999)

# Utilizar ggpairs con personalización
ggpairs(base,
        aes(color = NULL),  # Evitar la coloración automática para evitar confusión con la línea ajustada
        lower = list(continuous = add_smooth))  # Aplicar la función add_smooth en los gráficos de dispersión
