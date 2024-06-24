df_nuevo <- df$edad
df_nuevo <- as.data.frame(df_nuevo)

df_nuevo$edades_grupos <- cut(df_nuevo$df_nuevo, breaks = c(20,30,40,50,60,70,80,90), right = TRUE)
df_nuevo$enfermedad <- df$enfermedad

library(dplyr)
library(ggplot2)

# Supongamos que tu dataframe se llama `data`
# Calcular la media de `enfermedad` para cada grupo etario
mean_enfermedad <- df_nuevo %>%
  group_by(edades_grupos) %>%
  summarize(media_enfermedad = mean(enfermedad, na.rm = TRUE))

ggplot(mean_enfermedad, aes(x = edades_grupos, y = media_enfermedad)) +
  geom_point(size = 3, color = "darkcyan") +
  labs(
    caption = "Proporción de enfermos según grupo etario",
    x = "Grupo Etario",
    y = "Proporción de Enfermos"
  ) +
  theme(plot.caption = element_text(hjust = 0.5))
