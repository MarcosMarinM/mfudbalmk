load("entorno_procesado.RData")
library(dplyr)
library(stringr)

test_df <- career_summary_jugadoras_df %>%
  filter(str_detect(equipo, "Rabotn|Работнички")) %>%
  select(id, competicion_nombre, equipo) %>%
  head(5)
print(test_df)
