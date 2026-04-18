load("entorno_procesado.RData")
library(dplyr)
library(stringr)

test_df <- career_summary_jugadoras_df %>%
  filter(str_detect(equipo, "Rabotn|Работнички|Работницки")) %>%
  select(id, competicion_nombre, equipo) %>%
  head(5)
print(test_df$equipo)

print("Check how it's mapped in entidades_maestro_df:")
print(entidades_maestro_df %>% filter(str_detect(original_name, "Rabotn|Работнички|Работницки")) %>% select(original_name, translated_name_mk, translated_name_sq))
