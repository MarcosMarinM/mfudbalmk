source("R/07_setup.R")
source("R/08_functions.R")
load("entorno_procesado.RData")

span_result <- entity_name_spans("\u0420\u0430\u0431\u043e\u0442\u043d\u0438\u0447\u043a\u0438")
print(as.character(span_result))
