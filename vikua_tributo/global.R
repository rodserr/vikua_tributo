library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(shinymanager)
library(bigrquery)

credentials <- data.frame(
  user = c("admin", "user"), # mandatory
  password = c("admin", "user"), # mandatory
  admin = c(T, F),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# bq_auth(path = ".secrets/reporting-338116-67ffb2788635.json")
# .query <- "
# select
# lineas_de_factura_empresa_mostrar_nombre,
# registro_de_informacion_fiscal_rif, 
# lat,
# lon,
# actividad_econ,
# importe_usd,
# estado
# from `reporting-338116.sotillo.tributo_odoo`
# limit 1000
# "
# act_econ_features <- bq_project_query('reporting-338116',.query) %>% bq_table_download()

act_econ_features <- read_csv('act_econ_features.csv')

contribuyente_features <- act_econ_features %>% 
  group_by(lineas_de_factura_empresa_mostrar_nombre, registro_de_informacion_fiscal_rif,
           lat, lon, actividad_econ, estado) %>% 
  summarise(
    importe_usd = sum(importe_usd, na.rm = T),
    .groups= 'drop'
  ) %>% 
  pivot_wider(names_from = estado, values_from = importe_usd) %>% 
  rename(pendiente = Abierto, pagado = Pagado) %>% 
  mutate(
    across(c('pendiente', 'pagado'), ~replace_na(.x, 0)),
    importe_usd = pendiente + pagado,
    pendiente_pct = pendiente/importe_usd
  )

monthly_importe <- act_econ_features %>% 
  group_by(lineas_de_factura_empresa_mostrar_nombre, lat, lon, actividad_econ,
           periodo_date = periodo %>% str_extract('.*(?= al )') %>% lubridate::dmy()) %>% 
  summarise(importe_usd = sum(importe_usd), .groups = 'drop') %>% 
  group_by(lineas_de_factura_empresa_mostrar_nombre) %>% 
  arrange(lineas_de_factura_empresa_mostrar_nombre, periodo_date) %>% 
  mutate(cambio_pct = importe_usd/lag(importe_usd)-1)

map_variables <- c(
  'Importe USD' = 'importe_usd',
  'Pagado' = 'pagado',
  'Pendiente' = 'pendiente',
  "Actividad Economica" = 'act_econ')

act_econ_list <- unique(contribuyente_features$actividad_econ)
empresa_list <- unique(contribuyente_features$lineas_de_factura_empresa_mostrar_nombre)

# Precalculate the breaks we'll need for the two histograms
centileBreaks <- hist(plot = FALSE, contribuyente_features$importe_usd, breaks = 20)$breaks


# helpers----
# Show a popup at the given location
showContribuyentePopup <- function(rif, lat, lng) {
  
  selectedRIF <- contribuyente_features[contribuyente_features$registro_de_informacion_fiscal_rif == rif,]
  
  content <- as.character(tagList(
    tags$h4("Importe USD:", round(selectedRIF$importe_usd, 0)),
    tags$strong(HTML(sprintf("%s, %s %s",
                             selectedRIF$lineas_de_factura_empresa_mostrar_nombre,
                             selectedRIF$registro_de_informacion_fiscal_rif, 
                             selectedRIF$actividad_econ
    ))), tags$br(),
    sprintf("Percent of Pendiente: %s%%", round(selectedRIF$pendiente_pct*100, 1)), tags$br()
  ))
  
  leafletProxy("map") %>% addPopups(lng, lat, content)
}