
ui_raw <- navbarPage(
    "Vikua Tributo Dashboard", id="nav",
    # Interactive map----
    tabPanel("Mapa Interactivo",
             div(class="outer",
                 
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css"),
                   includeScript("gomap.js")
                 ),
                 
                 # If not using custom CSS, set height of leafletOutput to a number instead of percent
                 leafletOutput("map", width="100%", height="100%"),
                 
                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                 absolutePanel(
                   id = "controls", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                   width = 330, height = "auto",
                               
                   h2("Explorador de Variables"),
                               
                   selectInput(
                     "color_definition",
                     "Definir Color por:",
                     map_variables
                    ),
                   
                   # conditionalPanel(
                   #   "input.color == 'importe_usd'",
                   #   numericInput("color_importe", "Ultimos X Meses", .5)
                   #  ),
                   
                   conditionalPanel(
                     "input.color_definition == 'act_econ'",
                     selectInput("color_act_econ", "Resaltar Actividad Economica:", act_econ_list)
                   ),
                               
                               plotOutput("MapHistImporte", height = 200),
                               plotOutput("mapTopContribuyentes", height = 250)
                 ),
                 
                 tags$div(id="cite",
                          'Data compiled for ', tags$em('Guiriri - Alcaldia de Sotillo'), ' by Vikua, Inc.'
                 )
             )
    ),
    # Data Explorer-----
    tabPanel("Explorador de Datos",
             fluidRow(
               column(3, selectInput("actividad_economica", "Actividad Economica", act_econ_list, multiple=TRUE) ),
               column(3, textInput("empresa", "Empresa") )
             ),
             hr(),
             DT::dataTableOutput("contribuyente")
    ),
    
    conditionalPanel("false", icon("crosshair"))
)

# Log In-----
secure_app(
  ui_raw,
  id = "auth",
  # add image on top ?
  tags_top = tags$div(
    tags$h4("Demo", style = "align:center"),
    tags$img(
      src = "/Logo vikua ES.png", width = 100
    )
  ),
  # add information on bottom ?
  tags_bottom = tags$div(
    tags$p(
      "Cualquier inquietud, por favor contactar ",
      tags$a(
        href = "mailto: tech@vikua.com",
        target="_top", "administrator"
      )
    )
  ),
  # change auth ui background ?
  # https://developer.mozilla.org/fr/docs/Web/CSS/background
  background  = "linear-gradient(rgba(0, 0, 255, 0.5),
                       rgba(255, 255, 0, 0.5));", 
  # set language ?
  language = "es"
)