library(leaflet)
vars <- c("Asian %", "Black %", "Hispanic %", "White %", "Students with Disabilities %", "English Language Learners %", "Free or Reduced Lunch %", "Economic Need Index")

fluidPage(

    fluidRow(column(6, align="center",
                    h4("Select a School District"))),
    fluidRow(column(6,leafletOutput("map")),
             column(3,align="center",
                    radioButtons("level","School Level", c("Elementary","Middle"),inline=T)),
             column(3,align="center",
                    selectizeInput("var","Select a Variable",vars,selected="Free or Reduced Lunch %")),
             column(6, tabsetPanel(type="tabs",
                                   tabPanel("Plot", plotOutput("plot")),
                                   tabPanel("Table", DT::dataTableOutput("table"))))),
    tags$head(tags$style("#map{height:94vh !important;}")),
    tags$head(tags$style("#plot{height:75vh !important;}")),
    tags$head(tags$style("#table{height:75vh !important;}"))
)
