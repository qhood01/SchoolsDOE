library(leaflet)
vars <- c("Asian %", "Black %", "Hispanic %", "White %", "Students with Disabilities %", "English Language Learners %", "Poverty %")

fluidPage(

    fluidRow(column(6, align="center",
                    h4("Select a School District"))),
    fluidRow(column(6,leafletOutput("map")),
             column(6,radioButtons("level","School Level", c("Elementary","Middle"),inline=T),
                    selectizeInput("var","Select a Variable",vars,selected="Poverty %"),
                    plotOutput("plot")),
             tags$head(tags$style("#plot{height:75vh !important;}")),
             tags$head(tags$style("#map{height:94vh !important;}")))
)
