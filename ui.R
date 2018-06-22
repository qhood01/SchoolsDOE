vars <- c("Asian %", "Black %", "Hispanic %", "Other %", "White %", "Students with Disabilities %", "English Language Learners %", "Poverty %")

fluidPage(
    br(),
    column(6,leafletOutput("map", height="750px")),
    column(6,radioButtons("level","School Level", c("Elementary","Middle"),inline=T),
           selectizeInput("var","Select a Variable",vars),
           plotOutput("plot", height="600px")),
    br()
)