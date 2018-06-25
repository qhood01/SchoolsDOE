library(plyr)
library(dplyr)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(DT)

colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')
map <- readOGR(dsn="./districts_geojson")
schools.18 <- readRDS("./schools.18.rds")
districts.18 <- readRDS("./districts.18.rds")
## Todo
## Dynamic Sizing
## Markers for each school
## Zoom when click on district

function(input, output) {

    output$map <- renderLeaflet({
        leaflet(map) %>%
            setView(lng=-74.006,lat=40.7128,zoom=11) %>%
            addTiles() %>%
            addPolygons(layerId=~SchoolDist,
                        color="black",
                        weight=2,
                        smoothFactor=0.5,
                        fillColor=~Color,
                        fillOpacity=0.5,
                        highlightOptions=highlightOptions(color = "white", weight = 3,
                                                          bringToFront = TRUE))
    })

    click <- reactiveValues(id=NULL)

    observeEvent(input$map_shape_click,{
        click$id <- input$map_shape_click$id
    })

    output$plot=renderPlot({
        if (!is.null(click$id)) {
            id <- click$id
            df <- schools.18[which(schools.18$SchoolDist == id),]
            if (input$level == "Elementary") {
                df <- df[which(df$elementary == 1),]
            } else if (input$level == "Middle") {
                df <- df[which(df$middle == 1),]
            }
            boro <- as.character(map@data$Borough[which(map@data$SchoolDist == click$id)])
            c <- as.character(map@data$Color[which(map@data$SchoolDist == click$id)])
            df <- df[order(df[[input$var[1]]]),]
            par(bg="#F5F5F5")
            min <- 100*min(df[,input$var])
            max <- 100*max(df[,input$var])
            dotchart(x=100*df[,input$var[1]],labels=substr(df[["School Name"]],1,20),
                     xlab="Percent",pch=20,pt.cex=3,xlim=c(min,max),
                     main=paste0(boro," District ", id, "\nPercentage of Students by School in 2017-18"))
            if (length(input$var) == 1) {
                distPov <- 100*districts.18[which(districts.18$District == id),input$var]
                abline(v=distPov)
            } else {
                i <- 1
                for (var in input$var[-1]) {
                    points(x=100*df[[var]],y=1:nrow(df),col=colors[i],pch=20,cex=3)
                    i <- i+1
                }
            }
        }
    })

    output$table=DT::renderDataTable({
        if (!is.null(click$id)) {
            id <- click$id
            df <- schools.18[which(schools.18$SchoolDist == id),]
            if (input$level == "Elementary") {
                df <- df[which(df$elementary == 1),]
            } else if (input$level == "Middle") {
                df <- df[which(df$middle == 1),]
            }
            schools <- df[["School Name"]]
            boro <- as.character(map@data$Borough[which(map@data$SchoolDist == click$id)])
            df <- df[order(df[[input$var[1]]]),c("School Name",input$var,"Total Enrollment")]
            df[[input$var]] <- round(100*df[[input$var]],2)
            names(df) <- c("School", input$var, "Enrollment")
            return(df)
        }
    })
}

