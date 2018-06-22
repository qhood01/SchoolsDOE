library(plyr)
library(dplyr)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(scales)


## Todo
## Radio Button with elementary, middle, high -- new variable
## Markers for each school
## Zoom when click on district
## District 10 appears twice
map <- readOGR(dsn="./nysd_18b",layer="nysd")
map <- spTransform(map, CRS("+proj=longlat +datum=WGS84 +no_defs"))

districts.to.boro <- as.data.frame(table(substr(schools$DBN,1,2),substr(schools$DBN,3,3)))
districts.to.boro <- districts.to.boro[which(districts.to.boro$Freq > 0),]
districts.remove <- c("75","79","84")
districts.to.boro <- districts.to.boro[which(!districts.to.boro$Var1%in%districts.remove),]
districts.to.boro$Borough <- NA
districts.to.boro$Borough[which(districts.to.boro$Var2 == "K")] <- "Brooklyn"
districts.to.boro$Borough[which(districts.to.boro$Var2 == "M")] <- "Manhattan"
districts.to.boro$Borough[which(districts.to.boro$Var2 == "Q")] <- "Queens"
districts.to.boro$Borough[which(districts.to.boro$Var2 == "R")] <- "Staten Island"
districts.to.boro$Borough[which(districts.to.boro$Var2 == "X")] <- "Bronx"
districts.to.boro <- districts.to.boro[,c("Var1","Borough")]
names(districts.to.boro)[1] <- "SchoolDist"
boro.colors <- data.frame("Borough"=unique(districts.to.boro[["Borough"]]),"Color"=brewer.pal(5,"Dark2"))
districts.to.boro <- join(districts.to.boro,boro.colors)
map@data <- join(map@data,districts.to.boro)

schools <- read.csv("./schoolDemos.csv")
schools$SchoolDist <- as.numeric(substr(schools$DBN,1,2))
schools$elementary <- ifelse(schools$Grade.1 > 0,1,0)
schools$middle <- ifelse(schools$Grade.6 > 0,1,0)
schools$high <- ifelse(schools$Grade.9 > 0,1,0)
schools.16 <- schools[which(schools$Year == "2015-16"),]

districts <- read.csv("./districtDemos.csv")
districts.16 <- districts[which(districts$Year == "2015-16"),]

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
            df <- schools.16[which(schools.16$SchoolDist == id),]
            if (input$level == "Elementary") {
                df <- df[which(df$elementary == 1),]
            } else if (input$level == "Middle") {
                df <- df[which(df$middle == 1),]
            }
            boro <- as.character(map@data$Borough[which(map@data$SchoolDist == click$id)])
            c <- as.character(map@data$Color[which(map@data$SchoolDist == click$id)])
            df <- df[order(df$X..Poverty.1),]
            distPov <- districts.16[which(districts.16$District == id),"X..Poverty.1"]
            par(bg="#F5F5F5")
            dotchart(x=df$X..Poverty.1,labels=df$School.Name,xlab="Percent",pch=20,
                     color=c,pt.cex=2,main=paste0(boro," District ", id, "\nPercentage of Students in Poverty by School"))
            abline(v=distPov)
        }
    })

}

