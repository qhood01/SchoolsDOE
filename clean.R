library(plyr)
library(dplyr)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(DT)

map <- readOGR(dsn="./nysd_18b",layer="nysd")
map <- spTransform(map, CRS("+proj=longlat +datum=WGS84 +no_defs"))
map <- map[-33,]
schools <- read.csv("./schoolDemos.csv")
districts <- read.csv("./districtDemos.csv")

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
writeOGR(map,"./districts_geojson",layer="",driver="GeoJSON")

names(schools) <- gsub("X","",gsub("[.]","",gsub("1", " %", names(schools))))
names(schools)[c(34,36)] <- c("Students with Disabilities %", "English Language Learners %")
names(districts) <- gsub("X","",gsub("[.]","",gsub("1", " %", names(districts))))
names(districts)[c(34,36)] <- c("Students with Disabilities %", "English Language Learners %")
schools$SchoolDist <- as.numeric(substr(schools$DBN,1,2))
schools$elementary <- ifelse(schools$GradeK > 0,1,0)
schools$middle <- ifelse(schools$Grade6 > 0,1,0)
schools$high <- ifelse(schools$Grade9 > 0,1,0)

districtVars <- c("District", "Asian %", "Black %", "Hispanic %", "White %", "Students with Disabilities %", "English Language Learners %", "Poverty %")
districts.16 <- districts[which(districts$Year == "2015-16"),districtVars]
saveRDS(districts.16,"districts.16.rds")

schoolVars <- c("SchoolName", "SchoolDist", "elementary", "middle", "high", "TotalEnrollment", "Asian %", "Black %", "Hispanic %", "White %", "Students with Disabilities %", "English Language Learners %", "Poverty %")
schools.16 <- schools[which(schools$Year == "2015-16"),schoolVars]
saveRDS(schools.16,"schools.16.rds")
