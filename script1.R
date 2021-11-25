setwd("/home/shaan/Documents/Symbiosis_Linux/R/R-Mini_project_1")

library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(ggalt)
library(dplyr)
library(tmap)
library(leaflet)
library(tmaptools)
library(sp)
library(htmltools)
library(leaflet.extras)
library(viridis)

full_data<- read.csv("India_Education_Cleaned.csv")
#View(full_data)


#Data Wrangling

sortedData = subset(full_data, select = -c(globalid,Boys.Only.School, Girls.Only.School,Elementary.Enrolment.by.School.Category,Good.Condition.Classrooms) )
sortedData<-mutate(sortedData,SeniorTeachers=Age.55.56.of.Teachers+Age.57.58.of.Teachers+Age.59.60.of.Teachers)
sortedData = subset(sortedData,select = -c(Age.55.56.of.Teachers,Age.57.58.of.Teachers,Age.59.60.of.Teachers))
sortedData<-mutate(sortedData,JuniorTeachers=Total.Teachers-SeniorTeachers)
sortedData<-mutate(sortedData, Washrooms.For.Both=(Girls.Toilet.Facilitated.Schools+ Boys.Toilet.Facilitated.Schools)-Total.No.Of.Schools)
sortedData$District.Code<- as.numeric(sortedData$District.Code) #converting non numeric into numeric and NA in district code
sortedData[is.na(sortedData)]=0#replacing NULL values with 0


GoodAttr<-dplyr::select(sortedData,Total.No.Of.Schools,Schools.Approachable.By.Road.in.All.Weather,Washrooms.For.Both,Playground.Facilitated.Schools,Boundarywall.Facilitated.Schools,Drinking.Water.Facilitated.Schools,Electricity.Facilitated.Schools, Computer.Facilitated.Schools,Mid.Day.Meal.Facilitated.Schools,Total.Teachers)

Percentage<-transmute(GoodAttr,PercentageValues=(GoodAttr/Total.No.Of.Schools)*100)
write.csv(Percentage, file = "Percentage.csv")

PercentageCSV<-read.csv("Percentage.csv")

# selecting only numeric columns for Summation
PercentageCSV<- PercentageCSV[,2:11]

PercentageCSV<-mutate(PercentageCSV, cbind(PercentageCSV,SumAll=rowSums(PercentageCSV)))
Score <- transmute(PercentageCSV,AverageScore=(SumAll)/10) # dividing the sum by 10 number of coulmns For Average
sortedData$SchoolScore<-Score

sortedData$SchoolQuality[sortedData$SchoolScore <=100 ]="Ultra Poor MAX"
sortedData$SchoolQuality[sortedData$SchoolScore >100 ]="Poor"
sortedData$SchoolQuality[sortedData$SchoolScore >200 ]="Moderate"
sortedData$SchoolQuality[sortedData$SchoolScore >300 ]="Good"
sortedData$SchoolQuality[sortedData$SchoolScore >400 ]="Very Good"
sortedData$SchoolQuality[sortedData$SchoolScore >500 ]="Excellent"
sortedData$SchoolScore[is.na(sortedData$SchoolScore)]=0#replacing NULL values with 0

sortedData<-mutate(sortedData,Country="India")
sortedData<-mutate(sortedData,Division="NULL")

#Creating a temporary data frame containing statenames and states containing total number of teachers
TotalTeacher<-data.frame(Statenames=factor(), TotalTeachers=factor())
temp<- dplyr::select(sortedData, Statenames, Total.Teachers)
temp$Statenames<-as.factor(temp$Statenames) # converting into factors so we can  get levels

levelOfState<- levels(temp$Statenames)
for (StateNam in levelOfState) {
    StateNamm<- filter(temp, Statenames==StateNam)
    total=sum(StateNamm$Total.Teachers)
    nRow <- data.frame(Statenames=StateNam, TotalTeachers=total)
    TotalTeacher<- rbind(TotalTeacher,nRow)
    }

Ranks<- sortedData %>% count(Statenames)
names(Ranks)[2]<-"TotalDistricts"
Ranks<-merge(Ranks,TotalTeacher, by="Statenames")

Ranks$Rank.StateSize  <- rank(-Ranks$TotalDistricts, ties.method = "max")
Ranks$Rank.NoOfTeacher<- rank(-Ranks$TotalTeachers)


sortedData<-sortedData[,c(1,46,47,2,4,3,seq(5,37),41,42,43,44,45,39,40)]

NorthIndia<-     filter(sortedData, Statenames=="NCT of Delhi" | Statenames=="Haryana" | Statenames=="Punjab" | Statenames=="Chandigarh" | Statenames=="Rajasthan" | Statenames=="Uttar Pradesh" | Statenames=="Ladakh" | Statenames=="Jammu & Kashmir" | Statenames=="Himachal Pradesh" | Statenames=="Uttarakhand")
NorthEastIndia<- filter(sortedData, Statenames=="Assam" | Statenames=="Meghalaya" | Statenames=="Tripura" | Statenames=="Nagaland" | Statenames=="Mizoram" | Statenames=="Manipur" | Statenames=="Arunachal Pradesh" | Statenames=="Sikkim")
CentralIndia<-   filter(sortedData, Statenames=="Madhya Pradesh" | Statenames=="Chhatisgarh")
EastIndia<-      filter(sortedData, Statenames=="Goa" | Statenames=="Maharashtra" | Statenames=="Dadra & Nagar Haveli" | Statenames=="Gujarat" | Statenames=="Daman & Diu")
WestIndia<-      filter(sortedData, Statenames=="Bihar" | Statenames=="Orissa" | Statenames=="West Bengal" | Statenames=="Jharkhand")
SouthIndia<-     filter(sortedData, Statenames=="Kerala" | Statenames=="Tamil Nadu" | Statenames=="Telangana" | Statenames=="Karnataka" | Statenames=="Pondicherry" | Statenames=="Lakshadweep" | Statenames=="Andhra Pradesh" | Statenames=="Andaman & Nicobar Islands")

NorthIndia<-     mutate(NorthIndia,Division="North")
NorthEastIndia<- mutate(NorthEastIndia,Division="NorthEast")
CentralIndia<-   mutate(CentralIndia,Division="Central")
EastIndia<-      mutate(EastIndia,Division="East")
WestIndia<-      mutate(WestIndia,Division="West")
SouthIndia<-     mutate(SouthIndia,Division="South")

sortedData<- rbind(NorthIndia,NorthEastIndia,CentralIndia,EastIndia,WestIndia,SouthIndia)
sortedData<- arrange(sortedData,objectid)

#View(sortedData)

mapData<-readOGR("shapefiles/India_Education.shp") #loading the map Data from shape file
mapData<-mapData[1:693,1:1] #removing redundant data from the shape file
#View(mapData)
#plot(mapData)

mergedSHP_CSV<- merge(mapData,sortedData, by.x="objectid", by.y="objectid") # merging sorted cleaned data with the shape file via objectid layer
#View(mergedSHP_CSV)

# Administration Plots
tm_shape(mergedSHP_CSV) + tm_fill("#FFDE70") + tm_layout(title="INDIA", legend.show = FALSE)+ tm_compass(type = "8star", position = c("right", "top"))  #plotting India Full map
tm_shape(mergedSHP_CSV) + tm_fill("Division") + tm_layout(title="INDIA-Divisions" ,legend.outside = TRUE)+tm_compass(type = "8star", position = c("right", "top")) # plotting Divisions in INdia
tm_shape(mergedSHP_CSV) + tm_fill("Statenames") + tm_layout(title="INDIA-States"  ,legend.outside = TRUE)+tm_compass(type = "8star", position = c("right", "top"))+ tmap_options(max.categories = 38)#plotting States in India
tm_shape(mergedSHP_CSV) + tm_fill("District.Name")+ tm_layout(title="INDIA-District",legend.show = FALSE)+tm_compass(type = "8star", position = c("right", "top")) +tmap_options(max.categories = 686) #plotting Districts in India

# Facets - pseudofacets of given attributes
Total.Teachers  <- tm_shape(mergedSHP_CSV) +tm_fill("Total.Teachers") +tm_borders(alpha=.2) + tm_style("col_blind")
Total.classrooms<- tm_shape(mergedSHP_CSV) +tm_fill("Total.number.of.Classrooms")+tm_borders(alpha=.2)+ tm_style("col_blind")
Total.Schools   <- tm_shape(mergedSHP_CSV) +tm_fill("Total.No.Of.Schools") +tm_borders(alpha=.2) + tm_style("col_blind")
Total.Literacy  <- tm_shape(mergedSHP_CSV) +tm_fill("Total.Literacy.Proportion") +tm_borders(alpha=.2)+ tm_style("col_blind")
tmap_arrange(Total.Teachers,Total.classrooms,Total.Schools,Total.Literacy, ncol = 2)


# Histogram on a Continuous (Numeric) Variable
theme_set(theme_classic())
g <- ggplot(sortedData, aes( x=Major.Repair.Needed.Total.Classrooms)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram(aes(fill=SchoolQuality),
                   binwidth = 400,
                   col="black",
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning",
       subtitle="Literacy Proportion across School Quality")




#scatterplot literacy vs population
options(scipen = 999) # turn-off scientific notation like 1e+48
sortedDataSelect <- sortedData[sortedData$Total.Population > 3500000 &
                                 sortedData$Total.Population <= 5000000 &
                           sortedData$Total.Literacy.Proportion> 0.1 &
                             sortedData$Total.Literacy.Proportion< 100,]

ggplot(sortedData, aes(x=Total.Population, y=Total.Literacy.Proportion)) +
  geom_point(aes(colour=Statenames, size=Total.Population)) +   # draw points
  geom_smooth(method="loess", se=F) + # draw smoothing line
  ylim(c(0, 100)) +
  xlim(c(0, 5000000)) +
  geom_encircle(aes(x=Total.Population, y=Total.Literacy.Proportion),
                data=sortedDataSelect,
                color="red",
                size=2,
                expand=0.08) +   # encircle
  labs(subtitle="Literacy Vs Population",
       x="Population",
       y="Literacy",
       title="Scatterplot + Encircle",
       caption="India")


# Draw Bar plot
theme_set(theme_bw())
ggplot(sortedData, aes(x=Statenames, y=Total.No.Of.Schools)) +
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  labs(title="Ordered Bar Chart",
       subtitle="Total Schools Vs States ",
       caption="India") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#Wranggling data to make able to plot double bar graphs from single data frame
Rank2<- data.frame(Attributes=rep(c("Ranks of State Size","Ranks of No of Teachers"),each=nrow(Ranks)),
                   Statenames=rep(c(Ranks$Statenames),2),
                   Rank=c(Ranks$Rank.StateSize,Ranks$Rank.NoOfTeacher)
                   )

# plotting double bar grapgh
ggplot(data=Rank2, aes(x=Statenames, y=Rank, fill=Attributes)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(x = 'State Name', y='Ranking ',title = 'Comparing the Ranks: State Size vs Total No Of Teachers')


######

comb<- c("Total.No.Of.Schools", "Total.number.of.Classrooms")
tm_shape(mergedSHP_CSV) +tm_fill(comb) +tm_layout(legend.position = "right") +tm_borders(alpha=.2)


#interactive Leaflet plots

#interactive Leaflet plots
mergedSHP_CSV <- mergedSHP_CSV %>%
  sf::st_as_sf(coords = c("Latitude", "Longitude"),crs = WGS84)

palPwr <- leaflet::colorFactor(palette = viridis(37),domain = mergedSHP_CSV$Statenames)

leaflet(data = mergedSHP_CSV) %>%
  addTiles() %>%
  setView(lng =80,lat=26,zoom=4)%>%
  addPolygons(data = mergedSHP_CSV,
              fillColor = ~palPwr(mergedSHP_CSV$Statenames),
              stroke = TRUE,
              fillOpacity = 0.7,
              smoothFactor = 0.5,
              color = "grey",
              weight = 1,
              opacity =0.5,
              popup =paste("<b>","<i>",mergedSHP_CSV$District.Name,"</i>","<i>",mergedSHP_CSV$Statenames,"</i>","</b>", "<br>",
                           "<b>","Total Schools: ","</b>",mergedSHP_CSV$Total.No.Of.Schools,"<br>",
                           "<b>","Highly Qualified Teachers: " ,"</b>",mergedSHP_CSV$Total.Teachers,"/",mergedSHP_CSV$Post.Graduation.Qualified.Teachers+mergedSHP_CSV$Post.Doctrate.Qualified.Teachers,"<br>",
                           "<b>","Total Classrooms: ","</b>",mergedSHP_CSV$Single.Classroom.Schools,"<br>"
              ),

              fill = "Statenames",
              highlight = highlightOptions(weight = 5,
                                           color = "white",
                                           fillOpacity = 0.7,
                                           bringToFront = FALSE),
              label = mergedSHP_CSV$Statenames)


#######################################################################
#Rough works and savings

#hist(sortedData$"Total.No.Of.Schools")
#qtm(mergedSHP_CSV, fill = "Total.No.Of.Schools")
#Output.Areas<- readOGR(choose.files(caption = "Select Shapefile", multi = FALSE))
