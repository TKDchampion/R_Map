#�w��Library(Done!)//�s�q���Х��w��
#----------------------------------------------------------------------------------------------------------
#install.packages("shiny")
#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("makeIcon")
#----------------------------------------------------------------------------------------------------------

#�Ұ�Library
#----------------------------------------------------------------------------------------------------------
library(shiny) #��������(�γ~�h�A�i��a�ϥΤW�h�b�����R)
library(leaflet) #�s�@�a�ϥ�
library(rgdal) #�s�@SHAPEFILE��(�϶���)
#----------------------------------------------------------------------------------------------------------

#��n�פJ���ɮץ[�i�h�A�ó]�w���|
#----------------------------------------------------------------------------------------------------------

csvpath = "C:/Mapcase/Data/"
csvfilesn = list.files( path = csvpath, pattern="*.csv")
tmprt = function(rtcsv){read.csv( rtcsv, stringsAsFactors = FALSE)}
Cdata = lapply(paste(csvpath,csvfilesn, sep = ""), tmprt)
csvfilesn
setwd("C:/Mapcase/f")
#----------------------------------------------------------------------------------------------------------

#��rgdal�o��package�N�]�w�nshapefile�϶���(�u�t)�M��y��(WGS84)�վ�n!
#----------------------------------------------------------------------------------------------------------
states <- readOGR("f.shp",layer="f",verbose = FALSE,encoding = "UTF-8")
w8 <- spTransform(states,CRS("+proj=longlat +datum=WGS84"))
#----------------------------------------------------------------------------------------------------------

#ui�O�t�d�e�ݱƪ��M���s
#----------------------------------------------------------------------------------------------------------
ui <- bootstrapPage(
  title = "Dengue case mapping",
  tags$style(type="text/css","html,body{width:100%;height:100%}"),
  leafletOutput("map",width="100%",height="100%"),
  absolutePanel(top=100,right=20,draggable = FALSE,
                absolutePanel(top = -60, right = -10,titlePanel(h2("Dengue Case Mapping"))),
                selectInput("select", "Year", 
                            choices = list("2007" = 2007, "2008" = 2008, "2009" = 2009, "2010" = 2010, "2011" = 2011, "2012" = 2012, "2013" = 2013, "2014" = 2014, "2015" = 2015, "2016" = 2016), 
                            selected = 1),
                
                hr(),
                fluidRow(column(3, verbatimTextOutput("value"))),
                sliderInput("range", "Month", 1, 12,value = c(1,12), step = 1)
  )
)
Data_read = Cdata[[1]]
#----------------------------------------------------------------------------------------------------------

#�]�w����(�Ҧ����g�ר�)
#----------------------------------------------------------------------------------------------------------

#case_lable<- colorFactor(c("red","black","purple"), domain = c("�@��n��","���g�Ӯ�","���g���`�Ӯ�"))
case_lable<- colorFactor(c("red","blue"), domain = c("N","Y"))
#----------------------------------------------------------------------------------------------------------

#server�O�t�d��ݡA�p��Pø�s
#----------------------------------------------------------------------------------------------------------
server <-function(input,output){
  
  INTimebar <- reactive({Data_read[Data_read$Month >= input$range[1] & Data_read$Month <= input$range[2] & Data_read$Year == input$select,]})
  output$map <- renderLeaflet({leaflet(Data_read) %>% 
      addPolygons(data=w8,popup= ~FNAME) %>% 
      addTiles() %>% 
      fitBounds(~min(Enumeration_unit_long),
                ~min(Enumeration_unit_lat),
                ~max(Enumeration_unit_long),
                ~max(Enumeration_unit_lat)
      )
  }
  )
  observeEvent(input$select,
               {leafletProxy("map",data = INTimebar()) %>% 
                   clearMarkerClusters() %>%
                   addCircleMarkers(data=INTimebar(),
                                    lng=~Enumeration_unit_long,
                                    lat=~Enumeration_unit_lat,
                                    popup = ~paste("<h5>",Sex,"</h5>",
                                                   Date_Onset,"<br>",
                                                   County_living,"<br>",
                                                   Township_living,"<br>",
                                                   Imported,"<br>",
                                                   Month,"<br>",
                                                   Year,"<br>"
                                                   
                                    ),
                                    color=~case_lable(Imported),
                                    stroke = FALSE,
                                    fillOpacity = 0.9,
                                    clusterOptions = markerClusterOptions()
                                    
                   )
               }
  )
  observeEvent(input$range,
               {leafletProxy("map",data = INTimebar()) %>% 
                   clearMarkerClusters() %>%
                   addCircleMarkers(data=INTimebar(),
                                    lng=~Enumeration_unit_long,
                                    lat=~Enumeration_unit_lat,
                                    popup = ~paste("<h5>",Sex,"</h5>",
                                                   Date_Onset,"<br>",
                                                   County_living,"<br>",
                                                   Township_living,"<br>",
                                                   Imported,"<br>",
                                                   Month,"<br>",
                                                   Year,"<br>"
                                                   
                                    ),
                                    color=~case_lable(Imported),
                                    stroke = FALSE,
                                    fillOpacity = 0.9,
                                    clusterOptions = markerClusterOptions()
                                    
                   )
               }
  )
}
#----------------------------------------------------------------------------------------------------------

#�Ұ�Shiny
#----------------------------------------------------------------------------------------------------------
shinyApp(ui,server)  
#----------------------------------------------------------------------------------------------------------
