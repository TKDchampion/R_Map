#安裝Library(Done!)//新電腦請先安裝
#----------------------------------------------------------------------------------------------------------
#install.packages("shiny")
#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("makeIcon")
#----------------------------------------------------------------------------------------------------------

#啟動Library
#----------------------------------------------------------------------------------------------------------
library(shiny) #類似網頁(用途多，可把地圖用上去在做分析)
library(leaflet) #製作地圖用
library(rgdal) #製作SHAPEFILE用(區塊圖)
#----------------------------------------------------------------------------------------------------------

#把要匯入的檔案加進去，並設定路徑
#----------------------------------------------------------------------------------------------------------

csvpath = "C:/Mapcase/Data/"
csvfilesn = list.files( path = csvpath, pattern="*.csv")
tmprt = function(rtcsv){read.csv( rtcsv, stringsAsFactors = FALSE)}
Cdata = lapply(paste(csvpath,csvfilesn, sep = ""), tmprt)
csvfilesn
setwd("C:/Mapcase/f")
#----------------------------------------------------------------------------------------------------------

#用rgdal這個package將設定好shapefile區塊圖(工廠)和把座標(WGS84)調整好!
#----------------------------------------------------------------------------------------------------------
states <- readOGR("f.shp",layer="f",verbose = FALSE,encoding = "UTF-8")
w8 <- spTransform(states,CRS("+proj=longlat +datum=WGS84"))
#----------------------------------------------------------------------------------------------------------

#ui是負責前端排版和按鈕
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

#設定標籤(所有重症案例)
#----------------------------------------------------------------------------------------------------------

#case_lable<- colorFactor(c("red","black","purple"), domain = c("一般登革","重症個案","重症死亡個案"))
case_lable<- colorFactor(c("red","blue"), domain = c("N","Y"))
#----------------------------------------------------------------------------------------------------------

#server是負責後端，計算與繪製
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

#啟動Shiny
#----------------------------------------------------------------------------------------------------------
shinyApp(ui,server)  
#----------------------------------------------------------------------------------------------------------

