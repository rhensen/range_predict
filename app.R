library(shiny)
library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(raster)
library(exactextractr)
library(data.table)
library(leaflet)
library(shinythemes)
library(pbapply)
library(shinybusy)
library(scales)

#setwd("/Volumes/My Passport for Mac/")

#scenario<- "2020s_rcp_8_5/"
type<- c("annual", "bare","herbaceous","litter","sage","shrub")
s8.5<-c("2020s_rcp_8_5/", "2050s_rcp_8_5/","2080s_rcp_8_5/")
s4.5<-c("2020s_rcp_4_5/", "2050s_rcp_4_5/", "2080s_rcp_4_5/")

wd<- "2016_nlcd/"
rastlist <- list.files(path = wd, pattern='.img$', recursive=TRUE, all.files=TRUE, full.names=TRUE)
baserasters<- lapply(rastlist,raster)
names(baserasters)<- type


getraster<- function(scenario){
    wd<- paste("future_projections_", scenario, sep="")
    rastlist <- list.files(path = wd, pattern='.img$', recursive=TRUE, all.files=TRUE, full.names=TRUE)
    allrasters<- lapply(rastlist,raster)
    type<- c("annual", "bare","herbaceous","litter","sage","shrub")
    names(allrasters)<- type
    return(list("rasters"=allrasters))
}

count_ext<- function(bound,type,allrasters) { 
    bound<- bound %>% st_as_sf() %>% st_union
    #st_crs(bound) <- 4326
    df<-  pblapply(type, function(type) {
        ras<-allrasters[[type]]
        ext<- exact_extract(ras,bound) %>% 
            rbindlist(idcol = "ID") %>%
            group_by(value,ID) %>%
            summarise_at(vars(coverage_fraction),sum, na.rm = TRUE) 
        total<- ext %>% group_by(ID) %>% 
            summarise_at(vars(coverage_fraction),sum, na.rm = TRUE) 
        df<- data.table(sort(unique(ext$ID)), total$coverage_fraction)
        names(df)[1]<- "ID"
        names(df)[2]<- "Bound_Total"
        final<- merge(df, ext, by = 'ID')
        final<- final %>% mutate(cover= type, perc_tot= final$coverage_fraction/final$Bound_Total, perval= final$coverage_fraction*(value/100)/final$Bound_Total)
        #Cov_acre= coverage_fraction*0.222394, Total_Acre= Bound_Total*0.222394)
        final<-final[,c(-1)]
        setcolorder(final, c("cover","value", "coverage_fraction", "perc_tot","perval","Bound_Total"))
        return(final)
        progress$inc(amount = 1/length(type))
        
    })
    dp<- rbindlist(df)
    #dp<- data_panel[which(data_panel$Pixel_ID=="255"),] 
    dp1<- dp[which(dp$value!=255),]%>% group_by(cover)%>% summarize_at(vars(perval), sum)
    dp2<- dp[which(dp$value==255),]%>% summarize_at(vars(perc_tot), mean)
    dp2$cover<- c("other/na")
    names(dp2) <- c("perval","cover")
    #setcolorder(dp2, c("cover","value", "coverage_fraction", "perc_tot","perval","Bound_Total"))
    dpfin<- rbind(dp1,dp2)
    return(dpfin)
}

count_ext2<- function(bound,type,allrasters) { 
    bound<- bound %>% st_as_sf() %>% st_union
    #st_crs(bound) <- 4326
    df<-  pblapply(type, function(type) {
        ras<-allrasters[[type]]
        ext<- exact_extract(ras,bound) %>% 
            rbindlist(idcol = "ID") %>%
            group_by(value,ID) %>%
            summarise_at(vars(coverage_fraction),sum, na.rm = TRUE) 
        total<- ext %>% group_by(ID) %>% 
            summarise_at(vars(coverage_fraction),sum, na.rm = TRUE) 
        df<- data.table(sort(unique(ext$ID)), total$coverage_fraction)
        names(df)[1]<- "ID"
        names(df)[2]<- "Bound_Total"
        final<- merge(df, ext, by = 'ID')
        final<- final %>% mutate(cover= type, perc_tot= final$coverage_fraction/final$Bound_Total, perval= final$coverage_fraction*(value/100)/final$Bound_Total)
        #Cov_acre= coverage_fraction*0.222394, Total_Acre= Bound_Total*0.222394)
        final<-final[,c(-1)]
        setcolorder(final, c("cover","value", "coverage_fraction", "perc_tot","perval","Bound_Total"))
        return(final)
        progress$inc(amount = 1/length(type))
        
    })
    dp<- rbindlist(df)
    #dp<- data_panel[which(data_panel$Pixel_ID=="255"),] 
    dp1<- dp[which(dp$value<=100),]%>% group_by(cover)%>% summarize_at(vars(perval), sum)
    dp2<- dp[which(dp$value>100),]%>% summarize_at(vars(perc_tot), mean)
    dp2$cover<- c("other/na")
    names(dp2) <- c("perval","cover")
    #setcolorder(dp2, c("cover","value", "coverage_fraction", "perc_tot","perval","Bound_Total"))
    dpfin<- rbind(dp1,dp2)
    return(dpfin)
}


shpfilefunc<- function(input){  
    req(input)
    shpDF <- input
    pwd <- getwd()
    updir <- dirname(shpDF$datapath[1])
    setwd(updir)
    for (i in 1:nrow(shpDF)) {
        file.rename(shpDF$datapath[i], shpDF$name[i])
    }
    shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
    shpPath <- paste(updir, shpName, sep = "/")
    setwd(pwd)
    shpFile <- readOGR(shpPath)
    return(shpFile)
}
kmlfilefunc<- function(input){  
    req(input)
    kmlFile <- readOGR(input$datapath)
    return(kmlFile)
}



# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("cerulean"),h1("Rangeland Condition Predictions Tool", align = "center"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput(inputId = "shp", label = "Import Shape File Folder or KML file", multiple = T, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj', ".kml")),
            #fileInput(inputId = "kml", label = "Import KML File ", multiple = F, accept = c(".kml")),
            
        selectInput("pred", "Select 1st Prediction",
                    choices=  list("Good Scenarios"= s4.5, "Worse Scenarios"= s8.5),
                    selected = s8.5[[1]], multiple = F),
        selectInput("pred2", "Select 2nd Prediction",
                    choices=  list("Good Scenarios"= s4.5, "Worse Scenarios"= s8.5),
                    selected = s8.5[[2]], multiple = F)),
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            h4("Uploaded Polygon", align = "center"),
            leafletOutput("map",width = "100%", height = 400),
            h4("Prediction Chart", align = "center"),
            plotOutput("chart"),add_busy_spinner(spin = "fading-circle"),
           
            
            
        )
        
    )
)



# Define server logic to read selected file ----
server <- function(input, output) {
    uploadfile <- reactive({
        if(grepl(".kml", input$shp$name, fixed = TRUE)){
            return(kmlfilefunc(input$shp))
        }
        else(shpfilefunc(input$shp))
    })
    

    ras1<- reactive({
        getraster(input$pred)
    })
    ras2<- reactive({
        getraster(input$pred2)
    })
    output$map <-  renderLeaflet({
        if(is.null(input$shp$name)){leaflet()}
        else{
            leaflet(uploadfile()) %>%
                addPolygons(weight = 1, smoothFactor = 0.5,
                            opacity = 0.5, fillOpacity = 0.2) %>% 
                addProviderTiles(providers$Esri.WorldTopoMap)
        }
    })
    output$chart <- renderPlot({
        if(is.null(input$shp$name)){ggplot()} 
        else{
        plotdatb<- count_ext2(uploadfile(),type,baserasters)
        plotdatb$prediction<- "2016 NLCD"
        plotdat<- count_ext(uploadfile(),type,ras1()$rasters)
        plotdat$prediction<- input$pred
        plotdat2<- count_ext(uploadfile(),type,ras2()$rasters)
        plotdat2$prediction<- input$pred2
        fin<- rbind(plotdatb,plotdat)
        fin<- rbind(fin,plotdat2)
        positions<- type<- c("annual", "bare","herbaceous","litter","sage","shrub", "other/na")
        ggplot(fin, aes(x=cover, y=perval, fill=prediction))+ geom_bar(stat="identity",position="dodge")+ 
            labs(title="Cover Types Across Scenarios", x= "cover type", y="Percent Cover of upload file")+
            scale_fill_brewer()+ theme_dark() + scale_x_discrete(limits = positions)+ 
            scale_y_continuous(labels = scales::percent_format())
        }
        })
}

# Create Shiny app ----
shinyApp(ui, server)
