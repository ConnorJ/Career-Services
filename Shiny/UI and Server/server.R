library(shiny)
library(ggplot2)

library(shiny)
library(reshape2)
library(plyr)
library(leafletR)

library(leaflet)

if (!require(devtools))
  install.packages('devtools')
devtools::install_github('leaflet-shiny', 'jcheng5')

if (!require(maps))
  install.packages('maps')
devtools::install_github('ShinyDash', 'trestletech')


# Melt lat and long into graduation data
# location has the lat and long that I need to plot my data
data <- read.csv("https://raw.githubusercontent.com/ConnorJ/Stat-585-Project/master/data/locations.csv")

# Import Graduation placement data
grad <- read.csv("https://raw.githubusercontent.com/ConnorJ/Stat-585-Project/master/data/Eng%20Job.csv")


# Convert States full names to abbrevations
grad$State <- state.abb[match(grad$State,state.name)]
grad$loc <- paste(grad$City, ",",grad$State)
grad$City[grad$City == ""] <- NA
grad$Employer[grad$Employer == "Employed Unknown"] <- NA

# Merge the locations into the graduation data
grad.merge <- merge(na.omit(grad), data, by = "loc")

grad.merge$College <- "College of Engineering"

grad.merge$Compensation[grad.merge$Compensation == ""] <- NA

grad.merge$Compensation[grad.merge$Compensation == "0.00"] <- NA
grad.merge$Compensation[grad.merge$Compensation == 0] <-NA
grad.merge$Compensation <- gsub(",", "", grad.merge$Compensation, fixed = TRUE) 
grad.merge$Compensation <- gsub("$", "", grad.merge$Compensation, fixed = TRUE) 
grad.merge$Compensation <- gsub(".00", "", grad.merge$Compensation, fixed = TRUE) 

grad.merge$Compensation <- as.numeric(grad.merge$Compensation)
grad.merge$Compensation[grad.merge$Compensation <= 100] <- grad.merge$Compensation*40*52




grad.merge$Compensation[grad.merge$Compensation == 0] <-NA


grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "Materials Science & Engineering"] <- "Materials Science and Engineering"
grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "Materials Engineering"] <- "Materials Science and Engineering"
grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "Engineering Mechanics"] <- "Other Engineering"
grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "Engineering Science (Alumni Only)"] <- "Other Engineering"
grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "College of Engineering"] <- "Other Engineering"
grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "Engineering Applications (Alumni Only)"] <- "Other Engineering"
grad.merge$Major.1.at.Graduation[grad.merge$Major.1.at.Graduation == "Agricultural Engineering"] <- "Agricultural and Biosystems Engineering"


inputData = grad.merge
mapdata <- ddply(grad.merge,.(City,State),transform,count = NROW(piece))
mapstuff <- mapdata[ ! duplicated( mapdata[ c("City" , "State") ] ) , ]


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {

   map <- createLeafletMap(session, 'map')

  citiesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(inputData[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(inputData,
           Lat >= latRng[1] & Lat <= latRng[2] &
             Long >= lngRng[1] & Long <= lngRng[2])
  })
  
  
  filter <- reactive({  
    df <- citiesInBounds()
    
    if (input$WorkType != "All") {df <- subset(df, Work.Type == input$WorkType)}
    
    if (input$DegreeLevel != "All") {df <- subset(df, Degree.Level == input$DegreeLevel)}
    
    if (input$College == "All" & input$Major == "All"){df}
    if (input$College != "All" & input$Major == "All"){df <- subset(df, College == input$College)}
    if (input$College != "All" & input$Major != "All"){
      df <- subset(df, College == input$College & Major.1.at.Graduation == input$Major | Major.2.at.Graduation == input$Major )
    }
    df$Location <- paste(df$City, df$State)
    
    output$people1 = renderDataTable({
      dfTable <- df[, c(2, 7, 6, 3,4)]
      return(dfTable)
    })
    
    
    output$companies1 = renderTable({
      if (nrow(df)>0){
        df <- count(df,"Employer")
        df <- df[order(df$freq, decreasing = TRUE),]
        colnames(df) <- c("Employer","Employees")
        if (nrow(df)<5) {
          df<- df[1:nrow(df),]
          return(df)
        } else {
          df<- df[1:5,]
          return(df)
        }
      } 
    },include.rownames=FALSE)
    
    output$plot1 = renderPlot({
      if (nrow(df)>=5){
        print(qplot(data=(subset(na.omit(df), df$Compensation <= 500000 )), x=Compensation, geom="density", bin = 5000, size = 2, ylab = "Count", fill = "white") +
                xlim(0, 175000) + scale_size(guide = 'none') + guides(fill=FALSE) +
                theme(axis.title.y = element_text(size = rel(2)),
                      axis.title.x = element_text(size = rel(2)),
                      axis.text.y = element_blank())
            )
      } else {print("Not Enough Data")}
      
    })
    
    return(df)
  })
  
  radiusFactorcalc<- reactive({ radiusFactor <- 5000 * (as.numeric(input$radius)^2 +1) })
  
  observe({
    radiusFactor <- radiusFactorcalc()
    map$clearShapes() 
    mapdffilter <- filter()
    mapdata <- ddply(mapdffilter,.(City,State),transform,count = NROW(piece))
    mapstuff <- mapdata[ ! duplicated( mapdata[ c("City" , "State") ] ) , ]
    if (nrow(mapstuff) == 0)
      return()
    
    map$addCircle(
      mapstuff$Lat,
      mapstuff$Long,
      sqrt(mapstuff[["count"]]) * radiusFactor / max(5, input$map_zoom)^2,
      row.names(mapstuff),
      list(
        weight=1.2,
        fill=TRUE,
        color='#999933'
      )
    )
  })

  
  observe({
    dfmajor <- unique(subset(inputData, College == inputData$College))
    Major = c( "All", levels(factor(dfmajor$Major.1.at.Graduation)))
    updateSelectInput(session, "Major", choices = Major, selected="All")
  })
  


    
    
  
  
  
})



