library(ShinyDash)
library(shiny)
library(plyr)
#library(leafletR)
library(leaflet)

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
# Put this back for multiple college
#College = c( "All", levels(unique(inputData$College)))
College = c( "All", unique(inputData$College))
WorkType = c( "All", levels(unique(inputData$Work.Type)))
DegreeLevel = c( "All", levels(unique(inputData$Degree.Level)))


shinyUI(fluidPage(
  
 tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
  leafletMap(
   "map", "100%", 400,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
  initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
   options=list(
      center = c(37.45, -93.85),
      zoom = 4,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),
  
  
  # Application title
  titlePanel("Continuing The Adventure After Iowa State"),
  hr(),
  
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
      selectInput("College", "Choose a College", choices = College, selected= "All"),
      selectInput("Major", "Choose a Major", choices = NULL, selected="All" ),
      selectInput("DegreeLevel", "Degree Level", choices = DegreeLevel, selected="All" ),
      selectInput("WorkType", "Type of Position", choices = WorkType, selected="All" ),
      hr(),
      sliderInput("radius", "Choose Circle Radius:", min=0, max=10, value=5),
      hr(),
      h5("Top Employers in this Area"),
      tableOutput("companies1"),
      hr(),
      h5("Salaries in this Area"),
      plotOutput("plot1")
      
      
    ),
    
    
    mainPanel( 
      h4('Companies Hiring in this Area'),
      dataTableOutput("people1")                
    )
  )
))
