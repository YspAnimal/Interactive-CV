library(shiny)

workingPathUI <- function(id){
  ns <- NS(id)
  timevisOutput('timeLine')
  tags$div(id="skillsButtons")
}

workingPath <- function(input, output, session) {
  timeLineData <- read.xlsx("../WorkingPath.xlsx", sheet = 1, detectDates = TRUE)
  timeLineData$content <- timeLineData$place
  projects <- read.xlsx("../WorkingPath.xlsx", sheet = 2)
  skills <- read.xlsx("../WorkingPath.xlsx", sheet = 3)
  
  
  output$timeLine <- renderTimevis({
    timevis(timeLineData, showZoom = FALSE, height = 250)
  })
  inserted <- c()
  
  observeEvent(input$timeLine_selected, {
    selectedOrg <- timeLineData %>% 
      filter(id == as.numeric(input$timeLine_selected)) %>% 
      select(place)
    selectedSkills <- skills %>% 
      filter(company %in% selectedOrg)
    
    if (!is.null(inserted)) {
      map(inserted, function(x){
        removeUI(selector =  paste0("#", x))
      })
    }
    
    idNum <- 1
    skillsButtons <- map(selectedSkills$keySkills, function(x){
      id <- paste0('skill', idNum)
      inserted[idNum] <<- id
      button <- actionButton(id,x)
      idNum <<- idNum + 1
      return(button)
    })
    fluidRow(
      insertUI(
        selector = "#skillsButtons", 
        ui = skillsButtons
      )
    )
  })
  
}


shinyApp(ui = ui, server = server)