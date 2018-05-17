library(shiny)

ui <- fluidPage(
  timevisOutput('timeLine'),
  tags$div(id="skillsButtons")
  )

server <- function(input, output, session) {
  timeLineData <- read.xlsx("../WorkingPath.xlsx", sheet = 1, detectDates = TRUE)
  timeLineData$content <- timeLineData$place
  projects <- read.xlsx("../WorkingPath.xlsx", sheet = 2)
  skills <- read.xlsx("../WorkingPath.xlsx", sheet = 3)
  
  
  output$timeLine <- renderTimevis({
    timevis(timeLineData, showZoom = FALSE, height = 250)
  })

  observeEvent(input$timeLine_selected, {
    selectedOrg <- timeLineData %>% 
      filter(id == as.numeric(input$timeLine_selected)) %>% 
      select(place)
    selectedSkills <- skills %>% 
      filter(company %in% selectedOrg)

    removeUI(selector =  "#skill1")
    
    idNum <- 0
    skillsButtons <- map(selectedSkills$keySkills, function(x){
      print(x)
      idNum <<- idNum + 1
      id <- paste0('skill', idNum)
      actionButton(id,x)
    })
    print(str(skillsButtons))
    fluidRow(
      insertUI(
        selector = "#skillsButtons", 
        ui = skillsButtons
      )
    )
  })
  
}


shinyApp(ui = ui, server = server)