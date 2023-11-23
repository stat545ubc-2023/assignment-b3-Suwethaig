#Install the necessary packages if you donot have them installed
#install.packages("shinydashboard")
#install.packages("DT")

#Loading the necessary packages for the app
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

#Reading the csv file containing the crop recommendation dataset
crop_data <- read.csv("crop_recommendation.csv", stringsAsFactors = FALSE)

#Identifying the unique crop names to input in the UI
crop_names <- unique(crop_data['LABEL'])
print(crop_names)

# UI function
ui <- fluidPage(

  # Adding a title
  titlePanel(" TEMPERATURE ANALYSIS OF CROP DATA"),

  # Adding a side panel
  sidebarPanel(

    #Selecting the crop input
    selectInput("crop_type", "CROP",
                 choices = c("RICE", "MAIZE", "MANGO", "GRAPES",
                             "APPLE", "ORANGE", "COCONUT"),
                 selected = "RICE"),

    br(),

    #Adding a download button
    downloadButton("download_1", "Download Table"),

    br(), br(),

    #Adding an image
    imageOutput("crop_img")
  ),

  # Adding a main panel
  mainPanel(

    # Defining the UI element of plot output
    plotOutput("param_plot", height = "200px"),

    br(), br(),

    # Defining the UI element of table output
    dataTableOutput("param_table")
  )
  )

# Server function
server <- function(input, output) {

  #Filtering data based on the crop chosen by the user
  chosen_crop <- reactive({
    crop_data %>%
      filter(LABEL == input$crop_type)
  })

  #Displaying box plot of the chosen crop
  output$param_plot <- renderPlot({
    ggplot(chosen_crop(), aes(x = LABEL, y = TEMPERATURE))+
      geom_boxplot(outlier.colour="red", outlier.size=4)+
      theme(axis.text = element_text(size=16), axis.title.y = element_blank(),
            axis.title.x = element_text(vjust=-1, size=16))+
      coord_flip()
  })

  #Sorted table showing all of the crop parameters
  output$param_table <- renderDT(
    chosen_crop(), options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE),
  )

  #Download button to download the csv file of the sorted table
  output$download_1 <- downloadHandler(
    filename = function() {
      paste(input$crop_type,"DATA", ".csv", sep = " ")
    },
    content = function(file) {
      write.csv(chosen_crop(), file)
    }
  )

  #Image of the chosen crop in the side bar panel
  output$crop_img <- renderImage({
    list(src = file.path("www", paste0(input$crop_type, ".jpg")),
         width = "100%",
         height = 250)

  },deleteFile = F)

}

shinyApp(ui = ui, server = server)
