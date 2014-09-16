library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("Distribution of Sample Means"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
      br(),br(),
      checkboxInput("checkbox", label = "Add samples one at a time", value = FALSE),
      br(),br(),      
      sliderInput("n", 
                  "Sample size:", 
                   value = 100,
                   min = 1, 
                   max = 200),
      br(),
      sliderInput("reps", 
                  "Number of repetitions:", 
                  value = 200,
                  min = 1, 
                  max = 1000),
      br(),
      actionButton("resample", label = "Draw New Sample"),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
    ),
    
    mainPanel(
       plotOutput("plot")
    )
  )
))
