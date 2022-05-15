#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyMatrix)
library(sousmarin)
library(MASS)
library(tibble)
library(testit)




# Define UI for application that draws a histogram
ui <- fluidPage(
   # ## For text inputs ##
   #textInput("name", "What's your name?"), #for short text
   #passwordInput("password", "What is your pass"), #for passwords
   #textAreaInput("story", "Tell me something about you"), #for long text
   #
   ### for numeric inputs ##
   ##use slider only for small number cases
   #numericInput("num", "Number one", value = 0, min = 0, max = 100),
   #sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
   #sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),


   #slecet from a limited choices
   #selectInput() and radioButtons()

   #selectInput("Dist", "Distribution", dist),
   #radioButtons("Dist2", "Distribution in button", dist),
   #textInput("name",label=NULL, value="",width=NULL, placeholder = "Your name"),
   #textOutput("greeting")



  headerPanel("First shiny APP"),

  sidebarPanel(
      selectInput("Distribution", "Select Distribution",
                  choices = c("Uniform", "Normal", "Exponential","Gaussian vector")),
      numericInput("Sample", "Echantillon", value = 10),
      conditionalPanel(condition = "input.Distribution == 'Uniform'",
                       numericInput("a","a", value = 0),
                       numericInput("b", "b", value = 1)
                       ),

      conditionalPanel(condition = "input.Distribution == 'Normal'",
                       numericInput("Esperance", "Esperance", 0),
                       numericInput("Variance", "Variance", 1)
                       ),

      conditionalPanel(condition = "input.Distribution == 'Exponential'",
                       numericInput("lambda", "lambda", 1),
      ),
      conditionalPanel(condition="input.Distribution == 'Gaussian vector'",
                       numericInput("vx", "Var(X)", 1),
                       numericInput("vy", "Var(Y)",1 ),
                       numericInput("cv_xy", "Cov(X,Y)", 0.6)
                )
      ),

  mainPanel(
    plotOutput("Myplot"),
    textOutput("txt"),
    tableOutput("tbl")

  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
 # output$greeting = renderText({
 #   paste0("hello ", input$name)
 # })

  output$Myplot = renderPlot({

    dist_type = input$Distribution
    size = input$Sample

    if(dist_type == "Uniform")
    {
        randomvector = runif(size, input$a, input$b)
        hist(randomvector)
    }
    else if(dist_type == "Normal")
    {
      #plot the normal distribution
      mean = input$Esperance
      deviation = sqrt(input$Variance) #ecartype

      normal_f = function(m,e,x){
        return( (1/(e*sqrt(2*pi))) * exp(-(1/2) * ((x-m)/e)^2 ) )
      }

      exp_f = function(lam = 1, c)
      {
        return()
      }
      x_plot = seq(-4*deviation+mean, 4*deviation+mean, by=0.01)

      #Then pick random samples and show them in scatter form
      #We are using the uniform distribution to envelop the normal distribution
      #Going to change it later to put a function in the shiny app instead to envelop.
      #this variable will hold the coordinates of the random sample
      acceptx = c()
      accepty = c()

      counter = 0
      while(counter < size){
        u = runif(1,-4*deviation,4*deviation)
        y = runif(1, 0,normal_f(mean, deviation, mean))
        if(y <= normal_f(mean, deviation, u)){
          acceptx = rbind(acceptx, u)
          accepty = rbind(accepty, y)
          counter = counter + 1
        }
      }
      #plot(acceptx, accepty, xlim=c(-5*deviation+mean, 5*deviation+mean),ylim=c(0,normal_f(mean, deviation, mean)),col="red")
      hist(acceptx, prob="T")
      lines(x_plot, normal_f(mean, deviation, x_plot))
    }else if (dist_type == "Exponential")
      {
        #on utilise l'invers de la fonction de repartition

        lam = input$lambda
        u = runif(size)
        #on échantillon size x selon la dist exp
        rand_x = -log(1-u)/lam
        # pour les y, on prend la ligne vertiacl entre x et lam*exp(-lam *x)
        # et on pren un y uniforme sur la ligne
        rand_y = runif(size, 0, lam*exp(-lam*rand_x))
        #plot(rand_x, rand_y)

        x_plot = seq(0,10,by=0.01)
        hist(rand_x, prob="T")
        lines(x_plot, lam*exp(-lam*x_plot))
         # hist(rand_exp_vector)
    } else if (dist_type == "Gaussian vector") {

      vx <- input$vx
      vy <- input$vy
      cv_xy <- input$cv_xy


      mat_cov <- matrix(c(input$vx, input$cv_xy, input$cv_xy, input$vy), nrow = 2)
      #output$tbl <- renderTable({mat_cov})
      #output$txt <- renderText({as.numeric(mat_cov)})
      #output$tbl <- renderTable({chol(mat_cov)})


      X <- rmvnorm(1000, mat_cov, c(0, 0))
      df <- tibble(x = X[,1], y = X[,2])
      k <- kde2d(df$x, df$y, n = 1000)
      image(k, col = hcl.colors(12, "Blues", rev = TRUE), bty = "n")


    }

  })
}

# Run the application
shinyApp(ui, server)




