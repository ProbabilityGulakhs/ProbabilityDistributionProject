# Load packages
library(shiny)
library(shinythemes)
library("ggplot2")


ui <- fluidPage(
  
  navbarPage(
    
    theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
    
    "",
    
    tabPanel("Home",
             
             sidebarPanel(
               br(),br(),br(),
               h2("discrete and continues distribution"),br(),br(),
               h3("designed to generate numbers \br and plot distributions"),br(),
               h3("in distribution tab, you can choose your distribution"),br(),br(),br(),br(),br(),br()
               
             ),
             
             mainPanel(h3("our team:"),
                       
                       tabsetPanel(
                         tabPanel("Ali",
                                  mainPanel(
                                    h1(" "),
                                    h3("Ali gorji"),
                                    h4("94105825"),
                                    h1(" "),
                                    h4("email:"),
                                    h4("alli.gorji@gmail.com")
                                  ),
                                  imageOutput("ali")
                         ),
                         tabPanel("Javad", 
                                  mainPanel(
                                    h1(" "),
                                    h3("Javad Abdi"),
                                    h4("95106437"),
                                    h1(" "),
                                    h4("email:"),
                                    h4("abdijavad110@gmail.com")
                                  ),
                                  imageOutput("javad")
                         ),
                         tabPanel("Abolhassan", 
                                  mainPanel(
                                    h1(" "),
                                    h3("Abolhassan Eslami"),
                                    h4("95109926"),
                                    h1(" "),
                                    h4("email:"),
                                    h4("abolhassaneslami@gmail.com")
                                  ),
                                  imageOutput("hassan")
                         )
                         
                       )
                       
             )
             
    ),
    
    tabPanel("Distributions", 
             tabsetPanel(
               tabPanel("Uniform",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated between two below numbers"),br(),br(),
                            textInput("uniform_number_1", "number 1", "0"),
                            textInput("uniform_number_2", "number 2", "10"),
                            h4(textOutput("uniform_rand")),br(),
                            actionButton("un_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("un_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                            fluidRow(column(8,
                                    plotOutput("uniformplot"),
                                    offset = 2)
                              )))),
               tabPanel("Bernouli",
                        sidebarLayout(
                         sidebarPanel(
                           br(),h4("random number will be generated according to paramater"),br(),br(),
                           textInput("br_number_1", "bernouli parameter", "0.5"),
                           h4(textOutput("br_rand")),br(),
                           actionButton("br_gen", "generate", class = "btn-primary",width = '100%'),
                           h5(""),
                           actionButton("br_plot", "plot", class = "btn-primary",width = '100%')
                           ),
                         mainPanel(br(),br(),br(),br(),
                           fluidRow(column(8,
                                    plotOutput("brplot"),
                                    offset = 2)
                           )))),
               tabPanel("Binomial",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated according to probability paramater and number of trials"),br(),br(),
                            textInput("bi_number_1", "binomial probability parameter", "0.5"),
                            textInput("bi_number_2", "number of trials", "100"),
                            h4(textOutput("bi_rand")),br(),
                            actionButton("bi_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("bi_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                                    fluidRow(column(8,
                                                    plotOutput("biplot"),
                                                    offset = 2)
                                    )))),
               tabPanel("Geometric",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated according to paramater"),br(),br(),
                            textInput("ge_number_1", "geometric parameter", "0.5"),
                            h4(textOutput("ge_rand")),br(),
                            actionButton("ge_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("ge_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                                    fluidRow(column(8,
                                                    plotOutput("geplot"),
                                                    offset = 2)
                                    )))),
               tabPanel("Exponential",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated according to exponential distribution"),br(),br(),
                            textInput("exp_number_1", "lambda parameter", "3"),
                            h4(textOutput("exp_rand")),br(),
                            actionButton("exp_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("exp_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                                    fluidRow(column(8,
                                                    plotOutput("expplot"),
                                                    offset = 2)
                                    )))),
               tabPanel("Gama",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated according to gamma distribution"),br(),h5("beacuse gamma is sumation of k exponential, you should enter an lambda parameter for exponential too"),br(),br(),
                            textInput("ga_number_1", "lambda parameter", "1"),
                            textInput("ga_number_2", "number of exponentials", "5"),
                            h4(textOutput("ga_rand")),br(),
                            actionButton("ga_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("ga_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                                    fluidRow(column(8,
                                                    plotOutput("gaplot"),
                                                    offset = 2)
                                    )))),
               tabPanel("Poisson",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated according to poisson distribution"),br(),h5("beacuse poisson underlying exponential distribution, you should enter an lambda parameter for exponential too"),br(),br(),
                            textInput("po_number_1", "lambda parameter", "1"),
                            textInput("po_number_2", "time interval", "10"),
                            h4(textOutput("po_rand")),br(),
                            actionButton("po_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("po_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                                    fluidRow(column(8,
                                                    plotOutput("poplot"),
                                                    offset = 2)
                                    )))),
               tabPanel("Normal",
                        sidebarLayout(
                          sidebarPanel(
                            br(),h4("random number will be generated according to normal distribution"),br(),br(),
                            textInput("no_number_1", "mean", "0"),
                            textInput("no_number_2", "variance", "5"),
                            h4(textOutput("no_rand")),br(),
                            actionButton("no_gen", "generate", class = "btn-primary",width = '100%'),
                            h5(""),
                            actionButton("no_plot", "plot", class = "btn-primary",width = '100%')
                          ),
                          mainPanel(br(),br(),br(),br(),
                                    fluidRow(column(8,
                                                    plotOutput("noplot"),
                                                    offset = 2)
                                    ))))
             )
    ),
    
    tabPanel("Info", ""
             
    )
    
  )
  
)

server <- function(input, output) {
  
  #pics
  output$ali <- renderImage({list(src = "ali.jpg",contentType = 'image/jpg',width = 300,height = 500,alt = "ali gorji pic")},deleteFile = FALSE)
  output$javad <- renderImage({list(src = "javad.jpg",contentType = 'image/jpg',width = 300,height = 500,alt = "javad abdi pic")},deleteFile = FALSE)
  output$hassan <- renderImage({list(src = "hasan.jpg",contentType = 'image/jpg',width = 300,height = 500,alt = "ali eslami pic")},deleteFile = FALSE)
  
  #uniform
  output$uniformplot <- renderPlot({
    input$un_plot
    f2(as.numeric(input$uniform_number_1),as.numeric(input$uniform_number_2))
    })
  output$uniform_rand <- renderText({
    input$un_gen
    paste("random number is: ",dugen(as.numeric(input$uniform_number_1),as.numeric(input$uniform_number_2)))
    })
  
  #bernouli
  output$brplot <- renderPlot({
    input$br_plot
    f4(as.numeric(input$br_number_1))
  })
  output$br_rand <- renderText({
    input$br_gen
    paste("random number is: ",brgen(as.numeric(input$br_number_1)))
  })
  
  #binomial
  output$biplot <- renderPlot({
    input$bi_plot
    f5(as.numeric(input$bi_number_1),as.numeric(input$bi_number_2))
  })
  output$bi_rand <- renderText({
    input$bi_gen
    paste("random number is: ",bigen(as.numeric(input$bi_number_1),as.numeric(input$bi_number_2)))
  })
  
  #geometric
  output$geplot <- renderPlot({
    input$ge_plot
    f6(as.numeric(input$ge_number_1))
  })
  output$ge_rand <- renderText({
    input$ge_gen
    paste("random number is: ",gegen(as.numeric(input$ge_number_1)))
  })
  
  #exponantial
  output$expplot <- renderPlot({
    input$exp_plot
    f7(as.numeric(input$exp_number_1))
  })
  output$exp_rand <- renderText({
    input$exp_gen
    paste("random number is: ",expgen(as.numeric(input$exp_number_1)))
  })
  
  #gamma
  output$gaplot <- renderPlot({
    input$ga_plot
    f8(as.numeric(input$ga_number_1),as.numeric(input$ga_number_2))
  })
  output$ga_rand <- renderText({
    input$ga_gen
    paste("random number is: ",gagen(as.numeric(input$ga_number_1),as.numeric(input$ga_number_2)))
  })
  
  #poisson
  output$poplot <- renderPlot({
    input$po_plot
    f9(as.numeric(input$po_number_1),as.numeric(input$po_number_2))
  })
  output$po_rand <- renderText({
    input$po_gen
    paste("random number is: ",pogen(as.numeric(input$po_number_1),as.numeric(input$po_number_2)))
  })
  
  #normal
  output$noplot <- renderPlot({
    input$no_plot
    f10(as.numeric(input$no_number_1),as.numeric(input$no_number_2))
  })
  output$no_rand <- renderText({
    input$no_gen
    paste("random number is: ",nogen(as.numeric(input$no_number_1),as.numeric(input$no_number_2)))
  })
  
  
}










#1
# we are using Linear congruential generator its better not to change the parameters
regnrator <- function(m = 2 ^ 48, a = 25214903917, c = 11, floor = 0,  roof = 1, n = 1, Integer = FALSE){
  
  distance <- roof - floor
  
  #seed(using system time for most randomness):
  seed <- as.numeric(Sys.time()) * 1000
  
  #output numbers:
  numbers <- vector(length = n)
  
  seed <- (a * seed + c) %% m
  for(i in 1:n){
    seed <- (a * seed + c) %% m
    numbers[i] <- (seed / m) * distance + floor
  }
  
  if(Integer)
    for(i in 1:n)
      numbers[i] <- as.integer(numbers[i])
  
  return(numbers)
  
}
#2
dugen <- function(a,b){
  s = min(a,b)
  s2 = max(a,b)
  return(regnrator(n = 1,floor = s,roof = s2))
}
#3
cugen <- function(){
  return(dugen(1,0))
}
#4
brgen <- function(p)
{
  temp <- cugen()
  if(temp < p)
    return(1)
  else
    return(0)
}
#5
bigen <- function(p, n)
{
  temp2 <- 1
  for(i in 1:n)
    temp2 <- temp2 + brgen(p)
  temp2 <- temp2 - 1
  return(temp2)
}
#6
gegen <- function(parameter){
  
  i <- 0
  result <- brgen(parameter) 
  
  while(result == 0){
    result <- brgen(parameter)
  }
  result <- brgen(parameter)
  
  while(result == 0){
    i = i+1
    result <- brgen(parameter)
  }
  return(i)
}
#7
expgen <- function(lambda){
  uni <- cugen()
  exp <- (-1.0/lambda)*log(uni)
  return(exp)
}

#8
gagen <- function(exp_parameter, k){
  sum <- 0
  for(i in 1:k){
    sum <- sum + expgen(exp_parameter)
  }
  return(sum)
}

# 9
pogen <- function(ex,t = 1){
  i = 0;
  sum = expgen(ex)
  while(sum < t)
  {
    sum = sum + expgen(ex)
    i = i + 1 
  }
  return(i)
}


#10
nogen <- function(u, s){
  pois <- pogen(s) #pois = poisson(sigma) ~ N(sigma, sigma)
  normal <- pois - s + u #normal = N(u, sigma) as CLT does
  return(normal)
}

#11

#part 1
f1 <- function(){
  # numbers = 100 , between 0 to 1
  hist(regnrator(n=100), ylab = "count", col = colors())
  # numbers = 1000 , 1 to 10
  hist(regnrator(n=1000, floor = 1, roof = 10), ylab = "count", col = colors())
  # numbers = 1000 , 1 to 10 , Integer = TRUE
  qplot(regnrator(n=1000, floor = 1, roof = 10, Integer = TRUE), ylab = "count")
}

#part 2

f2 <- function(a,b){
  s = 1
  c = c(dugen(a,b))
  k = dugen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = dugen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  uniform = c
  return(hist(uniform,ylab = "count",col = colors()))
}
#part 3
f3 <- function(){
  s = 1
  c = c(cugen())
  k = cugen()
  while (s < 1000){
    while (k == c[s]){
      k = cugen()
    }
    c = c(c,k)
    s = s + 1
  }
  uniform_between_zero_and_one = c
  hist(uniform_between_zero_and_one, ylab = "count", col = colors())
}

#part 4
f4 <- function(a){
  s = 1
  c = c(brgen(a))
  while (s < 1000){
    c = c(c,brgen(a))
    s = s + 1
  }
  qplot(c, ylab = "count")
}
#part 5
f5 <- function(a,b){
  s = 1
  c = c(bigen(a,b))
  while (s < 1000){
    c = c(c,bigen(a,b))
    s = s + 1
  }
  Binomial = c
  hist(Binomial, ylab = "count", col = colors())
}

#part 6
f6 <- function(a){
  s = 1
  c = c(gegen(a))
  while (s < 1000){
    c = c(c,gegen(a))
    s = s + 1
  }
  Geometric = c
  qplot(Geometric, ylab = "count")
}
#part 7
f7 <- function(a){
  s = 1
  c = c(expgen(a))
  k = expgen(a)
  while (s < 1000){
    while (k == c[s]){
      k = expgen(a)
    }
    c = c(c,k)
    s = s + 1
  }
  hist(c, ylab = "count", col = colors())
}
#part 8
f8 <- function(a,b){
  s = 1
  c = c(gagen(a,b))
  k = gagen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = gagen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  bins <- seq(floor(min(c)),ceiling(max(c)),0.5)
  gamma = c
  qplot(gamma, breaks = bins, ylab = "count")
}
#part 9
f9 <- function(a,b){
  s = 1
  c = c(pogen(a,b))
  k = pogen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = pogen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  Poisson = c
  bins = seq(0,100,1)
  qplot(Poisson, breaks = bins, ylab = "count",xlim = c(0,100))
}
# part 10
f10 <- function(a,b){
  s = 1
  c = c(nogen(a,b))
  k = nogen(a,b)
  while (s < 3000){
    #while (k == c[s]){
      k = nogen(a,b)
    #}
    if(s%%2 == 0&k>=0) {
      k=-k
      c = c(c,k)
    }else if(k>0){
      c = c(c,k)
    }
    s = s + 1
  }
  Normal = c
  bins = seq(-100,100,1)
  qplot(Normal, breaks = bins, ylab = "count",xlim = c(-100,100))
}







shinyApp(ui, server)