library(shiny)


shinyUI(fluidPage(
  

  titlePanel("Next Word Prediction"),
  
  
  sidebarLayout(
    sidebarPanel(
    
       tabsetPanel(type = "tabs", 
             tabPanel("Word Prediction", textInput("text", label = h4("Insert a sentence in the text box below and I will tell you what your next word is...well sometimes!"))
                     # submitButton("SUBMIT")
                      ), 
        
             tabPanel("App info", br(),
                    br(),
                    p("This is a word prediction application, just like the ones you see in  your phone."),
                    br(),
                    p("The app was developed as a capstone for the ",a("Data Science Specialization", href = "https://www.coursera.org/specialization/jhudatascience/1"), "offered by", strong("Johns Hopkins University"), "on", strong("Coursera")," and sponsered by", strong("SwiftKey"),"."),
                    p("The prediction function was developed using ",strong("Ngram"), "model and", strong("Markov chains"),". Four Ngram probability tables were built, unigram, bigram, trigram and quadrigram tables."),
                    br(),
                    p(strong("Stupid backoff"), "smoothing is used to estimate probabilities for words not in the corpus"),
                    br(),
                    p("To use the application insert any valid language sentence in the tex box. The function will predict the most likely next word, in addition to three other likely words."))),
       br(),br(),br(),br(),br(),br(),br(),
       img(src="CourseTrackLogo.jpg", height = 135, width = 240), 
      
       br(),
       em("Prepared as the Capstone for the Data Science Specialization"),
       br(),
       span("By Salaheldin Atabani",style="color:grey")
    ),
    
   
    mainPanel(
      p(h3(strong("SINGLE WORD:", style="color:purple"))),
      br(),
      p(h1(textOutput('text'),style="color:steelblue")),
      br(),
      p(h3(strong("BONUS WORDS:", style="color:purple"))),
       span(h2(textOutput('more1'),style="color:dimgrey"),h2(textOutput('more2'),style="color:dimgrey"),h2(textOutput('more3'),style="color:dimgrey"))
   
      #br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      #img(src="CourseTrackLogo.jpg", height = 135, width = 240),em("Prepared as the Capstone for the Data Science Specialization")
      
      
      )
  )
)
)