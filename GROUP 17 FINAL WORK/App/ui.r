library(shiny)
library(scales)
library(tm)
library(wordcloud)
library(SnowballC)
library(dplyr)
library(reshape2)
library(tidytext)
library(syuzhet)
library(pander)
library(xlsx)
library(ggplot2)
library(partykit)
library(DT)
library(shinydashboard)
library(hunspell)
library(textclean)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Product Review Analysis System", titleWidth = 350),
                    dashboardSidebar(
                      sidebarMenu(
                        ##Tab One
                        ##Contact:
                        menuItem("About",tabName = "about",icon = icon("clipboard")),
                        menuItem("File Upload",tabName = "file",icon = icon("file-text-o")),
                        ##Tab Two
                        menuItem("Text Output",tabName = "text",icon = icon("file-text-o")),
                        ##Grammar
                        menuItem("Grammar Check",tabName = "gr",icon = icon("file-text-o")),
                        ##Tab WordBreakdown
                        menuItem("Grammar check continuaton...",tabName = "wc",icon = icon("table")),
                        ##Tab Three
                        menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
                        ##Tab Four
                        menuItem("Word Count Bar Plot",tabName = "barplot",icon = icon("bar-chart-o")),
                        ##Tab Five
                        menuItem("Emotional Sentiment",tabName = "emotionalsentiment",icon = icon("bar-chart-o")),
                        ##Tab Six
                        menuItem(paste("Positive vs. Negative Sentiment"),tabName = "pnsentiment",icon = icon("bar-chart-o")),
                        ##Word Tokenizer
                        menuItem("Word Tokenizer",tabName = "wt",icon = icon("file-text-o")),
                        ##Character Tokenizer
                        menuItem("Character Tokenizer",tabName = "ct",icon = icon("file-text-o")),
                        ##References
                        menuItem("References",tabName = "ref",icon = icon("file-text-o")),
                        ##Rating
                        menuItem("Rating",tabName = "rate",icon = icon("bar-chart-o"))
                        
                      )),
                    
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "gr",
                        box(title = "Prediction Example:",width = 12,status = "warning",solidHeader = T,collapsible = FALSE,
                            textInput("missample", strong("Enter a statement or word below and click the button below to detect or predict misspelled word(s) and a suggestion")),
                            textOutput("misword"),
                            actionButton("missamplebutton", "Check spelling")
                                ),
                        box(title = "Table of Misspelled words",width = 12,status = "warning",solidHeader = T,collapsible = FALSE,
                            textOutput("mistable")
                        ),
                        box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Frequency of Misspelled words",
                            textOutput("freqmisp")
                        )
                                
                        ),
                        tabItem(tabName = "wc",
                                helpText(paste("This tab allows you to display the frequency of misspelled words in the uploaded csv file. "),
                                         br(),
                                         paste("It also includes some functionalities about Grammar.")
                                ),
                                br(),
                                verbatimTextOutput("nomisp"),
                                verbatimTextOutput("punc"),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Plot",
                                  
                                    br(),
                                    plotOutput("puncplot")
                                                )
                              
                                ),
                                 
                        tabItem(tabName = "rate",
                                  helpText(paste("This tab displays all visuals based on ratings. Click on the plus(+) sign to view the visuals."),
                                br(),
                                br(),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = T, title = "Distribution of star ratings across products",collapsed = T,
                                  paste("The Visual below shows that most of the products are of star rating 5."),
                                  plotOutput("rategg")
                                )
                                  
                                
                                ),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = T,title = "Plot of Review Length by Rating",collapsed = T,
                                    paste("The Visual below shows that products with higher rating have longer review length while those with lower review length have lower rating."),
                                    plotOutput("ratingvswordlength")
                                    ),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = T,title = "Frequency of words with spelling errors by rating ",collapsed = T,
                                    paste("The Visual below shows that punctuation and spelling donot affect product rating."),
                                    plotOutput("spellvsrating")
                                )
                                ),
                        tabItem(tabName = "wt",
                                helpText(paste("This tab displays distribution of Words per Review."),
                                         br(),
                                         br(),
                                         box( width = 12,status = "warning",solidHeader = T,collapsible = T,title = "Plot",
                                              
                                              paste(" Each number represents the number of words in a particular review in a row in the Uploaded file."),
                                              br(),
                                           textOutput("wordfreq")
                                           ),
                                         box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Word frequency plot",
                                             plotOutput("wordfreqplot")
                                             )
                                )),
                        tabItem(tabName = "ct",
                                helpText(paste("This tab displays distribution of characters per review."),
                                         br(),
                                         br()
                                ),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Character frequency plot",
                                  plotOutput("charactersinwordsplot")
                                    )
                                
                                ),
                        tabItem(tabName = "file",
                                fileInput("file", "Upload CSV file", accept=c("text/plain", ".csv")),
                                helpText(paste("Please upload a CSV file with the text", 
                                               "you would like to analyze.")
                                )),
                        tabItem(tabName = "text",
                                helpText(paste("This tab displays the uploaded csv file.")),
                                br(),
                                br(),
                                DT::dataTableOutput("data")),
                        tabItem(tabName = "barplot",
                                helpText(paste("This tab allows you to display the frequency of words in the uploaded csv file "),
                                         br(),
                                         paste("via a bar chart. The bar chart by default displays the first through 20"),
                                         br(),
                                         paste("most frequent words in the text.")),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = T,title = "Frequency of words ",
                                    
                                    textOutput("freqtable")
                                ),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Plot showing frequency of words ",
                                  
                                  plotOutput("freq")
                                    
                                    )
                                
                                ),
                        tabItem(tabName = "wordcloud",
                                fluidRow(
                                    box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,
                                      helpText(paste("The minimum frequency refers to the minimum number of times"),
                                               br(),
                                               paste("the word needs to appear in the uploaded text to be included in the wordcloud.")),
                                      # Slider input for frequency change
                                      sliderInput("slider1", "Minimum Frequency:",
                                                  min = 1, max = 50, value = 5),
                                      helpText(paste("The maximum number of words refers to the maximum number of words"),
                                               br(),
                                               paste("you want to appear in the wordcloud that is created.")),
                                      # Slider input for number of words change
                                      sliderInput("slider2", "Max words:",
                                                  min = 10, max = 1000, value = 100),
                                      actionButton(inputId = "click", "Create Word cloud")
                                    ),
                                    box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Word cloud",
                                      # Wordcloud image rendered
                                      plotOutput("wc"))
                                  )),
                        tabItem(tabName = "emotionalsentiment",
                                
                                helpText(paste("This tab allows you to calculate eight types of emotion present within the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The following types of emotion are calculated:"),
                                         br(),
                                         br(),
                                         paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                         br(),
                                        
                                 paste("Each bar represents the overall percentage of each emotion present within the uploaded text file.")),
                                tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                br(),
                                br(),
                                br(),
                                box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Prediction Example:",
                                    br(),
                                    textInput("sentiment","Enter a statement in the input box below and click on the button below it to predict the Emotions in it"),
                                    actionButton("sentimentbutton","My sentiment Analysis"),br(),
                                    sliderInput("senttext","Adjust the number rows used for Sentiment Analysis",min = 1, max = 71000, value = 10),
                                    textOutput("sentimentsummary")
                                    ),
                                actionButton("sentimentcsv", "Sentiment Analysis from CSV file"),
                                plotOutput("sentiment")),
                        tabItem(tabName = "pnsentiment",
                                helpText(paste("This tab allows you to calculate the positive and negative sentiment present within the uploaded text."),
                                         paste("The following sentiments are calculated:"),
                                         br(),
                                         br(),
                                         tags$b(paste("Positive & Negative")),
                                         br(),
                                         paste("The bar graphs displayed are in relation to the percentage of positive and negative words present in the uploaded text."),
                                         
                                         box(width = 12,status = "warning",solidHeader = T,collapsible = FALSE,title = "Polarity Graph",
                                             actionButton("pol","Click to view Polarity from Uploaded file"), br(),
                                             sliderInput("Poltext","Adjust the number rows used for Polarity Analysis", min = 1, max = 71000, value = 10),
                                             plotOutput("polarity")
                                             
                                         )
                                         )
                      
                                ),
                        tabItem(tabName = "ref",
                                helpText(strong("References :"),
                                         br(),
                                         br(),
                                         paste("Cashell, D. (2014)."),em("Social media sentiment analysis using data mining techniques"),paste(". National 	College of Ireland."),
                                         br(),
                                         br(),
                                         paste("Hennessey, A. (2014)."),em("Sentiment analysis of twitter: using knowledge based and machine learning techniques"),paste(". National College of Ireland."),
                                         br(),
                                         br(),
                                         paste("Jockers, M. (2016)."),em("Introduction to the syuzhet package"),paste(".Retrieved from:"),a("https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Mohammad, S. (2013)."),em("NRC word-emotion association lexicon (aka emolex)"),paste(".Retrieved from:"),a("http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Mullen. (2014)."),em("Introduction to sentiment analysis"),paste(".Retrieved from:"),a("https://lct-master.org/files/MullenSentimentCourseSlides.pdf",href="https://lct-master.org/files/MullenSentimentCourseSlides.pdf",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Robinson, D. (2016)."),em("Text analysis of trump's tweets confirms he writes only the angrier android half"),paste(".Retrieved from:"),a("http://varianceexplained.org/r/trump-tweets/",href= "http://varianceexplained.org/r/trump-tweets/",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Smith, D. (2015)."),em("Comparing subreddits, with latent semantic analysis in r"),paste(". Retrieved from:"),a("http://blog.revolutionanalytics.com/2017/03/comparing-subreddits.html",href="http://blog.revolutionanalytics.com/2017/03/comparing-subreddits.html",target="_blank"))),
                        tabItem(tabName = "about",
                                helpText(
                                  strong(h1("                                     Product Review Analysis System")),
                                  paste("Product Review Analysis system is a web based system that will analyse customer reviews from over 1000 products provided by different online markets."), br(),
                                  paste("PRAS is able to assess how writing quality impacts positive and negative online product reviews such as analyzing reviews for grammatical errors, frequency of particular words, and product ratings."),
                                  
                                  h2("Goals"),
                                  paste("The PRAS aims to analyse Grammar on the uploaded csv file."),br(),
                                  paste("The PRAS aims to analyse the Product Rating."),
                                  
                                  h2("Objectives"),
                                  
                                  paste("System user should be able to upload a csv file containing data to analyse."),br(),
                                  paste("The system should be able to identify whether a review has Punctuation end mark e.g. full stop, question mark and Exclamation mark."),br(),
                                  paste("The system should be able to identify misspelled words in a review and displays the results in table form."),br(),
                                  paste("The system should be able to do word count on the data file and return the results in a nicely formatted table."),br(),
                                  paste("The system should be able to calculate eight types of emotions for example Anger, Joy, Disgust, Anticipation, Sadness, Surprise, Fear and Trust present within the uploaded data file."),br(),
                                  paste("The system should be able to deduce the polarity of a given review i.e.  if a review is Negative or Positive."),br(),
                                  paste("The system should be able to be display results in Visual form."),br(),
                                  paste("The system should able to show the distribution of rating across products."),br(),
                                  paste("The system should able to show a visual of frequency of words with spelling errors against rating."),
                                  br(),
                                  h4(strong("About the developers")),
                                  paste("The Product Review Analysis was developed by a team of four students pursing a Bachelors in Software Engineering 
                                    at Makerere University."), br(),
                                  paste("The students put together the knowledge obtained from their Data science lectures to create this system."),
                                  paste("The developers are:"),br(),
                                  paste("KIGGUNDU ISMAIL SSALI", "Email: salimkalamity9@gmail.com" ,sep = "---"), br(),
                                  paste("NUWASIIMA BRENDA ","Email: nuwabrenda@gmail.com",sep = "---"), br(),
                                  paste("ABUBAKARI SIMBA" ,"Email: bakarsimba1997@gmail.com",sep = "---"), br(),
                                  paste("AYESIGA TONY NSUBUGA", "Email: ayesiga47@gmail.com",sep = "---")
                        
                                )
                        )
                      ))
)