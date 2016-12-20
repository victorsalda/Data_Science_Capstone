library(shiny)
library(tm)
library(SnowballC)

shinyUI(fluidPage(
                  # Info.
                  helpText(h2(strong("Capstone Project: NLP. (v1.0)"))),
                  helpText(h3(strong("Victor D. Saldaña C."))),
                  helpText(h5(strong("PhD(c) in Geoinformatics Engineering"))),
                  helpText(h5(strong("Technical University of Madrid (Spain)"))),
                  helpText(a("https://www.linkedin.com/in/victorsalda", href="https://www.linkedin.com/in/victorsalda")),
                  helpText(a("https://github.com/victorsalda", href="https://github.com/victorsalda")),
                  helpText(a("http://rpubs.com/victorsalda", href="http://rpubs.com/victorsalda")),
                  helpText(" "),
                  helpText(h5(strong("December, 2016"))),
                  
                  # Application description.
                  div(helpText("This web app was designed to", strong("predict the next 10 words"),"after typing
                           a sentence. To evaluate a new sentence and predict the next word just erase
                           the first one (that is an example for the sentence “Wellcome to”) and press the
                           summit button. Then, ten new words option will be displayed in the same number of
                           action buttons. These buttons don't execute any action (right now) but they
                           were put for next versions of the app. Technical details may 
                           be found at", a("Link 1", href="http://rpubs.com/victorsalda/234366"),"and",
                                      a("Link 2.", href="https://github.com/victorsalda/Data_Science_Capstone.git"),
                               strong("BE PATIENT AND WAIT SOME SECONDS UNTIL THE APP STARTS (PREDICTED WORDS ARE DISPLAYED)
                                      ...ENJOY...",style = "color:red"))),
                  
                  # Sidebar with a slider input for number of bins. 
                  sidebarLayout(sidebarPanel(width = 12,
                                             h3("Text prediction panel",align = "center"),
                                             textInput("text_input_bar", "Input your text:", "Wellcome to"),
                                             submitButton("Submit"),
                                             h4("predicted words:",align = "left"),
                                             # textOutput("word_1"),
                                             # textOutput("word_3"),
                                             # textOutput("word_2"),
                                             # textOutput("word_4"),
                                             # textOutput("word_5"),
                                             # textOutput("word_6"),
                                             div(style="display:inline-block",uiOutput("button_word_1")),
                                             div(style="display:inline-block",uiOutput("button_word_2")),
                                             div(style="display:inline-block",uiOutput("button_word_3")),
                                             div(style="display:inline-block",uiOutput("button_word_4")),
                                             div(style="display:inline-block",uiOutput("button_word_5")),
                                             div(style="display:inline-block",uiOutput("button_word_6")),
                                             div(style="display:inline-block",uiOutput("button_word_7")),
                                             div(style="display:inline-block",uiOutput("button_word_8")),
                                             div(style="display:inline-block",uiOutput("button_word_9")),
                                             div(style="display:inline-block",uiOutput("button_word_10"))
                                             # uiOutput("button_word_2"),
                                             # uiOutput("button_word_3"),
                                             # uiOutput("button_word_4"),
                                             # uiOutput("button_word_5")
                                             ),
                                mainPanel(textOutput("text_user")
                                          )
                                )
                  )
)