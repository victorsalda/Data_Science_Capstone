library(shiny)
library(tm)
library(SnowballC)

# Define server logic required to draw a histogram

shinyServer(function(input, output,session){

  #Load the data base (.csv files) with bigrams and trigrams ordered by frequency.  
  bigrams<-isolate(read.csv("data_frames_bigrams_ordered_app.csv"))
  trigrams<-isolate(read.csv("data_frames_trigrams_ordered_app.csv"))
    
  #Get the text input by the app's user. 
  text_user<-reactive({as.character(input$text_input_bar)})
  
  #Clean the text input by the app's user.
  text_user_cleaned<-reactive({removeNumbers(text_user())})
  text_user_cleaned_2<-reactive({stemDocument(text_user_cleaned())})
  text_user_cleaned_3<-reactive({removeWords(text_user_cleaned_2(),stopwords("en"))})
  text_user_cleaned_4<-reactive({gsub("@|—|–|_|⁄|/", " ", text_user_cleaned_3())})
  text_user_cleaned_5<-reactive({gsub("Ã|œ|Œ|ã|å|â|ã|å|¢|ë|í|€|£|®|°|¿|¾|¡|¯|“|«|‹|›|‘","",text_user_cleaned_4())})
  text_user_cleaned_6<-reactive({gsub("’|“|”|“|•|†|‡|″|“|※|№|Nº|ª|²|‰|‱|″|‴|℗|℠|™|₳|฿|₵|₡","",text_user_cleaned_5())})
  text_user_cleaned_7<-reactive({gsub("₢|₫|₯|₠|ƒ|€|₲|₴|₭|₺|£|ℳ|₥|₦|₧|₱|₰|៛|₹|₨|₪|৳|₸|₮|₩|¥","",text_user_cleaned_6())})
  text_user_cleaned_8<-reactive({removePunctuation(text_user_cleaned_7())})
  text_user_cleaned_9<-reactive({stripWhitespace(text_user_cleaned_8())})
  text_user_cleaned_10<-reactive({tolower(text_user_cleaned_9())})
  
  #Last cleaned words input by user (as regular expression).
  last_word_intup <- reactive(paste(c("^",tail(unlist(strsplit(text_user_cleaned_10(), split = " ")),1),"\\b"),collapse=""))
  
  last_two_words_intup <- reactive(paste(c("^",tail(unlist(strsplit(text_user_cleaned_10(), split = " ")),2)[1]," ",tail(unlist(strsplit(text_user_cleaned_10(), split = " ")),2)[2],"\\b"),collapse=""))
  
  #N-gams in the data base that match last cleaned words input by user. 
  bigrams_last_word_intup<-reactive(bigrams[grep(last_word_intup(), bigrams$bigrams), ][1:5,])
  trigrams_last_two_words_intup<-reactive(trigrams[grep(last_two_words_intup(), trigrams$trigrams), ][1:5,])
 
  # Predicted word based on trigrams' decreasing probability.
  word_bigram_prob_1<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[1],split = " ")[[1]][2])
  word_bigram_prob_2<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[2],split = " ")[[1]][2])
  word_bigram_prob_3<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[3],split = " ")[[1]][2])
  word_bigram_prob_4<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[4],split = " ")[[1]][2])
  word_bigram_prob_5<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[5],split = " ")[[1]][2])
  word_bigram_prob_6<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[6],split = " ")[[1]][2])
  word_bigram_prob_7<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[7],split = " ")[[1]][2])
  word_bigram_prob_8<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[8],split = " ")[[1]][2])
  word_bigram_prob_9<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[9],split = " ")[[1]][2])
  word_bigram_prob_10<-reactive(strsplit(as.character(bigrams_last_word_intup()[grep(last_word_intup(), bigrams_last_word_intup()$bigrams), ][1:10,][1][,1])[10],split = " ")[[1]][2])
  
    
  # Predicted word based on trigrams' decreasing probability.
  word_trigram_prob_1<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[1],split = " ")[[1]][3])
  word_trigram_prob_2<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[2],split = " ")[[1]][3])
  word_trigram_prob_3<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[3],split = " ")[[1]][3])
  word_trigram_prob_4<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[4],split = " ")[[1]][3])
  word_trigram_prob_5<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[5],split = " ")[[1]][3])
  word_trigram_prob_6<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[6],split = " ")[[1]][3])
  word_trigram_prob_7<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[7],split = " ")[[1]][3])
  word_trigram_prob_8<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[8],split = " ")[[1]][3])
  word_trigram_prob_9<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[9],split = " ")[[1]][3])
  word_trigram_prob_10<-reactive(strsplit(as.character(trigrams_last_two_words_intup()[grep(last_two_words_intup(), trigrams_last_two_words_intup()$trigrams), ][1:10,][1][,1])[10],split = " ")[[1]][3])
  
  # output$word_1 <- renderText({ 
  #   word_bigram_prob_1()
  # })
  # output$word_2 <- renderText({ 
  #   word_bigram_prob_2()
  # })
  # output$word_3 <- renderText({ 
  #   word_bigram_prob_3()
  # })
  # output$word_4 <- renderText({ 
  #   word_trigram_prob_1()
  # })
  # output$word_5 <- renderText({ 
  #   word_trigram_prob_2()
  # })
  # output$word_6 <- renderText({ 
  #   word_trigram_prob_3()
  # })
  
  output$button_word_1 <- renderUI({
    if (!is.na(word_trigram_prob_1())){
    button_word_1 <- word_trigram_prob_1()
    actionButton(button_word_1, button_word_1,width=100)
    }else if (!is.na(word_bigram_prob_1())){
    button_word_1 <- word_bigram_prob_1()
    actionButton(button_word_1, button_word_1,width=100)
    }else{
      button_word_1 <- sample(c("the","be","to","of"),1)
      actionButton(button_word_1, button_word_1,width=100)
    }
  })
  output$button_word_2 <- renderUI({
    if (!is.na(word_trigram_prob_2())){
      button_word_2 <- word_trigram_prob_2()
      actionButton(button_word_2, button_word_2,width=100)
    }else if (!is.na(word_bigram_prob_2())){
      button_word_2 <- word_bigram_prob_2()
      actionButton(button_word_2, button_word_2,width=100)
    }else{
      button_word_2 <- sample(c("and","a","in","that"),1)
      actionButton(button_word_2, button_word_2,width=100)
    }
  })
  output$button_word_3 <- renderUI({
    if (!is.na(word_trigram_prob_3())){
      button_word_3 <- word_trigram_prob_3()
      actionButton(button_word_3, button_word_3,width=100)
    }else if (!is.na(word_bigram_prob_3())){
      button_word_3 <- word_bigram_prob_3()
      actionButton(button_word_3, button_word_3,width=100)
    }else{
      button_word_3 <- sample(c("have","i","it","for"),1)
      actionButton(button_word_3, button_word_3,width=100)
    }
  })
  output$button_word_4 <- renderUI({
    if (!is.na(word_trigram_prob_4())){
      button_word_4 <- word_trigram_prob_4()
      actionButton(button_word_4, button_word_4,width=100)
    }else if (!is.na(word_bigram_prob_4())){
      button_word_4 <- word_bigram_prob_4()
      actionButton(button_word_4, button_word_4,width=100)
    }else{
      button_word_4 <- sample(c("not","on","with","he"),1)
      actionButton(button_word_4, button_word_4,width=100)
    }
  })
  output$button_word_5 <- renderUI({
    if (!is.na(word_trigram_prob_5())){
      button_word_5 <- word_trigram_prob_5()
      actionButton(button_word_5, button_word_5,width=100)
    }else if (!is.na(word_bigram_prob_5())){
      button_word_5 <- word_bigram_prob_5()
      actionButton(button_word_5, button_word_5,width=100)
    }else{
      button_word_5 <- sample(c("as","you","do","at"),1)
      actionButton(button_word_5, button_word_5,width=100)
    }
  })
  output$button_word_6 <- renderUI({
    if (!is.na(word_trigram_prob_6())){
      button_word_6 <- word_trigram_prob_6()
      actionButton(button_word_6, button_word_6,width=100)
    }else if (!is.na(word_bigram_prob_6())){
      button_word_6 <- word_bigram_prob_6()
      actionButton(button_word_6, button_word_6,width=100)
    }else{
      button_word_6 <- sample(c("the","be","to","of"),1)
      actionButton(button_word_6, button_word_6,width=100)
    }
  })
  output$button_word_7 <- renderUI({
    if (!is.na(word_trigram_prob_7())){
      button_word_7 <- word_trigram_prob_7()
      actionButton(button_word_7, button_word_7,width=100)
    }else if (!is.na(word_bigram_prob_7())){
      button_word_7 <- word_bigram_prob_7()
      actionButton(button_word_7, button_word_7,width=100)
    }else{
      button_word_7 <- sample(c("and","a","in","that"),1)
      actionButton(button_word_7, button_word_7,width=100)
    }
  })
  output$button_word_8 <- renderUI({
    if (!is.na(word_trigram_prob_8())){
      button_word_8 <- word_trigram_prob_8()
      actionButton(button_word_8, button_word_8,width=100)
    }else if (!is.na(word_bigram_prob_8())){
      button_word_8 <- word_bigram_prob_8()
      actionButton(button_word_8, button_word_8,width=100)
    }else{
      button_word_8 <- sample(c("have","i","it","for"),1)
      actionButton(button_word_8, button_word_8,width=100)
    }
  })
  output$button_word_9 <- renderUI({
    if (!is.na(word_trigram_prob_9())){
      button_word_9 <- word_trigram_prob_9()
      actionButton(button_word_9, button_word_9,width=100)
    }else if (!is.na(word_bigram_prob_9())){
      button_word_9 <- word_bigram_prob_9()
      actionButton(button_word_9, button_word_9,width=100)
    }else{
      button_word_9 <- sample(c("not","on","with","he"),1)
      actionButton(button_word_9, button_word_9,width=100)
    }
  })
  output$button_word_10 <- renderUI({
    if (!is.na(word_trigram_prob_10())){
      button_word_10 <- word_trigram_prob_10()
      actionButton(button_word_10, button_word_10,width=100)
    }else if (!is.na(word_bigram_prob_10())){
      button_word_10 <- word_bigram_prob_10()
      actionButton(button_word_10, button_word_10,width=100)
    }else{
      button_word_10 <- sample(c("as","you","do","at"),1)
      actionButton(button_word_10, button_word_10,width=100)
    }
  })
  observe({
    # This will change the value of input$inText, based on x
    updateTextInput(session, "text_input_bar", value = paste(input$text_input_bar))
  })
})