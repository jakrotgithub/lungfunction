library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(lme4) # to build linear mixed model  
library(lmerTest) # for outputing test results from the mixed model
library(plyr) #for merging data
library(MuMIn)
library(fBasics)
library(ROCR)
library(pROC)
library(ipw)
library(data.table)

setwd("~/Documents/RStudio projects1/20171008/Lungfunction/Lungfunction_FEV_7_3/")

# source('~/RStudio projects/20171109/problem1/FEV_7_3_functions.R')
# source('~/RStudio projects/20171109/problem1/FEV_7_3_na_inputs_check.R')
source('FEV_7_3_functions.R')
source('FEV_7_3_na_inputs_check.R')


#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    # source('~/RStudio projects/20171109/problem1/FEV_sidebarPanel.R'), #load left sidebar Panel that has the inputs
    source('FEV_7_3_sidebarPanel.R'), #load left sidebar Panel that has the inputs
    

    mainPanel (
      tags$p("lmer summary:"),
      verbatimTextOutput("lmer_summary"),
      width = 5, class = 'rightAlign'
    )
  )
)

server <- function(input, output, session) {

  #load inputs button
  observeEvent(input$load_inputs,{
    #prompt user to select the file to load
    # file_to_load <- file.choose(new=FALSE)
    # file_to_load <- file.choose(new=FALSE)
    # choose.files(default="",caption="Select input CSV files",multi=FALSE,filters = Filters[c("csv", "All"),])
    file_to_load <- choose.files(default="",caption="Select input CSV files",multi=FALSE)
    
    loadedInputs <- read.csv(file_to_load)

    #from the loaded file - get numeric values for the numericInput inputsr
    for (i in 1:(length(loadedInputs$FEV_input_names)-4)) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_num_vals[(i)]) )
    }
    #from the loaded file - get strings for selectInput inputs
    for (i in (length(loadedInputs$FEV_input_names)-3):(length(loadedInputs$FEV_input_names))) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_char_vals[(i)]) )
    }
  })

  #save inputs button
  observeEvent(input$save_inputs,{
    #labels - 1st column in the data frame
    FEV_frame_labels <- FEV_input_labels()
    #numerical values - 2nd column in the data frame
    FEV_frame_num_values <- c(input$trig, #FEV_frame_num_values used to generate data frame column with numeric values only
                              input$hema,
                              input$alb,
                              input$glob,
                              input$alk_phos,
                              input$white_bc,
                              input$qrs,
                              input$alcohol,
                              input$wine,
                              input$cocktail,
                              input$height_square,
                              input$cum_smoke,
                              input$age,
                              input$follow_up_baseline,
                              -999,# input$ba_use
                              -999,# input$dys_exer
                              -999,# input$noc_s
                              -999# input$sex
    )
    #non-numerical/character inputs - 3rd column in the data frame
    FEV_frame_char_values <- c("NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                               "NULL","NULL","NULL",
                               input$ba_use,
                               input$dys_exer,
                               input$noc_s,
                               input$sex
    )
    # FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels, FEV_input_vals=FEV_frame_values)
    FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels,
                                 FEV_input_num_vals=FEV_frame_num_values,
                                 FEV_input_char_vals = FEV_frame_char_values)
    #prompt user to select the file to load
    file_to_save <- choose.files(default="",caption="Save inputs as .csv file",multi=FALSE)
    # userInputsToSave <- read.csv(file_to_save)
    write.csv( FEV_data_frame , file = file_to_save)
  })
  
  #run code, that for every input checks if the value is na
  source('FEV_7_3_na_inputs_check.R')

  #make lmer summary non-reactive --> it is only calculated when the user presses "Run Linear mixed-effects models" button
  output$lmer_summary <- renderPrint({
    if (input$lmer_Submit_button == 0)
       return()
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    #input$lmer_Submit_button
    isolate({
      file_name=BINARY_CODE_FROM_INPUTS(input$age,
                                        input$follow_up_baseline,
                                        input$trig,
                                        input$hema, 
                                        input$alb,
                                        input$glob,
                                        input$alk_phos,
                                        input$white_bc,
                                        input$qrs,
                                        input$alcohol,
                                        input$wine,
                                        input$cocktail,
                                        input$height_square,
                                        input$cum_smoke,
                                        input$sex,
                                        input$ba_use,
                                        input$dys_exer,
                                        input$noc_s
      )
      
      # full_file_name = paste(file_name,".csv")
      full_file_name = paste(paste(file_name,collapse=" "),".rdata") #DK - updated 2017-10-29 because BINARY_CODE_FROM_INPUTS was updated from generating a single character to generating a vector of characters
      #2.0 if RDS file(for given inputs) exists, get lmer_summary from the rts file
      if(file.exists(full_file_name)){
        ptm <- proc.time()
        progress$set(message = "Reading RDS File", value = 0)
        progress$inc(amount=0.99)
        
        # loaded_lmer_fn <- readRDS(full_file_name)
        # load the file with the model and the summary
        # model - lmer_function_output
        # model summary - lmer_function_output_summary
        load(full_file_name)
        
        progress$set(message = "Finished reading RDS File", value = 1.00)
        progress$inc(amount=0.01)
        
        #print summary
        progress$set(message = "Extracting lmer summary", value = 0)
        progress$inc(amount=0.99)
        
        # summary_lmfin <- capture.output({
        #   print(summary(loaded_lmer_fn))
        # })
        summary_lmfin <- lmer_function_output_summary
        
        file_exists_exec_time <- proc.time() - ptm
        file_exists_exec_time
        # browser()
        progress$set(message = "Finished Extracting lmer summary", value = 1.00)
        progress$inc(amount=0.01)
        
        summary_lmfin
        
      }
      else{ #If file does not exist
        ptm <- proc.time()
        #BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('age','follow_up_baseline','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        BINARY_INPUT_VALUES <- c("1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
        BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
        #FACTOR_NAMES_DATAFRAME
        INPUTS <- c('age','follow_up_baseline','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('age','year','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('agecat','year2','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c(NA,'(year|RANDOMID)','triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)

        #
        progress$set(message = "calculating lmer fn", value = 0)
        progress$inc(amount=0.99)
        
        # browser()
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
        
        
        progress$inc(amount=0.01)
        progress$set(message = "Finished calculating lmer fn", value = 1.00)
        
        progress$set(message = "Extracting lmer summary", value = 0)
        progress$inc(amount=0.99)
        lmer_function_output_summary <- summary(lmer_function_output)
        progress$inc(amount=0.01)
        progress$set(message = "Finished Extracting lmer summary", value = 1.00)
          
        progress$set(message = "Saving RDATA file w/ model and summary", value = 0)
        progress$inc(amount=0.99)
        
        # saveRDS(object=lmer_function_output,file = full_file_name,compress=TRUE, refhook = NULL)
        save(lmer_function_output,lmer_function_output_summary,file=full_file_name)
        
        progress$set(message = "Finished Saving RDATA file w/ model and summary", value = 1.00)
        progress$inc(amount=0.01)
        
        # progress$set(message = "Calculating lmer summary", value = 0)
        # progress$inc(amount=0.99)
        
        #print summary
        # summary_lmfin <- capture.output({
        #   print(summary(lmer_function_output))
        # })
        summary_lmfin <- lmer_function_output_summary
        
        file_exists_exec_time <- proc.time() - ptm
        # browse
        
        # progress$inc(amount=0.01)
        # progress$set(message = "Finished Calculating lmer summary", value = 1.00)
        
        summary_lmfin
      }
    }) #end of isolate({...})
  }, width=400)#end of output$lmer_summary <- renderTable
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)