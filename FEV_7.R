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

source('~/Documents/RStudio projects1/20171008/Lungfunction1/FEV_functions.R')
source('~/Documents/RStudio projects1/20171008/Lungfunction1/FEV_sidebarPanel.R')



#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    FEV_sidebar(),

    mainPanel (
      #tags$p("Baseline FEV Effect (mL):"),
      #verbatimTextOutput("baseline_FEV"),
      #tags$p("Rate of FEV change, mL/y:"),
      #verbatimTextOutput("rate_of_change_FEV"),
     # tags$p("Plot graph of linear regression:"),
      #plotOutput("plot"),
      #tags$p("Regression line:"),
      #verbatimTextOutput("regression_line"),

      tags$p("lmer summary:"),
      verbatimTextOutput("lmer_summary"),
      width = 5, class = 'rightAlign'
    )
  )
)

server <- function(input, output, session) {

  #browser()
  #load inputs
  observeEvent(input$load_inputs,{
    if(!file.exists('FEV_inputs.CSV')) {return(NULL)}
    loadedInputs <- read.csv('FEV_inputs.CSV')
    
    #load numeric values for the numericInput inputs
    for (i in 1:(length(loadedInputs$FEV_input_names)-4)) { 
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_num_vals[(i)]) )
    }
    #load strings for selectInput inputs
    for (i in (length(loadedInputs$FEV_input_names)-3):(length(loadedInputs$FEV_input_names))) { 
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_char_vals[(i)]) )
    }
  })
  
  #save inputs
  observeEvent(input$save_inputs,{
    FEV_frame_labels <- FEV_input_labels()
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
                              # input$ba_use,
                              # input$dys_exer,
                              # input$noc_s,
                              # input$sex
                              -999,
                              -999,
                              -999,
                              -999
    )
    FEV_frame_char_values <- c("NULL", #FEV_frame_char_values used to generate data frame column with char values only
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               "NULL",
                               input$ba_use,
                               input$dys_exer,
                               input$noc_s,
                               input$sex
    )
    # FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels, FEV_input_vals=FEV_frame_values)
    FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels,
                                 FEV_input_num_vals=FEV_frame_num_values,
                                 FEV_input_char_vals = FEV_frame_char_values)
    # saveRDS( reactiveValuesToList(input) , file = 'inputs.RDS')
    write.csv( FEV_data_frame , file = 'FEV_inputs.csv')
  })  
  

  
  ##########NULL functions
  # An observer is like a reactive expression in that it can read reactive values and call reactive expressions,
  # and will automatically re-execute when those dependencies change. But unlike reactive expressions,
  # it doesn't yield a result and can't be used as an input to other reactive expressions.
  # Thus, observers are only useful for their side effects (for example, performing I/O).
  
  ##NULL for age
  react <- reactiveValues()                               #Create an object for storing reactive values; name of the object = react
  observe({                                               #Create a reactive observer - reactive expression in that it can read reactive values and call reactive expressions, reexecutes when dependencies change
    if(is.na(input$age)){return()}                        #is.na(input$age) tests is age input is not available; if age input is not available returns NULL
    if(input$age < 0){                                    #if entered age is less than 0, return 0
      react$age =0
      updateNumericInput(session, "age", age = react$age) #Change the value of a number input on the client -
      # SYNTAX: updateNumericInput(session, inputId, label = NULL, value = NULL,min = NULL, max = NULL, step = NULL)
      # The input updater functions send a message to the client, telling it to change the settings of an input object.
      # The messages are collected and sent after all the observers (including outputs) have finished running.
    } else {                                              #Else update the age
      react$age <- input$age
    }
  }
  )
  #NULL for follow_up_baseline
  react <- reactiveValues()
  observe({
    if(is.na(input$follow_up_baseline)){return()}
    else  {
      react$follow_up_baseline <- input$follow_up_baseline
    }
  }
  )
  #NULL for triglycerides
  react <- reactiveValues()
  observe({
    if(is.na(input$trig)){return()}
    else  {
      react$trig <- input$trig
    }
  }
  )
  #NULL for hematocrit
  react <- reactiveValues()
  observe({
    if(is.na(input$hema)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$hema <- input$hema
    }
  }
  )
  #NULL for albumin
  react <- reactiveValues()
  observe({
    if(is.na(input$alb)){return()}
    else  {
      react$alb <- input$alb
    }
  }
  )
  #NULL for globulin
  react <- reactiveValues()
  observe({
    if(is.na(input$glob)){return()}
    else  {
      react$glob <- input$glob
    }
  }
  )
  #NULL for Alkaline Phosphotase
  react <- reactiveValues()
  observe({
    if(is.na(input$alk_phos)){return()}
    else  {
      react$alk_phos <- input$alk_phos
    }
  }
  )
  #NULL for white blood cell count
  react <- reactiveValues()
  observe({
    if(is.na(input$white_bc)){return()}
    else  {
      react$white_bc <- input$white_bc
    }
  }
  )
  #NULL for QRS interval (hundredth of sec)
  react <- reactiveValues()
  observe({
    if(is.na(input$qrs)){return()}
    else  {
      react$qrs <- input$qrs
    }
  }
  )
  #NULL for alcohol index
  react <- reactiveValues()
  observe({
    if(is.na(input$alcohol)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$alcohol <- input$alcohol
    }
  }
  )
  #NULL for wine intake
  react <- reactiveValues()
  observe({
    if(is.na(input$wine)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$wine <- input$wine
    }
  }
  )
  #NULL for cocktail intake
  react <- reactiveValues()
  observe({
    if(is.na(input$cocktail)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$cocktail <- input$cocktail
    }
  }
  )
  #NULL for Height
  react <- reactiveValues()
  observe({
    if(is.na(input$height_square)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$height_square <- input$height_square
    }
  }
  )
  #NULL for cumulative smoke pack-year
  react <- reactiveValues()
  observe({
    if(is.na(input$cum_smoke)){return()}
    if(input$hema < 0){                                   
      react$hema =0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$cum_smoke <- input$cum_smoke
    }
  }
  )
  
  # browser()
  # https://shiny.rstudio.com/articles/isolation.html
  #isolate(): This function takes an R expression, and it tells Shiny that the calling observer or 
  #reactive expression should not take a dependency on any reactive objects inside the expression.
  output$lmer_summary <- renderTable({
    #browser()
    if (input$lmer_Submit_button == 0)
       return()

    #input$lmer_Submit_button
    isolate({

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
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
      full_file_name = paste(paste(file_name,collapse=" "),".rds") #BINARY_CODE_FROM_INPUTS was updated from generating a single character to generating a vector of characters
      #2.0 if RDS file(for given inputs) exists, get lmer_summary from the rts file
      if(file.exists(full_file_name)){ 
        # 3.If file exists, Load lmer object from the file
        
        loaded_lmer_fn <- readRDS(full_file_name)
        
        print(summary(loaded_lmer_fn))
        
        # # output$lmer_summary_output <- renderUI({
        # #   # paste("File for these inputs already exists\nSummary has been loaded from \n",full_file_name)
        # #   summary(loaded_lmer_fn)
        # # })
        # summary(loaded_lmer_fn)
        
      }
      else{
        # 4.If file does not exist

        #Create BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('age','follow_up_baseline','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        BINARY_INPUT_VALUES <- c("1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
        # BINARY_CODE_DATAFRAME1 <- data.frame(BINARY_INPUT_VALUES1, BINARY_INPUT_NAMES1)
        BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
        
        #Create FACTOR_NAMES_DATAFRAME
        INPUTS <- c('age','follow_up_baseline','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('age','year','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('agecat','year2','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c(NA,'(year|RANDOMID)','triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
        # browser()
        
        
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)

        
        # write.csv( FEV_coefficients_data_frame , file = full_file_name)
        saveRDS(object=lmer_function_output,file = full_file_name,compress=TRUE, refhook = NULL)

        
        
        # output$lmer_summary_output <- renderUI({
        #   # paste("File for these inputs already exists\nSummary has been loaded from \n",full_file_name)
        #   summary(lmer_function_output)
        # })
        print(summary(lmer_function_output))
        # renderUI({
        #   summary(lmer_function_output)
        # })
        
      }
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

    }) #end of isolate({...})
    
  })#end of output$lmer_summary <- renderTable
  
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)