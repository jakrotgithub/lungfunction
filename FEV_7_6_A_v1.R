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

setwd("~/Documents/RStudio projects1/20171111/")
source('FEV_functions.R')

GLOBAL_lmer_model <- NULL
GLOBAL_lmer_model_summary <- NULL

# lmer_function_output_summary <- NULL
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    source('FEV_sidebarPanel.R'),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Summary",
                           selectInput(
                             "lmer_summary_DropDownBox",
                             "Model Summary Selection",
                             list(
                               'Entire Summary',
                               'Coefficients',
                               'Residuals',
                               'Formula'),
                             selected = 'Entire Summary'),
                           verbatimTextOutput("lmer_summary")),
                  tabPanel("Plots",
                           tags$p("Plot graph of linear regression:"),
                           plotOutput("plot_FEV1_decline"),
                           tags$p("Regression line equation:"),
                           verbatimTextOutput("regression_line",placeholder = TRUE))
      )
    )
  )
)

server <- function(input, output, session) {

  #Browse button - prompts user to select input values file and loads it into GUI
  observeEvent(input$file1,{
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)

    #load the data frame from the csv file
    loadedInputs <- read.csv(inFile$datapath)

    #from the loaded file - get numeric values(INDEX 1-14) for the numericInput inputs
    for (i in 1:(length(loadedInputs$FEV_input_names)-4)) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_num_vals[(i)]) )
    }
    #from the loaded file - get strings(index 15-18) for selectInput inputs
    for (i in (length(loadedInputs$FEV_input_names)-3):(length(loadedInputs$FEV_input_names))) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_char_vals[(i)]) )
    }
  })

  #'Clear Inputs' button - set all inputs to NULL
  observeEvent(input$clear_inputs, {
    FEV_input_IDs <- FEV_input_labels()
    for (i in 1:length(FEV_input_IDs)) {
      session$sendInputMessage(FEV_input_IDs[i],  list(value = NULL) )
    }
  })


  #Save Inputs button - prompts user to save inputs to a csv file
  output$save_inputs <- downloadHandler(
    filename = function() {
      paste("FEV_input", ".csv", sep = "")
    },

    content = function(file) {
      #labels - 1st column in the data frame
      FEV_frame_labels <- FEV_input_labels()
      #numerical values - 2nd column in the data frame
      FEV_frame_num_values <- c(input$trig,
                                input$hema,
                                input$alb,
                                input$glob,
                                input$alk_phos,
                                input$white_bc,
                                input$qrs,
                                input$alcohol,
                                input$wine,
                                input$cocktail,
                                input$height,
                                input$cum_smoke,
                                input$age,
                                -999,# input$ba_use
                                -999,# input$dys_exer
                                -999,# input$noc_s
                                -999# input$sex
      )

      #non-numerical/character inputs - 3rd column in the data frame
      FEV_frame_char_values <- c("NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                                 "NULL","NULL",
                                 input$ba_use,
                                 input$dys_exer,
                                 input$noc_s,
                                 input$sex
      )

      FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels,
                                   FEV_input_num_vals=FEV_frame_num_values,
                                   FEV_input_char_vals = FEV_frame_char_values)
      write.csv(FEV_data_frame, file)
    }
  )

  #FEV_na_inputs_check.R checks for every inputs if the value is na - NOTE: enable this when introducing reactive outputs
  # source('FEV_na_inputs_check.R')
  


  #make lmer summary non-reactive --> it is only calculated when the user presses "Run Linear mixed-effects models" button
    output$lmer_summary <- renderPrint({
      # Take a dependency on input$lmer_Submit_button
      if (input$lmer_Submit_button == 0) #important to include because otherwise executes the rest of code even though user has not yet pressed the button
         return()

      # Create a Progress object
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      file_name <- isolate(
        BINARY_CODE_FROM_INPUTS(
          input$age,
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
          input$height,
          input$cum_smoke,
          input$sex,
          input$ba_use,
          input$dys_exer,
          input$noc_s
        )
      )
      
      
      #use collapse="" to get rid of spaces between 1s and 0s; use sep="" to get rid of space betweeen file name and ".rdata"
      full_file_name = paste(paste(file_name,collapse=""),".rdata",sep="")
      # browser()
      #if RDATA file(for given inputs) exists, get lmer_summary from the rts file
      if(file.exists(full_file_name)){
        # browser()
        progress$set(message = "Extracting lmer summary from RDATA File", value = 1.00)
        load(full_file_name) #loading the file loads 2 objects: lmer_function_output,lmer_function_output_summary
        
        GLOBAL_lmer_model <<- lmer_function_output #could also be after lines 178-181
        GLOBAL_lmer_model_summary <<- lmer_function_output_summary
        
        summary_lmfin <- lmer_function_output_summary
        summary_lmfin_coeff <- coef(summary_lmfin)
        summary_lmfin_resid <- resid(summary_lmfin)
        summary_lmfin_formula <- formula(summary_lmfin)
        # summary_lmfin
        
        # browser()
        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          summary_lmfin
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          summary_lmfin_coeff
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          summary_lmfin_resid
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          summary_lmfin_formula
        }
        
      }
      else{ #If file does not exist, run model, save results and summary into .rdata file and 
        #BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('age','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
        #FACTOR_NAMES_DATAFRAME
        INPUTS <- c('age','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height_square','cum_smoke','sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('age','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('agecat','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c(NA,'triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
        # browser()
        progress$set(message = "calculating lmer function", value = 1.00)
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
        
        progress$set(message = "Extracting lmer summary", value = 1.00)
        lmer_function_output_summary <- summary(lmer_function_output)
        
        progress$set(message = "Saving RDATA file w/ model and summary", value = 1.00)
        save(lmer_function_output,lmer_function_output_summary,file=full_file_name)
        
        GLOBAL_lmer_model <<- lmer_function_output #most important has to be after line 214
        GLOBAL_lmer_model_summary <<- lmer_function_output_summary
        
        # lmer_function_output_summary

        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          lmer_function_output_summary
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          coef(lmer_function_output_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          resid(lmer_function_output_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          formula(lmer_function_output_summary)
        }
      }
    }) #end of output$lmer_summary <- renderPrint
    
    
    
    
    output$plot_FEV1_decline <- renderPlot({ 
      # Take a dependency on input$plot_FEV1_button
      if (input$plot_FEV1_button == 0) #important to include because otherwise executes the rest of code even though user has not yet pressed the button
        return()
      
      # browser()
      # # Create a Progress object
      # progress <- shiny::Progress$new()
      # on.exit(progress$close())
      
      file_name <- isolate(
        BINARY_CODE_FROM_INPUTS(
          input$age,
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
          input$height,
          input$cum_smoke,
          input$sex,
          input$ba_use,
          input$dys_exer,
          input$noc_s
        )
      )
      
        ba_use_bool     <- isolate(BA_USE_CHECK(input$ba_use)) #use gui inputs; inputs which get called within renderplot fn. need to be isolated???
        noc_s_bool      <- isolate(NSB_CHECK(input$noc_s))
        dys_exer_effect <- isolate(DYS_EXER_CHECK(input$dys_exer))
        dys_sex_effect  <- isolate(SEX_CHECK(input$sex,input$dys_exer))
        
        # loadedCoefficients <- read.csv(full_file_name)
        loadedCoefficients <- coeffs(GLOBAL_lmer_model_summary)
        FEV_coeff_vector <- loadedCoefficients[c(1:28),] #DK- 20171124 will need to fix??? estimates ???
        FEV_coeff_val_vector <- as.data.frame(FEV_coeff_vector)$Estimate #in order for estimate to work, need to turn vector into data.frame; only then can I extract estimates
        
        fev_slope <- isolate(FEV_pass_coefficients(
                                          FEV_coeff_val_vector,
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
                                          input$height, # input$height_square,
                                          input$cum_smoke,
                                          input$age,
                                          input$ba_use,
                                          ba_use_bool,
                                          input$dys_exer,
                                          input$noc_s,
                                          noc_s_bool,
                                          input$sex))
        
        ba_use_bool_rc        <- isolate(BA_USE_CHECK_RC(input$ba_use))
        noc_s_bool_rc         <- isolate(NSB_CHECK_RC(input$noc_s))
        dys_exer_effect_rc    <- isolate(DYS_EXER_CHECK_RC(input$dys_exer))
        female_male_effect_rc <- isolate(SEX_FM_RC(input$sex))
        
        fev_intercept = isolate(FEV_RC_pass_coefficients(
          FEV_coeff_val_vector,
          # input$follow_up_baseline,
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
          input$height, # input$height_square,
          input$cum_smoke,
          input$age,
          input$ba_use,
          ba_use_bool_rc,
          input$dys_exer,
          input$noc_s,
          noc_s_bool_rc,
          input$sex))
        
        ################NEW GGPLOT CODE############################
        #create data frame
        df<-data.frame(slope=fev_slope, intercept=fev_intercept)
        
        ggplot()+
          scale_x_continuous(name="x", limits=c(-100,600)) +
          scale_y_continuous(name="y", limits=c(-8000,2000)) +
          geom_abline(data=df, mapping=aes(slope=df[1,1], intercept=df[1,2]))
        ################END OF NEW GGPLOT CODE########################
        
        
      # })
      #FEV PLOT - END
      
    })
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)