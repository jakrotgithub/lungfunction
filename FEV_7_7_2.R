#added description for FEV_slope & FEV_intercept functions 
#changed names of two functions to something more intuitive:
#FEV_pass_coefficients -> FEV_slope
#FEV_RC_pass_coefficients -> FEV_intercept
#included sidebarPanel file back in the mainPanel

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

setwd("~/Documents/RStudio projects1/20171111/FEV_7_7_2/")
source('FEV_7_7_2_functions.R')

GLOBAL_lmer_model <- NULL
GLOBAL_lmer_model_summary <- NULL
GLOBAL_lmer_model_loaded_FLAG <- NULL
button_width <- 160 #so we could change width of button right away without having to change width within every 'fluidRow'

# lmer_function_output_summary <- NULL
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),

  sidebarLayout(
    # source('FEV_sidebarPanel.R')

    
    sidebarPanel(
      #button_width <- 160,
      fluidRow(
        column(12,
               fluidRow(column(5,div(style = "font-size: 12px;",numericInput("age","Age (year)",value = NULL,min = 0,max = 250,step = 1,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("trig","Triglycerides (mg/dl)",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("hema","Hematocrit (%)",value = NULL,min = 0,max = 100,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("alb","Albumin (mg/L)",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("glob","Globulin (g/L)",value = NULL,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("alk_phos","Alkaline Phosphotase",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("white_bc","White blood cells(10^9/L)",value = NULL,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("qrs","QRS interval (0.01 sec)",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("alcohol","Alcohol index (ozs/wk)",value = NULL,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("wine","Wine intake (glasses/wk)",value = NULL,min = 0,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("cocktail","Cocktail intake (drinks/wk)",value = NULL,min = 0,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("height","Height (cm)",value = NULL,min = 0,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("cum_smoke","Cum. smoke pack-year",value = NULL,min = 0,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",selectInput("sex","sex",list('Not Selected','female', 'male'),selected = 'Not Selected'))),
                 column(5,div(style = "font-size: 12px;",selectInput("ba_use","Bronchodilator or aerosol",list('Not Selected','Current use', 'Former use', 'No use'),selected = 'Not Selected')))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",selectInput("dys_exer","Dyspnea on exertion",list('Not Selected','On rigorous exercise','On moderate exercise','On slight exertion','No dyspnea on ex.'),selected = 'Not Selected'))),
                 column(5,div(style = "font-size: 12px;",selectInput("noc_s","Nocturnal symptoms",list('Not Selected','Yes', 'Maybe', 'No'),selected = 'Not Selected')))),
               
               #action buttons
               fluidRow(column(5, div(style = "font-size: 12px;",downloadButton("save_inputs", "Save Inputs")))),
               fluidRow(column(10,div(style = "font-size: 12px;",fileInput("file1","Choose CSV File to Load",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),buttonLabel = "Load Inputs...")))),
               fluidRow(column(5, div(style = "font-size: 12px;",actionButton("lmer_Submit_button", "Run Linear mixed-effects models")))),
               fluidRow(column(5, div(style = "font-size: 12px;",actionButton("clear_inputs", "Clear Inputs")))),
               fluidRow(column(5, div(style = "font-size: 12px;",actionButton("plot_FEV1_button", "Plot FEV1 decline")))))
      ),
      width=4
    )
    ,

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
                           verbatimTextOutput("regression_line",placeholder = TRUE)),
                  tabPanel("Description of Application for User",
                           tags$p("Description of Inputs:"),
                           tags$p("How to run simulation?"),
                           tags$p("Application terms, concepts & abbreviations:"),
                           tags$p("Example: Patient scenario which demonstrates how the application works"),
                           tags$p("How was this application created?")
                                    ),
                  # tabPanel("Resources",
                  #          br(),
                  #          fluidRow(
                  #            column(6, "Resources for Clinician",
                  #                   fluidRow(column(6, div(style = "font-size: 12px;",actionButton("article_1", "Article 1")))),
                  #                   fluidRow(column(6, div(style = "font-size: 12px;",actionButton("article_2", "Article 2"))))
                  #            ),
                  #            column(6, "Resources for User")
                  #          )
                  #          ),
                  tabPanel("Disclaimer")
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


      #use collapse="" to get rid of spaces between 1s and 0s; use sep="" to get rid of space betweeen file name and ".rds"
      full_file_name = paste(paste(file_name,collapse=""),".rds",sep="")
      # browser()
      
      #if file exists but model has not been loaded, load the model from the file
      if(file.exists(full_file_name) && is.null(GLOBAL_lmer_model_loaded_FLAG)){
        progress$set(message = "Extracting lmer summary from RDS File", value = 1.00)
        lmer_function_output <- readRDS(full_file_name)
        
        #set the model-loaded-flag to TRUE
        GLOBAL_lmer_model_loaded_FLAG <<- TRUE
        
        GLOBAL_lmer_model <<- lmer_function_output #could also be after lines 178-181
        GLOBAL_lmer_model_summary <<- summary(lmer_function_output)
        
        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          GLOBAL_lmer_model_summary
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          coef(GLOBAL_lmer_model_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          resid(GLOBAL_lmer_model_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          formula(GLOBAL_lmer_model_summary)
        }
      }
      #if file exists and model has been loaded, then get the model from GLOBAL variable
      else if(file.exists(full_file_name) && !is.null(GLOBAL_lmer_model_loaded_FLAG)){
        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          GLOBAL_lmer_model_summary
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          coef(GLOBAL_lmer_model_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          resid(GLOBAL_lmer_model_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          formula(GLOBAL_lmer_model_summary)
        }
      }
      #if file does not exist, then calculate lmer model and create file
      else if(!file.exists(full_file_name)){
        #BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('age','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height','cum_smoke','sex','ba_use','dys_exer','noc_s')
        BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
        #FACTOR_NAMES_DATAFRAME
        INPUTS <- c('age','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height','cum_smoke','sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('age','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('agecat','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c(NA,'triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
        progress$set(message = "calculating lmer function", value = 1.00)
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
        
        progress$set(message = "Extracting lmer summary", value = 1.00)
        # lmer_function_output_summary <- summary(lmer_function_output)
        
        #set the model-loaded-flag to TRUE
        GLOBAL_lmer_model_loaded_FLAG <<- TRUE
        
        progress$set(message = "Saving RDATA file w/ model and summary", value = 1.00)
        saveRDS(lmer_function_output,file=full_file_name)
        
        GLOBAL_lmer_model <<- lmer_function_output #most important has to be after line 214
        GLOBAL_lmer_model_summary <<- summary(lmer_function_output)
        
        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          GLOBAL_lmer_model_summary
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          coef(GLOBAL_lmer_model_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          resid(GLOBAL_lmer_model_summary)
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          formula(GLOBAL_lmer_model_summary)
        }
      }

      
      # #if RDS file(for given inputs) exists, get lmer_summary from the rds file
      # if(file.exists(full_file_name)){
      #   # browser()
      #   progress$set(message = "Extracting lmer summary from RDS File", value = 1.00)
      #   # load(full_file_name) #loading the file loads 1 object: lmer_function_output
      #   lmer_function_output <- readRDS(full_file_name)
      # 
      #   GLOBAL_lmer_model <<- lmer_function_output #could also be after lines 178-181
      #   lmer_function_output_summary <- summary(lmer_function_output)
      #   GLOBAL_lmer_model_summary <<- lmer_function_output_summary
      # 
      #   summary_lmfin <- lmer_function_output_summary
      #   summary_lmfin_coeff <- coef(summary_lmfin)
      #   summary_lmfin_resid <- resid(summary_lmfin)
      #   summary_lmfin_formula <- formula(summary_lmfin)
      #   # summary_lmfin
      # 
      #   # browser()
      #   if(input$lmer_summary_DropDownBox == 'Entire Summary'){
      #     summary_lmfin
      #   }
      #   else if(input$lmer_summary_DropDownBox == 'Coefficients'){
      #     summary_lmfin_coeff
      #   }
      #   else if(input$lmer_summary_DropDownBox == 'Residuals'){
      #     summary_lmfin_resid
      #   }
      #   else if(input$lmer_summary_DropDownBox == 'Formula'){
      #     summary_lmfin_formula
      #   }
      # 
      # }
      # else{ #If file does not exist, run model, save results and summary into .rdata file and
      #   #BINARY_CODE_DATAFRAME
      #   BINARY_INPUT_NAMES <- c('age','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height','cum_smoke','sex','ba_use','dys_exer','noc_s')
      #   BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
      #   #FACTOR_NAMES_DATAFRAME
      #   INPUTS <- c('age','trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height','cum_smoke','sex','ba_use','dys_exer','noc_s')
      #   EQUATION_FACTORS1 <- c('age','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
      #   EQUATION_FACTORS2 <- c('agecat','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
      #   EQUATION_FACTORS3 <- c(NA,'triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      #   FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
      #   # browser()
      #   progress$set(message = "calculating lmer function", value = 1.00)
      #   lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
      # 
      #   progress$set(message = "Extracting lmer summary", value = 1.00)
      #   lmer_function_output_summary <- summary(lmer_function_output)
      # 
      #   progress$set(message = "Saving RDATA file w/ model and summary", value = 1.00)
      #   saveRDS(lmer_function_output,file=full_file_name)
      # 
      #   GLOBAL_lmer_model <<- lmer_function_output #most important has to be after line 214
      #   GLOBAL_lmer_model_summary <<- lmer_function_output_summary
      # 
      #   # lmer_function_output_summary
      # 
      #   if(input$lmer_summary_DropDownBox == 'Entire Summary'){
      #     lmer_function_output_summary
      #   }
      #   else if(input$lmer_summary_DropDownBox == 'Coefficients'){
      #     coef(lmer_function_output_summary)
      #   }
      #   else if(input$lmer_summary_DropDownBox == 'Residuals'){
      #     resid(lmer_function_output_summary)
      #   }
      #   else if(input$lmer_summary_DropDownBox == 'Formula'){
      #     formula(lmer_function_output_summary)
      #   }
      # }
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
        FEV_coeff_vector <- loadedCoefficients[c(1:28),] #20171124 will need to fix??? estimates ???
        FEV_coeff_val_vector <- as.data.frame(FEV_coeff_vector)$Estimate #in order for estimate to work, need to turn vector into data.frame; only then can I extract estimates

        fev_slope <- isolate(FEV_slope(
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
                                          input$height, # input$height,
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

        fev_intercept = isolate(FEV_intercept(
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
          input$height, # input$height,
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
