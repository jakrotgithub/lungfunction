library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyjs)

source('~/Documents/RStudio projects1/20171008/Lungfunction/FEV_coefficients.R')
source('~/Documents/RStudio projects1/20171008/Lungfunction/FEV_functions.R')
source('~/Documents/RStudio projects1/20171008/Lungfunction/FEV_sidebarPanel.R')


#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    FEV_sidebar(),

    mainPanel (
      # tags$p("Baseline FEV Effect (mL):"),
      # verbatimTextOutput("baseline_FEV"),
      # tags$p("Rate of FEV change, mL/y:"),
      # verbatimTextOutput("rate_of_change_FEV"),
      tags$p("Plot graph of linear regression:"),
      plotOutput("plot"),
      tags$p("Regression line:"),
      verbatimTextOutput("regression_line"), width = 5, class = 'rightAlign' 
    )
  )
)

server <- function(input, output, session) {

  ################################################################################
  #######################BEGINNING OF SUBMIT BUTTON CODE##########################
  ################################################################################
  
  #Submit inputs to calculate FEV coefficients
  observeEvent(input$submit_inputs,{
    
    #browser()
    # 1.Determine which inputs are null - generate "binary"-notation filename based on that
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
    full_file_name = paste(file_name,".csv")
    # 2.Check if a filename with matching inputs exists - assume .csv file extension for now
    if(file.exists(full_file_name)){
      # 3.If file exists, Load the coefficients from the file and proceed with calculations
      # loadedCoefficients is a data frame?
      loadedCoefficients <- read.csv(full_file_name)
      coefficient_names = loadedCoefficients$coefficient_name
      coefficient_values = loadedCoefficients$coefficient_value
    }
    else{
      # 4.If file does not exist
      # 4.a calculate coefficients - put dummy code in here for new (just set coefficients to the pre-computed constants)
      # Need to create calculate_coefficients() function - it will return a vector of size 28(total number of coefficients)
      
      ####DUMMY CODE BEGIN###############
      intercept = 1127.26 #intercept defined for baseline FEV, mL (Table 2. Wenjia's manuscript)
      trig_effect = -0.30       # parameter: Triglycerides
      hema_effect = -12.28      # Hematocrit
      alb_effect = 11.33        # Albumin
      glob_effect = -3.49       # Globulin
      alk_phos_effect = -1.48   # Alkaline phosphotase
      white_bc_effect = -0.20   # White blood cell count
      qrs_effect = 27.37        # QRS interval
      alcohol_effect = -5.99    # Alcohol index
      wine_effect = 10.76       # Wine intake
      cocktail_effect = -0.60   # Cocktail intake
      height_square_effect = 0.11      # Height square
      cum_smoke_effect = -3.46  # Cumulative smoke pack-year
      smoke_pack_years_trig_effect = 0.003 # Smoke pack-years * Triglycerides
      intercept_rc = 21.86 #effect for rate of FEV change defined ['rc' stands for 'rate of change'] (Table 2. Wenjia's manuscript)
      follow_up_baseline_effect = -0.46 #Follow-up since baseline, y
      trig_effect_rc = 0.004       # parameter: Triglycerides
      hema_effect_rc = -0.29      # Hematocrit
      alb_effect_rc = 0.07        # Albumin
      glob_effect_rc = 0.10      # Globulin
      alk_phos_effect_rc = 0.03   # Alkaline phosphotase
      white_bc_effect_rc = -0.03   # White blood cell count
      qrs_effect_rc = -0.64        # QRS interval
      alcohol_effect_rc = 0.14    # Alcohol index
      wine_effect_rc = -0.23       # Wine intake
      cocktail_effect_rc = -0.13   # Cocktail intake
      height_square_effect_rc = 0
      cum_smoke_effect_rc = 0
      
      FEV_ceoff_val_vector <- c(intercept, 
                               trig_effect,
                               hema_effect,
                               alb_effect,
                               glob_effect,
                               alk_phos_effect,
                               white_bc_effect,
                               qrs_effect,
                               alcohol_effect,
                               wine_effect,
                               cocktail_effect,
                               height_square_effect,
                               cum_smoke_effect,
                               smoke_pack_years_trig_effect,
                               intercept_rc,
                               follow_up_baseline_effect,
                               trig_effect_rc,
                               hema_effect_rc,
                               alb_effect_rc,
                               glob_effect_rc,
                               alk_phos_effect_rc,
                               white_bc_effect_rc,
                               qrs_effect_rc,
                               alcohol_effect_rc,
                               wine_effect_rc,
                               cocktail_effect_rc,
                               height_square_effect_rc,
                               cum_smoke_effect_rc
                               )
      ####DUMMY CODE END###############

      
      # 4.b create data frame with coefficients
      # FEV_coeff_name_vector is defined inside FEV_functions.R
      FEV_coeff_data_frame<- data.frame(FEV_coeff_names=FEV_coeff_name_vector,
                                        FEV_coeff_vals=FEV_ceoff_val_vector)
          # 4.c save the coefficients to a new file
      write.csv( FEV_coeff_data_frame , file = full_file_name)
    }
  })
  
  ################################################################################
  #######################END OF SUBMIT BUTTON CODE################################
  ################################################################################
  
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
  
  v <- reactiveValues(doPlot = FALSE)
observeEvent(input$calc_and_plot, {
  v$doPlot <- input$calc_and_plot
})
  
  output$plot <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)

    fev_intercept = FEV(input$trig, input$hema,
        input$alb, input$glob,
        input$alk_phos, input$white_bc,
        input$qrs, input$alcohol,
        input$wine, input$cocktail,
        input$height_square, input$cum_smoke,
        input$age,
        input$ba_use, ba_use_bool,
        input$dys_exer,
        input$noc_s, noc_s_bool,
        input$sex)

    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    female_male_effect_rc=SEX_FM_RC(input$sex)

   fev_slope = FEV_RC(input$follow_up_baseline, input$trig, input$hema,
           input$alb, input$glob,
           input$alk_phos, input$white_bc,
           input$qrs, input$alcohol,
           input$wine, input$cocktail,
           input$height_square, input$cum_smoke,
           input$age,
           input$ba_use, ba_use_bool_rc,
           input$dys_exer,
           input$noc_s, noc_s_bool_rc, input$sex)
   
   print (fev_intercept)
   print (fev_slope)
################NEW GGPLOT CODE############################
   #create data frame
#   df<-data.frame(slope=fev_slope, intercept=fev_intercept)
   df <- matrix (NA, nrow = 20, ncol = 2)
   colnames(df) <- c("year", "fev1")
   df[, 1] <- c(1:20)
   df[1,2] <- fev_intercept
   for (i in (2:20)) {
     df[i,2] <- df[i-1, 2] + fev_slope
   }
   df <- as.data.frame(df)
   print (df)
   ggplot(df, aes (y = fev1, x = year))+
#     scale_x_continuous(name="Year", limits=c(0,20)) +
#     scale_y_continuous(name="Predicted FEV1", limits=c(0, 5000)) +  
     geom_point() + 
     geom_line()
################END OF NEW GGPLOT CODE########################

})
  })
  
  
  output$regression_line <- renderText({
    if (v$doPlot == FALSE) return()
    
    isolate({
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)

    fev_intercept = FEV(input$trig, input$hema,
        input$alb, input$glob,
        input$alk_phos, input$white_bc,
        input$qrs, input$alcohol,
        input$wine, input$cocktail,
        input$height_square, input$cum_smoke,
        input$age,
        input$ba_use, ba_use_bool,
        input$dys_exer,
        input$noc_s, noc_s_bool,
        input$sex)

    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    female_male_effect_rc=SEX_FM_RC(input$sex)

   fev_slope = FEV_RC(input$follow_up_baseline, input$trig, input$hema,
           input$alb, input$glob,
           input$alk_phos, input$white_bc,
           input$qrs, input$alcohol,
           input$wine, input$cocktail,
           input$height_square, input$cum_smoke,
           input$age,
           input$ba_use, ba_use_bool_rc,
           input$dys_exer,
           input$noc_s, noc_s_bool_rc, input$sex)



   paste('y', '=', fev_slope, '*', 'x','+ (', fev_intercept,')')
  })
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
      react$hema = 0
      updateNumericInput(session, "hema", hema = react$hema)
    } else  {
      react$cum_smoke <- input$cum_smoke
    }
  }
  )

  # browser()
  output$age_cat_effect <- renderText({
    # age_bool=AGE_CHECK(input$age)
    # ACE(input$age, age_bool)
    
    if(is.na(input$age)){return()}
    age_bool=AGE_CHECK(input$age)
    ACE(input$age, age_bool)
  })
  
  #######For rate of change coefficient
  output$age_cat_effect_rc <- renderText({
    age_bool=AGE_CHECK(input$age)
    ACE_RC(input$age, age_bool)
  })
  
  output$ba_use_effect <- renderText({
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    BUE(input$ba_use, ba_use_bool)
  })
  #######For rate of change coefficient
  output$ba_use_effect_rc <- renderText({
    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    BUE(input$ba_use, ba_use_bool_rc)
  })
  
  output$noc_s_effect <- renderText({
    noc_s_bool=NSB_CHECK(input$noc_s)
    NSB(input$noc_s, noc_s_bool)
  })
  #######For rate of change coefficient
  output$noc_s_effect_rc <- renderText({
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    NSB(input$noc_s, noc_s_bool_rc)
  })
  
  # output$dys_exer_effect <- renderText({
  #   dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
  #   DEE(input$dys_exer, dys_exer_effect)
  # })
  # #######For rate of change coefficient
  # output$dys_exer_effect_rc <- renderText({
  #   dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
  #   DEE(input$dys_exer, dys_exer_effect_rc)
  # })
  # 
  # output$dys_sex_effect <- renderText({
  #   dys_sex_effect=SEX_CHECK(input$sex, input$dys_exer)
  #   DSE(input$sex, input$dys_exer, dys_sex_effect)
  # })
  
  # #####For rate of change of FEV
  # output$female_male_effect_rc <- renderText({
  #   female_male_effect_rc=SEX_FM_RC(input$sex)
  #   # SFM(input$sex, female_male_effect_rc)
  #   SFM(dys_exer)
  # })
  # 
  output$baseline_FEV <- renderText({
    
    if(is.na(input$age)){return()} #check if age input is NULL
    
    #perform parameter checks
    #age_bool=AGE_CHECK(input$age)                            ###JK
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)
    
    
    
    FEV(input$trig,
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
        #age_bool,                                                  ###JK
        input$ba_use,
        ba_use_bool,
        input$dys_exer,
        input$noc_s,
        noc_s_bool,
        input$sex)
  })
  
  #####For rate of change of FEV
  output$rate_of_change_FEV <- renderText({

    if(is.na(input$age)){
      # return()
      validate(
        need(input$age != "", "Please enter age")
      )
      } #check if age input is NULL
    
    #perform parameter checks
    #age_bool=AGE_CHECK(input$age)                                      ###JK
    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    female_male_effect_rc=SEX_FM_RC(input$sex)

    FEV_RC(input$follow_up_baseline, input$trig, input$hema,          
        input$alb, input$glob,
        input$alk_phos, input$white_bc,
        input$qrs, input$alcohol,
        input$wine, input$cocktail,
        input$height_square, input$cum_smoke,
        input$age, 
        #age_bool,                                                       ###JK
        input$ba_use, ba_use_bool_rc,
        input$dys_exer,
        input$noc_s, noc_s_bool_rc, input$sex)

  })
  
}


#Run the application
shinyApp(ui = ui, server = server)