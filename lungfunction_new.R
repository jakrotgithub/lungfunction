#make changes to server function based on changes to FEV_RC global

library(shiny)
library(shinythemes)

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

#######For rate of change coefficient
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
########################################################################
height_square_effect_rc = 0
cum_smoke_effect_rc = 0
########################################################################

#functions that determine boolean values
AGE_CHECK <- function(age){
  if ((35 <= age) & (age <= 49)) {
    age_bool = 0
  } else if ((50 <= age) & (age <= 64)) {
    age_bool = 1
  } else if ((age <= 34) | (age >= 65)) {
    age_bool = 2
  }
  return(age_bool)  
}

BA_USE_CHECK <- function(ba_use){
  if(ba_use == 'Current use') {
    ba_use_bool = 1
  } else if (ba_use == 'Former use') {
    ba_use_bool = 0
  } else if (ba_use == 'No use') {
    ba_use_bool = 0
  }
  return(ba_use_bool)
}

#######For rate of change coefficient
BA_USE_CHECK_RC <- function(ba_use){
  if(ba_use == 'Current use') {
    ba_use_bool_rc = 1
  } else if (ba_use == 'Former use') {
    ba_use_bool_rc = 0
  } else if (ba_use == 'No use') {
    ba_use_bool_rc = 0
  }
  return(ba_use_bool_rc)
}

NSB_CHECK <- function(noc_s){
  if(noc_s == 'Yes') {
    noc_s_bool = 0
  } else if (noc_s == 'Maybe') {
    noc_s_bool = 1
  } else if (noc_s == 'No') {
    noc_s_bool = 0
  }
  return(noc_s_bool)
}

#######For rate of change coefficient
NSB_CHECK_RC <- function(noc_s){
  if(noc_s == 'Yes') {
    noc_s_bool_rc = 1
  } else if (noc_s == 'Maybe') {
    noc_s_bool_rc = 0
  } else if (noc_s == 'No') {
    noc_s_bool_rc = 0
  }
  return(noc_s_bool_rc)
}

DYS_EXER_CHECK <- function(dys_exer){
  if(dys_exer == "On slight exertion") {
    dys_exer_effect = -226.09
  } else if (dys_exer == "On moderate exercise") {
    dys_exer_effect = -560.37
  } else if (dys_exer == "On rigorous exercise") {
    dys_exer_effect = -224.83
  } else if (dys_exer == 'No dyspnea on exertion') {
    dys_exer_effect = 0
  }
  return(dys_exer_effect)
}

#######For rate of change coefficient
DYS_EXER_CHECK_RC <- function(dys_exer){
  if(dys_exer == "On slight exertion") {
    dys_exer_effect_rc = 15.79
  } else if (dys_exer == "On moderate exercise") {
    dys_exer_effect_rc = -2.43
  } else if (dys_exer == "On rigorous exercise") {
    dys_exer_effect_rc = -1.81
  } else if (dys_exer == 'No dyspnea on exertion') {
    dys_exer_effect_rc = 0
  }
  return(dys_exer_effect_rc)
}

SEX_CHECK <- function(sex, dys_exer){
  if ((sex == 'female') & (dys_exer == 'On rigorous exercise')) {dys_sex_effect = 149.38}
  else if ((sex == 'female') & (dys_exer == 'On moderate exercise')) {dys_sex_effect = 575.01}
  else if ((sex == 'female') & (dys_exer == 'On slight exertion'))   {dys_sex_effect = -368.46}
  else if ((sex == 'female') & (dys_exer == 'No dyspnea on exertion')) {dys_sex_effect = 0}
  else if ((sex == 'male') & (dys_exer == 'On rigorous exercise')) {dys_sex_effect = 0}
  else if ((sex == 'male') & (dys_exer == 'On moderate exercise')) {dys_sex_effect = 0}
  else if ((sex == 'male') & (dys_exer == 'On slight exertion'))   {dys_sex_effect = 0}
  else if ((sex == 'male') & (dys_exer == 'No dyspnea on exertion')) {dys_sex_effect = 0}
  return(dys_sex_effect)
}

#AS - Albumin*Sex (female vs. male)
AS <- function(sex, alb) {
  if (sex == 'female') {
    alb_sex = -9.50*alb
  } else if (sex == 'male') {
    alb_sex = 0
  }
  return (alb_sex)
}

#HS - Height square, cm^2
HS <- function(sex, height_square) {
  if (sex == 'female') {
    height_square_sex = -0.02*(height_square^2) #######################CHANGE WAS IMPLEMENTED!
  } else if (sex == 'male') {
    height_square_sex = 0
  }
  return (height_square_sex)
}

#SEX_FM - Sex (female vs. male)
SEX_FM <- function(sex) {
  if (sex == 'female') {
    female_male_effect = -660.44
  } else if (sex == 'male') {
    female_male_effect = 0
  }
  return (female_male_effect)
}

#######For rate of change coefficient
SEX_FM_RC <- function(sex) {
  if (sex == 'female') {
    female_male_effect_rc = 5.25
  } else if (sex == 'male') {
    female_male_effect_rc = 0
  }
  return (female_male_effect_rc)
}
#########################################################
# SFM <- function(sex, female_male_effect_rc) {            #DK - you are not using <sex> or <female_male_effect_rc> variables inside the function
#     sex_fm_rc = dys_exer_effect_rc                        #As of right now this function performs the exact same thing as DYS_EXER_CHECK_RC
#     return(sex_fm_rc)
# }
#########################################################
SFM <- function(dys_exer) { #DK - Right now all this function does is call the DYS_EXER_CHECK_RC function, not sure if this is correct, might need to update
  sex_fm_rc = DYS_EXER_CHECK_RC(dys_exer)
  return(sex_fm_rc)
}

ACE <- function(age, age_bool) {
  if (age_bool == 1 | age_bool == 0) {
    ace = (-15.76*age) + ((-139.62+29.47)*age_bool)-29.47
  } else if (age_bool == 2) {
    ace = 0
  }
  return(ace)
}

#######For rate of change coefficient
ACE_RC <- function(age, age_bool) {
  if (age_bool == 1 | age_bool == 0) {
    ace_rc = (-0.81*age) + ((6.68-2.69)*age_bool)+2.69
  } else if (age_bool == 2) {
    ace_rc = 0
  }
  return(ace_rc)
}


#BUE - Bronchodilator Use Effect
BUE <- function(ba_use, ba_use_bool) {
  if(ba_use == 'Former use' | ba_use == "Current use") {
    bue = (-213.06*ba_use_bool)-50.95
  } else if (ba_use == 'No use') {
    bue = 0
  }
  return(bue)
}

#######For rate of change coefficient
BUE_RC <- function(ba_use, ba_use_bool_rc) {
  if(ba_use == 'Former use' | ba_use == "Current use") {
    bue_rc = (3.16*ba_use_bool_rc)-1.86
  } else if (ba_use == 'No use') {
    bue_rc = 0
  }
  return(bue_rc)
}

#NSB - Nocturnal Symptoms Effect
NSB <- function(noc_s, noc_s_bool) {
  if(noc_s == 'Yes' | noc_s == 'Maybe') {
    nsb = (-219.16*noc_s_bool)-342.92
  } else if (noc_s == 'No') {
    nsb = 0
  }
  return(nsb)
}

#######For rate of change coefficient
NSB_RC <- function(noc_s, noc_s_bool_rc) {
  if(noc_s == 'Yes' | noc_s == 'Maybe') {
    nsb_rc = (1.91*noc_s_bool_rc)+2.04
  } else if (noc_s == 'No') {
    nsb_rc = 0
  }
  return(nsb_rc)
}

#DEE - Dyspnea on Exertion Effect
DEE <- function (dys_exer, dys_exer_effect) {
  dee = dys_exer_effect
  return(dee)
}

#######For rate of change coefficient
DEE_RC <- function (dys_exer, dys_exer_effect_rc) {
  # dee_rc = dys_exer_effect_rc
  dee_rc = DYS_EXER_CHECK_RC(dys_exer) 
  return(dee_rc)
}

#DSE - Dyspnea Sex Effect
DSE <- function(sex, dys_exer, dys_sex_effect) {
  dse = dys_sex_effect
  return(dse)
}

FEV <- function (trig,
                 hema,
                 alb,
                 glob,
                 alk_phos,
                 white_bc,
                 qrs,
                 alcohol,
                 wine,
                 cocktail,
                 height_square,
                 cum_smoke,
                 age,
                 age_bool,
                 ba_use,
                 ba_use_bool,
                 dys_exer,
                 # dys_exer_effect,              #DELETED
                 noc_s,
                 noc_s_bool,
                 # dys_sex_effect,              #DELETED
                 sex) {
  b_fev =
    intercept +                                   
    AS(sex, alb) +                             
    HS (sex, height_square) +                               
    (cum_smoke*trig*smoke_pack_years_trig_effect) +                          
    SEX_FM(sex) +                         
    ACE(age, age_bool) +                   
    BUE(ba_use, ba_use_bool) +             
    NSB(noc_s, noc_s_bool) +               
    # DEE(dys_exer, dys_exer_effect) +              #DELETED
    DEE(dys_exer, DYS_EXER_CHECK(dys_exer)) +       #UPDATED
    # DSE(sex, dys_exer, dys_sex_effect) +          #DELETED
    DSE(sex, dys_exer, SEX_CHECK(sex,dys_exer)) +   #UPDATED
    SEX_CHECK(sex, dys_exer) +
    (trig*trig_effect) +                  
    (hema*hema_effect) +                  
    (alb*alb_effect) +                    
    (glob*glob_effect) +                  
    (alk_phos*alk_phos_effect) +          
    (white_bc*white_bc_effect) +          
    (qrs*qrs_effect) +                    
    (alcohol*alcohol_effect) +            
    (wine*wine_effect) +                  
    (cocktail*cocktail_effect) +          
    ((height_square^2)*height_square_effect) +              
    (cum_smoke*cum_smoke_effect)          
  
  return(b_fev)
}

#####For rate of change of FEV
FEV_RC <- function (follow_up_baseline, trig, hema, alb, glob, alk_phos, white_bc,
                    qrs, alcohol, wine, cocktail, height_square, cum_smoke,
                    age, age_bool, ba_use, ba_use_bool_rc, dys_exer,
                    noc_s, noc_s_bool_rc, sex) {
  b_fev_rc =
    intercept_rc +  
    (follow_up_baseline*follow_up_baseline_effect) +
    SEX_FM_RC(sex) +
    ACE_RC(age, age_bool) +                   
    BUE_RC(ba_use, ba_use_bool_rc) +             
    NSB_RC(noc_s, noc_s_bool_rc) +               
    DEE_RC(dys_exer, DYS_EXER_CHECK_RC(dys_exer)) +      
    (trig*trig_effect_rc) +                  
    (hema*hema_effect_rc) +                  
    (alb*alb_effect_rc) +                    
    (glob*glob_effect_rc) +                  
    (alk_phos*alk_phos_effect_rc) +          
    (white_bc*white_bc_effect_rc) +          
    (qrs*qrs_effect_rc) +                    
    (alcohol*alcohol_effect_rc) +            
    (wine*wine_effect_rc) +                  
    (cocktail*cocktail_effect_rc) +          
    ((height_square^2)*height_square_effect_rc) +              
    (cum_smoke*cum_smoke_effect_rc)          
  
  return(b_fev_rc)
}

#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    sidebarPanel(
      # numericInput("age",
      #              "Age (year)",
      #              value = 35,
      #              min = 35,
      #              max = 64,
      #              step = 1,
      #              width = 200),
      numericInput("age",
                   "Age (year)",
                   value = NULL,               ###Have to change value to NULL!
                   min = 35,
                   max = 64,
                   step = 1,
                   width = 200),
      selectInput("sex",
                  "Sex",
                  list('female', 'male'),
                  selected = 'male'),
      numericInput("follow_up_baseline",
                   "Follow-up since baseline (year)",
                   value = 0,
                   min = -100,
                   max = 100,
                   width = 200),
      numericInput("trig",
                   "Triglycerides (mg/dl)",
                   value = NULL, 
                   step = 0.01,
                   width = 200),
      numericInput("hema",
                   "Hematocrit (%)",
                   value = 0,
                   min = 0,
                   max = 100,
                   step = 0.01,
                   width = 200),
      numericInput("alb",
                   "Albumin (mg/L)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("glob",
                   "Globulin (g/L)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("alk_phos",
                   "Alkaline Phosphotase (units)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("white_bc",
                   "White blood cell count (10^9/L)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("qrs",
                   "QRS interval (hundredth of sec)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("alcohol",
                   "Alcohol index (ozs/wk)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("wine",
                   "Wine intake (glasses/wk)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("cocktail",
                   "Cocktail intake (drinks/wk)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("height_square",
                   "Height square (cm^2)",
                   value = 0,
                   step = 0.01,
                   width = 200),
      numericInput("cum_smoke",
                   "Cumulative smoke pack-year",
                   value = 0,
                   step = 0.01,
                   width = 200),
      column(8,
             selectInput("ba_use",
                         "Bronchodilator or aerosol (vs. no use)",
                         list('Current use', 'Former use', 'No use'),
                         selected = 'No use')),
      
      column(8,
             selectInput("dys_exer",
                         "Dyspnea on exertion (vs. none)",
                         list('On rigorous exercise', 'On moderate exercise', 'On slight exertion', 'No dyspnea on exertion'),
                         selected = 'No dyspnea on exertion')),
      column(8,
             selectInput("noc_s",
                         "Nocturnal symptoms (vs. none)",
                         list('Yes', 'Maybe', 'No'),
                         selected = 'No'))
    ),
    
    mainPanel (
      # tags$p("FEV (mL)"),
      # verbatimTextOutput("baseline_FEV"),
      # tags$p("Age Category Effect:"),
      # verbatimTextOutput("age_cat_effect"),
      # tags$p("Bronchodilator or aerosol (vs. no use) Effect:"), #for end product have to remove
      # verbatimTextOutput("ba_use_effect"), #for end product have to remove
      # tags$p("Dyspnea on exertion (vs. none) Effect:"),
      # verbatimTextOutput("dys_exer_effect"),
      # tags$p("Dyspnea*Sex (vs. male or no dyspnea) Effect:"),
      # verbatimTextOutput("dys_sex_effect"),
      # tags$p("Nocturnal symptoms (vs. none) Effect:"),
      # verbatimTextOutput("noc_s_effect"),
      tags$p("Baseline FEV Effect (mL):"),
      verbatimTextOutput("baseline_FEV"),
      tags$p("Rate of FEV change, mL/y:"),
      verbatimTextOutput("rate_of_change_FEV"),
      tags$p("Plot graph of linear regression:"),
      plotOutput("plot"),
      tags$p("Regression line:"),
      verbatimTextOutput("regression_line")
    )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    ## Setup up coordinate system (with x == y aspect ratio):
    plot(c(0, 546.19), c(2000, -7053.81), type = "n", xlab = "x", ylab = "y")
    ## the x- and y-axis, and an integer grid
    abline(546.19, -3.8, col = "red")
    abline(h = 0, v = 0, col = "gray60")
  })
  
  output$regression_line <- renderText({
    age_bool=AGE_CHECK(input$age)
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)
    
    a = FEV(input$trig, input$hema,
            input$alb, input$glob,
            input$alk_phos, input$white_bc,
            input$qrs, input$alcohol,
            input$wine, input$cocktail,
            input$height_square, input$cum_smoke,
            input$age, age_bool,
            input$ba_use, ba_use_bool,
            input$dys_exer, dys_exer_effect,
            input$noc_s, noc_s_bool,
            dys_sex_effect, input$sex)
    
    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    female_male_effect_rc=SEX_FM_RC(input$sex)
    
    b = FEV_RC(input$follow_up_baseline, input$trig, input$hema,
               input$alb, input$glob,
               input$alk_phos, input$white_bc,
               input$qrs, input$alcohol,
               input$wine, input$cocktail,
               input$height_square, input$cum_smoke,
               input$age, age_bool,
               input$ba_use, ba_use_bool_rc,
               input$dys_exer, female_male_effect_rc,
               input$noc_s, noc_s_bool_rc, input$sex)
    
    paste('y', '=', a, b, '*', 'x')
  })
  
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
  
  #NULL for triglycerides
  react <- reactiveValues()
  observe({
    if(is.na(input$trig)){return()}
    else  {
      react$trig <- input$trig
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
  
  output$dys_exer_effect <- renderText({
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    DEE(input$dys_exer, dys_exer_effect)
  })
  #######For rate of change coefficient
  output$dys_exer_effect_rc <- renderText({
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    DEE(input$dys_exer, dys_exer_effect_rc)
  })
  
  output$dys_sex_effect <- renderText({
    dys_sex_effect=SEX_CHECK(input$sex, input$dys_exer)
    DSE(input$sex, input$dys_exer, dys_sex_effect)
  })
  
  #####For rate of change of FEV
  output$female_male_effect_rc <- renderText({
    female_male_effect_rc=SEX_FM_RC(input$sex)
    # SFM(input$sex, female_male_effect_rc)
    SFM(dys_exer)
  })
  
  output$baseline_FEV <- renderText({
    
    if(is.na(input$age)){return()} #DK - check if age input is NULL
    
    #perform parameter checks
    age_bool=AGE_CHECK(input$age)
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)
    
    
    
    FEV(input$trig, input$hema, 
        input$alb, input$glob,
        input$alk_phos, input$white_bc,
        input$qrs, input$alcohol,
        input$wine, input$cocktail,
        input$height_square, input$cum_smoke,
        input$age, age_bool,
        input$ba_use, ba_use_bool,
        input$dys_exer,
        input$noc_s, noc_s_bool,
        input$sex)
  })
  
  #####For rate of change of FEV
  output$rate_of_change_FEV <- renderText({
    
    if(is.na(input$age)){
      # return()
      validate(
        need(input$age != "", "Please enter age")
      )
    } #DK - check if age input is NULL
    
    #perform parameter checks
    age_bool=AGE_CHECK(input$age)
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
           input$age, age_bool,
           input$ba_use, ba_use_bool_rc,
           input$dys_exer, female_male_effect_rc,
           input$noc_s, noc_s_bool_rc, input$sex)
    
    ###REFERENCE FEV_RC definition
    # FEV_RC <- function (follow_up_baseline, trig, hema, alb, glob, alk_phos, white_bc,
    #                     qrs, alcohol, wine, cocktail, height_square, cum_smoke,
    #                     age, age_bool, ba_use, ba_use_bool_rc, dys_exer,
    #                     dys_exer_effect_rc, noc_s, noc_s_bool_rc, sex)
    
    
  })
  
}


#Run the application
shinyApp(ui = ui, server = server)