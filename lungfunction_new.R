#make changes to server function based on changes to FEV_RC global

library(shiny)
library(shinythemes)

intercept <- 1127.26 #intercept defined for baseline FEV, mL (Table 2. Wenjia's manuscript)

trig_effect <- -0.30       # parameter: Triglycerides
hema_effect <- -12.28      # Hematocrit
alb_effect <- 11.33        # Albumin
glob_effect <- -3.49       # Globulin
alk_phos_effect <- -1.48   # Alkaline phosphotase
white_bc_effect <- -0.20   # White blood cell count
qrs_effect <- 27.37        # QRS interval
alcohol_effect <- -5.99    # Alcohol index
wine_effect <- 10.76       # Wine intake
cocktail_effect <- -0.60   # Cocktail intake
height_square_effect <- 0.11      # Height square
cum_smoke_effect <- -3.46  # Cumulative smoke pack-year
smoke_pack_years_trig_effect <- 0.003 # Smoke pack-years * Triglycerides

#######For rate of change coefficient
intercept_rc = 21.86 #effect for rate of FEV change defined ['rc' stands for 'rate of change'] (Table 2. Wenjia's manuscript)

follow_up_baseline_effect <- -0.46 #Follow-up since baseline, y
trig_effect_rc <- 0.004       # parameter: Triglycerides
hema_effect_rc <- -0.29      # Hematocrit
alb_effect_rc <- 0.07        # Albumin
glob_effect_rc <- 0.10      # Globulin
alk_phos_effect_rc <- 0.03   # Alkaline phosphotase
white_bc_effect_rc <- -0.03   # White blood cell count
qrs_effect_rc <- -0.64        # QRS interval
alcohol_effect_rc <- 0.14    # Alcohol index
wine_effect_rc <- -0.23       # Wine intake
cocktail_effect_rc <- -0.13   # Cocktail intake
########################################################################
height_square_effect_rc <- 0
cum_smoke_effect_rc <- 0
########################################################################

#functions that determine boolean values
AGE_CHECK <- function(age){
  if (is.null(age))              {age_bool = 3}
  else if ((35 <= age) & (age <= 49)) {age_bool = 0}
  else if ((50 <= age) & (age <= 64)) {age_bool = 1}
  else if ((age <= 34) | (age >= 65)) {age_bool = 2}
  return(age_bool)  
}

BA_USE_CHECK <- function(ba_use){
  if(ba_use == 'Current use') {ba_use_bool = 1}
  else if (ba_use == 'Former use') {ba_use_bool = 0}
  else if (ba_use == 'No use') {ba_use_bool = 0}
  return(ba_use_bool)
}

#######For rate of change coefficient
BA_USE_CHECK_RC <- function(ba_use){
  if(ba_use == 'Current use') {ba_use_bool_rc = 1}
  else if (ba_use == 'Former use') {ba_use_bool_rc = 0}
  else if (ba_use == 'No use') {ba_use_bool_rc = 0}
  return(ba_use_bool_rc)
}

NSB_CHECK <- function(noc_s){
  if(noc_s == 'Yes') {noc_s_bool = 0}
  else if (noc_s == 'Maybe') {noc_s_bool = 1}
  else if (noc_s == 'No') {noc_s_bool = 0}
  return(noc_s_bool)
}

#######For rate of change coefficient
NSB_CHECK_RC <- function(noc_s){
  if (noc_s == 'Yes')     {noc_s_bool_rc = 1}
  else if (noc_s == 'Maybe')  {noc_s_bool_rc = 0}
  else if (noc_s == 'No')     {noc_s_bool_rc = 0}
  return(noc_s_bool_rc)
}

DYS_EXER_CHECK <- function(dys_exer){
  if (dys_exer == "On slight exertion")          {dys_exer_effect = -226.09}
  else if (dys_exer == "On moderate exercise")  {dys_exer_effect = -560.37}
  else if (dys_exer == "On rigorous exercise")  {dys_exer_effect = -224.83}
  else if (dys_exer == 'No dyspnea on ex.')     {dys_exer_effect = 0}
  return(dys_exer_effect)
}

#######For rate of change coefficient
DYS_EXER_CHECK_RC <- function(dys_exer){
  if (dys_exer == "On slight exertion")    {dys_exer_effect_rc = 15.79}
  else if (dys_exer == "On moderate exercise")  {dys_exer_effect_rc = -2.43}
  else if (dys_exer == "On rigorous exercise")  {dys_exer_effect_rc = -1.81}
  else if (dys_exer == 'No dyspnea on ex.')     {dys_exer_effect_rc = 0}
  return(dys_exer_effect_rc)
}

SEX_CHECK <- function(sex, dys_exer){
  if ((sex == 'female') & (dys_exer == 'On rigorous exercise')) {dys_sex_effect = 149.38}
  else if ((sex == 'female') & (dys_exer == 'On moderate exercise')) {dys_sex_effect = 575.01}
  else if ((sex == 'female') & (dys_exer == 'On slight exertion'))   {dys_sex_effect = -368.46}
  else if ((sex == 'female') & (dys_exer == 'No dyspnea on ex.'))    {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'On rigorous exercise'))   {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'On moderate exercise'))   {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'On slight exertion'))     {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'No dyspnea on ex.'))      {dys_sex_effect = 0}
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

ACE <- function(age) {
  age_bool=AGE_CHECK(age)
  if (age_bool == 1 | age_bool == 0) {
    ace = (-15.76*age) + ((-139.62+29.47)*age_bool)-29.47
  } else if ((age_bool == 2) | (age_bool == 3)) {
    ace = 0
  } 
  return(ace)
}

#######For rate of change coefficient
ACE_RC <- function(age) {
  age_bool=AGE_CHECK(age)
  if (age_bool == 1 | age_bool == 0) {
    ace_rc = (-0.81*age) + ((6.68-2.69)*age_bool)+2.69
  }  else if ((age_bool == 2) | (age_bool == 3)) {
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
                 #age_bool,                                        ###JK
                 ba_use,
                 ba_use_bool,
                 dys_exer,
                 noc_s,
                 noc_s_bool,
                 sex) {
  b_fev =
    intercept +                                   
    AS(sex, alb) +                             
    HS (sex, height_square) +                               
    (cum_smoke*trig*smoke_pack_years_trig_effect) +                          
    SEX_FM(sex) +                         
    ACE(age) +                                                      ###JK
    BUE(ba_use, ba_use_bool) +             
    NSB(noc_s, noc_s_bool) +               
    DEE(dys_exer, DYS_EXER_CHECK(dys_exer)) +       #UPDATED
    DSE(sex, dys_exer, SEX_CHECK(sex,dys_exer)) +   #UPDATED
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
FEV_RC <- function (follow_up_baseline, 
                    trig, 
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
                    #age_bool,                                       ###JK
                    ba_use, 
                    ba_use_bool_rc, 
                    dys_exer,
                    noc_s, 
                    noc_s_bool_rc, 
                    sex) {
  b_fev_rc =
    intercept_rc +  
    (follow_up_baseline*follow_up_baseline_effect) +
    SEX_FM_RC(sex) +
    ACE_RC(age) +                                                           ###JK
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

#function for generating binary code
BINARY_CODE_FROM_INPUTS <- function(age, follow_up_baseline, trig, hema, alb, glob, alk_phos,
                                    white_bc, qrs, alcohol, wine, cocktail, height_square,
                                    cum_smoke, ba_use, dys_exer, noc_s) {
  if(is.null(age)) {a = 0} else {a = 1}
  if(is.null(follow_up_baseline)) {b = 0} else {b = 1}
  if(is.null(trig)) {c = 0} else {c = 1}
  if(is.null(hema)) {d = 0} else {d = 1}
  if(is.null(alb)) {e = 0} else {e = 1}
  if(is.null(glob)) {f = 0} else {f = 1}
  if(is.null(alk_phos)) {g = 0} else {g = 1}
  if(is.null(white_bc)) {h = 0} else {h = 1}
  if(is.null(qrs)) {i = 0} else {i = 1}
  if(is.null(alcohol)) {j = 0} else {j = 1}
  if(is.null(wine)) {k = 0} else {k = 1}
  if(is.null(cocktail)) {l = 0} else {l = 1}
  if(is.null(height_square)) {m = 0} else {m = 1}
  if(is.null(cum_smoke)) {n = 0} else {n = 1}
  if(is.null(sex)) {o = 0} else {o = 1}
  if(is.null(ba_use)) {p = 0} else {p = 1}
  if(is.null(dys_exer)) {q = 0} else {q = 1}
  if(is.null(noc_s)) {r = 0} else {r = 1}
  bc <- paste(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
  return(bc)
}

#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               
               ####inputs on ROW #1
               fluidRow(
                 
                 #row 1, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "age",
                              "Age (year)",
                              value = NULL,
                              min = 0,
                              max = 250,
                              step = 1,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 1, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "follow_up_baseline",
                              # "Follow-up since baseline (year)",
                              "Follow-up",
                              value = NULL,
                              min = -100,
                              max = 100,
                              width = 160
                            )
                        )
                 )
               ),
               
               ####inputs on ROW #2
               fluidRow(
                 
                 #row 2, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "trig",
                              "Triglycerides (mg/dl)",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 2, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "hema",
                              "Hematocrit (%)",
                              value = NULL,
                              min = 0,
                              max = 100,
                              step = 0.01,
                              width = 160
                            )
                        )
                 )
               ),
               ####inputs on ROW #3
               fluidRow(
                 
                 #row 3, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "alb",
                              "Albumin (mg/L)",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 3, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "glob",
                              "Globulin (g/L)",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 )
               ),
               ####inputs on ROW #4
               fluidRow(
                 
                 #row 4, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "alk_phos",
                              "Alkaline Phosphotase",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 4, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "white_bc",
                              "White blood cells(10^9/L)",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 )
               ),
               ####inputs on ROW #5
               fluidRow(
                 
                 #row 5, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "qrs",
                              "QRS interval (0.01 sec)",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 5, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "alcohol",
                              "Alcohol index (ozs/wk)",
                              value = NULL,
                              step = 0.01,
                              width = 160
                            )
                        )
                 )
               ),
               ####inputs on ROW #6
               fluidRow(
                 
                 #row 6, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "wine",
                              "Wine intake (glasses/wk)",
                              value = NULL,
                              min = 0,
                              step = 0.01,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 6, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "cocktail",
                              "Cocktail intake (drinks/wk)",
                              value = NULL,
                              min = 0,
                              step = 0.01,
                              width = 160
                            )
                        )
                 )
               ),
               ####inputs on ROW #7
               fluidRow(
                 
                 #row 7, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "height_square",
                              "Height square (cm^2)",
                              value = NULL,
                              min = 0,
                              step = 0.01,
                              width = 160
                            )
                        )
                 ),
                 
                 #row 7, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            numericInput(
                              "cum_smoke",
                              "Cum. smoke pack-year",
                              value = NULL,
                              min = 0,
                              step = 0.01,
                              width = 160
                            )
                        )
                 )
               ),
               ####inputs on ROW #8
               fluidRow(
                 
                 #row 8, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            selectInput(
                              "sex",
                              "sex",
                              list('female', 'male'),
                              selected = 'male')
                        )
                 ),
                 
                 #row 8, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            selectInput(
                              "ba_use",
                              "Bronchodilator or aerosol",
                              list('Current use', 'Former use', 'No use'),
                              selected = 'No use'
                            )
                        )
                 )
               ),
               
               ####inputs on ROW #9
               fluidRow(
                 
                 #row 9, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            selectInput(
                              "dys_exer",
                              "Dyspnea on exertion",
                              list(
                                'On rigorous exercise',
                                'On moderate exercise',
                                'On slight exertion',
                                'No dyspnea on ex.'
                              ),
                              selected = 'No dyspnea on ex.'
                            )
                        )
                 ),
                 
                 #row 9, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            selectInput(
                              "noc_s",
                              "Nocturnal symptoms",
                              list('Yes', 'Maybe', 'No'),
                              selected = 'No'
                            )
                        )
                 )
               ),
               
               ####inputs on ROW #10
               fluidRow(
                 
                 #row 10, left input
                 column(5,
                        div(style = "font-size: 12px;",
                            actionButton("submitButton", "Submit")
                        )
                 ),
                 
                 #row 10, right input
                 column(5,
                        div(style = "font-size: 12px;",
                            actionButton("clearButton", "Clear")
                        )
                 )
               )
               
        )
      ), width=6
    ),
    
    
    mainPanel (
      tags$p("Baseline FEV Effect (mL):"),
      verbatimTextOutput("baseline_FEV"),
      tags$p("Rate of FEV change, mL/y:"),
      verbatimTextOutput("rate_of_change_FEV"),
      tags$p("Plot graph of linear regression:"),
      plotOutput("plot"),
      tags$p("Regression line:"),
      verbatimTextOutput("regression_line"), width = 5, class = 'rightAlign'
    )
  )
)

server <- function(input, output, session) {
  
  #observeEvent(input$submitButton)
  # observeEvent(input$button, {
  #   cat("Showing", "rows\n")
  # })
  #####################################################################
  ######################PLOT FEV#######################################
  ##################################################################### 
  output$plot <- renderPlot({
    #age_bool=AGE_CHECK(input$age)                                        ###JK
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)
    
    fev_slope = FEV(input$trig, input$hema,
                    input$alb, input$glob,
                    input$alk_phos, input$white_bc,
                    input$qrs, input$alcohol,
                    input$wine, input$cocktail,
                    input$height_square, input$cum_smoke,
                    input$age, 
                    #age_bool,                                          ###JK
                    input$ba_use, ba_use_bool,
                    input$dys_exer,
                    input$noc_s, noc_s_bool,
                    input$sex)
    
    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    female_male_effect_rc=SEX_FM_RC(input$sex)
    
    fev_intercept = FEV_RC(input$follow_up_baseline, input$trig, input$hema,
                           input$alb, input$glob,
                           input$alk_phos, input$white_bc,
                           input$qrs, input$alcohol,
                           input$wine, input$cocktail,
                           input$height_square, input$cum_smoke,
                           input$age, 
                           #age_bool,                                           ###JK
                           input$ba_use, ba_use_bool_rc,
                           input$dys_exer,
                           input$noc_s, noc_s_bool_rc, input$sex)
    
    #validate calculated slope and intercept
    # validate(
    # 	need(input$fev_slope != NULL, "Cannot calculate slope due to missing parameters")
    # 	need(input$fev_intercept != NULL, "Cannot calculate y-intercept due to missing parameters")
    # )
    
    ## Setup up coordinate system (with x == y aspect ratio):
    plot(c(0, a), c(2000, (((a+b*2000)-a)/2000), type = "n", xlab = "x", ylab = "y"))
    ## the x- and y-axis, and an integer grid
    abline(fev_slope, fev_intercept, col = "red")
    abline(h = 0, v = 0, col = "gray60")
  })
  
  ################NEW GGPLOT CODE############################
  #create data frame
  df<-data.frame(slope=fev_slope, intercept=fev_intercept)
  
  ggplot()+
    scale_x_continuous(name="x", limits=c(-100,600)) +
    scale_y_continuous(name="y", limits=c(-8000,2000)) +
    geom_abline(data=df, mapping=aes(slope=df[1,1], intercept=df[1,2]))
  ################END OF NEW GGPLOT CODE########################
  
  output$regression_line <- renderText({
    #age_bool=AGE_CHECK(input$age)                                        ###JK
    ba_use_bool=BA_USE_CHECK(input$ba_use)
    noc_s_bool=NSB_CHECK(input$noc_s)
    dys_exer_effect=DYS_EXER_CHECK(input$dys_exer)
    dys_sex_effect=SEX_CHECK(input$sex,input$dys_exer)
    
    fev_slope = FEV(input$trig, input$hema,
                    input$alb, input$glob,
                    input$alk_phos, input$white_bc,
                    input$qrs, input$alcohol,
                    input$wine, input$cocktail,
                    input$height_square, input$cum_smoke,
                    input$age, 
                    #age_bool,                                          ###JK
                    input$ba_use, ba_use_bool,
                    input$dys_exer,
                    input$noc_s, noc_s_bool,
                    input$sex)
    
    ba_use_bool_rc=BA_USE_CHECK_RC(input$ba_use)
    noc_s_bool_rc=NSB_CHECK_RC(input$noc_s)
    dys_exer_effect_rc=DYS_EXER_CHECK_RC(input$dys_exer)
    female_male_effect_rc=SEX_FM_RC(input$sex)
    
    fev_intercept = FEV_RC(input$follow_up_baseline, input$trig, input$hema,
                           input$alb, input$glob,
                           input$alk_phos, input$white_bc,
                           input$qrs, input$alcohol,
                           input$wine, input$cocktail,
                           input$height_square, input$cum_smoke,
                           input$age, 
                           #age_bool,                                           ###JK
                           input$ba_use, ba_use_bool_rc,
                           input$dys_exer,
                           input$noc_s, noc_s_bool_rc, input$sex)
    
    
    
    paste('y', '=', fev_slope, fev_intercept, '*', 'x')
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
  # #NULL for triglycerides
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$trig)){return()}
  #   else  {
  #     react$trig <- input$trig
  #   }
  # }
  # )
  # #NULL for hematocrit
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$hema)){return()}
  #   if(input$hema < 0){                                   
  #     react$hema =0
  #     updateNumericInput(session, "hema", hema = react$hema)
  #   } else  {
  #     react$hema <- input$hema
  #   }
  # }
  # )
  # #NULL for albumin
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$alb)){return()}
  #   else  {
  #     react$alb <- input$alb
  #   }
  # }
  # )
  # #NULL for globulin
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$glob)){return()}
  #   else  {
  #     react$glob <- input$glob
  #   }
  # }
  # )
  # #NULL for Alkaline Phosphotase
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$alk_phos)){return()}
  #   else  {
  #     react$alk_phos <- input$alk_phos
  #   }
  # }
  # )
  # #NULL for white blood cell count
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$white_bc)){return()}
  #   else  {
  #     react$white_bc <- input$white_bc
  #   }
  # }
  # )
  # #NULL for QRS interval (hundredth of sec)
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$qrs)){return()}
  #   else  {
  #     react$qrs <- input$qrs
  #   }
  # }
  # )
  # #NULL for alcohol index
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$alcohol)){return()}
  #   if(input$hema < 0){                                   
  #     react$hema =0
  #     updateNumericInput(session, "hema", hema = react$hema)
  #   } else  {
  #     react$alcohol <- input$alcohol
  #   }
  # }
  # )
  # #NULL for wine intake
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$wine)){return()}
  #   if(input$hema < 0){                                   
  #     react$hema =0
  #     updateNumericInput(session, "hema", hema = react$hema)
  #   } else  {
  #     react$wine <- input$wine
  #   }
  # }
  # )
  # #NULL for cocktail intake
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$cocktail)){return()}
  #   if(input$hema < 0){                                   
  #     react$hema =0
  #     updateNumericInput(session, "hema", hema = react$hema)
  #   } else  {
  #     react$cocktail <- input$cocktail
  #   }
  # }
  # )
  # #NULL for Height
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$height_square)){return()}
  #   if(input$hema < 0){                                   
  #     react$hema =0
  #     updateNumericInput(session, "hema", hema = react$hema)
  #   } else  {
  #     react$height_square <- input$height_square
  #   }
  # }
  # )
  # #NULL for cumulative smoke pack-year
  # react <- reactiveValues()
  # observe({
  #   if(is.na(input$cum_smoke)){return()}
  #   if(input$cum_smoke < 0){                                   
  #     react$cum_smoke = 0
  #     #updateNumericInput(session, "cum_smoke", cum_smoke = react$cum_smoke)
  #   } else  {
  #     react$cum_smoke <- input$cum_smoke
  #   }
  # }
  # )
  
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
    
    if(is.na(input$age)){return()} #check if age input is NULL
    
    #perform parameter checks
    #age_bool=AGE_CHECK(input$age)                            ###JK
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
        input$age, 
        #age_bool,                                                  ###JK
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