#comments for testing GitHub
library(shiny)
library(shinythemes)

ACE <- function(age, age_bol) { #does not depend on input and output; no need to type input$age
        ace = (-15.76*age) + ((-139.62+29.47)*age_bol)-29.47
        return(ace)
}

BUE <- function(ba_use, ba_use_bol) {
        bue = (-213.06*ba_use_bol)-50.95
        return(bue)
}

NSB <- function(noc_s, noc_s_bol) {
        nsb = (-219.16*noc_s_bol)-342.92
        return(nsb)
}

DEE <- function (dys_exer, dys_exer_effect) {
        dee = dys_exer_effect
        return(dee)
}

DSE <- function(sex, dys_exer, dys_sex_effect) {
        dse = dys_sex_effect
        return(dse)
}

i = 1127.26 #intercept defined (Table 2. Wenjia's manuscript)
alb_sex = -9.50 #Albumin*Sex (female vs. male)
h_sex = -0.02 #Height square*Sex (female vs. male)
smoke_trig = -0.003 #Smoke pack-years*Triglycerides
female_male = -660.44 #Sex (female vs. male)

trig_effect = -0.30
hema_effect = -12.28
alb_effect = 11.33
glob_effect = -3.49
alk_phos_effect = -1.48
white_bc_effect = -0.20
qrs_effect = 27.37
alcohol_effect = -5.99
wine_effect = 10.76
cocktail_effect = -0.60
height_effect = 0.11
cum_smoke_effect = -3.46

#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
        theme = shinytheme("united"),
        tags$head(tags$script(src = "message-handler.js")),
        
        #Application title
        titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
        
        sidebarLayout(
                sidebarPanel(
                        numericInput("age", 
                                     "Age (year)", 
                                     value = 35, 
                                     min = 35, 
                                     max = 64, 
                                     step = 1, 
                                     width = 200),
                        selectInput("sex",
                                    "Sex",
                                    list('female', 'male'),
                                    selected = 'male'),
                        numericInput("trig", 
                                     "Triglycerides (mg/dl)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("hema", 
                                     "Hematocrit (%)", 
                                     value = " ", 
                                     min = 0,
                                     max = 100,
                                     step = 0.01, 
                                     width = 200),
                        numericInput("alb", 
                                     "Albumin (mg/L)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("glob", 
                                     "Globulin (g/L)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("alk_phos", 
                                     "Alkaline Phosphotase (units)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("white_bc", 
                                     "White blood cell count (10^9/L)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("qrs", 
                                     "QRS interval (hundredth of sec)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("alcohol", 
                                     "Alcohol index (ozs/wk)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("wine", 
                                     "Wine intake (glasses/wk)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("cocktail", 
                                     "Cocktail intake (drinks/wk)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("height", 
                                     "Height square (cm^2)", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("cum_smoke", 
                                     "Cumulative smoke pack-year", 
                                     value = " ", 
                                     step = 0.01, 
                                     width = 200),
                        column(8,
                               selectInput("ba_use", 
                                           "Bronchodilator or aerosol (vs. no use)", 
                                           list('Current use', 'Former use'),
                                           selected = 'Current use')),

                        column(8,
                               selectInput("dys_exer", 
                                           "Dyspnea on exertion (vs. none)", 
                                           list('On rigorous exercise', 'On moderate exercise', 'On slight exertion'),
                                           selected = 'On slight exertion')),
                        column(8,
                               selectInput("noc_s", 
                                           "Nocturnal symptoms (vs. none)", 
                                           list('Yes', 'Maybe'),
                                           selected = 'On slight exertion'))

                ),
                
                mainPanel (
                        # tags$p("FEV (mL)"),
                        # verbatimTextOutput("baseline_FEV"),
                        tags$p("Age Category Effect:"),
                        verbatimTextOutput("age_cat_effect"),
                        tags$p("Bronchodilator or aerosol (vs. no use) Effect:"), #for end product have to remove
                        verbatimTextOutput("ba_use_effect"), #for end product have to remove
                        tags$p("Dyspnea on exertion (vs. none) Effect:"),
                        verbatimTextOutput("dys_exer_effect"),
                        tags$p("Dyspnea*Sex (vs. male or no dyspnea) Effect:"),
                        verbatimTextOutput("dys_sex_effect"),
                        tags$p("Nocturnal symptoms (vs. none) Effect:"),
                        verbatimTextOutput("noc_s_effect"),
                        tags$p("Baseline FEV Effect:"),
                        verbatimTextOutput("baseline_FEV")
                )
        )
)

server <- function(input, output, session) {
        
        # output$age_cat_effect <- renderText({
        #   ACE(1)
        #   if (input$age_cat_effect == '35-49 y') {
        #     age_bol = 0
        #   } else {
        #     age_bol = 1
        #   }
        #   ACE(age_bol)
        # })
        
        output$age_cat_effect <- renderText({ 
                if (input$age <=49) { #age category assigned 
                        age_bol = 0
                } else {
                        age_bol = 1
                }
                ACE(input$age, age_bol)
        })
        
        output$ba_use_effect <- renderText({
                if(input$ba_use == 'Current use') {
                        ba_use_bol = 1
                } else {
                        ba_use_bol = 0
                }
               BUE(input$ba_use, ba_use_bol) 
        })
        
        output$noc_s_effect <- renderText({
                if(input$noc_s == 'Yes') {
                        noc_s_bol = 0
                } else {
                        noc_s_bol = 1
                }
                NSB(input$noc_s, noc_s_bol)
        })

        
        output$dys_exer_effect <- renderText({
                if(input$dys_exer == "On slight exertion") {
                        dys_exer_effect = -226.09
                } else if (input$dys_exer == "On moderate exercise") {
                        dys_exer_effect = -560.37
                } else {
                        dys_exer_effect = -224.83
                }
                DEE(input$dys_exer, dys_exer_effect)
        })

        output$dys_sex_effect <- renderText({
                if ((input$sex == 'male') & (input$dys_exer == 'On rigorous exercise')) {
                        dys_sex_effect = -224.83
                } else if ((input$sex == 'male') & (input$dys_exer == 'On moderate exercise')) {
                        dys_sex_effect = -560.37
                } else if ((input$sex == 'male') & (input$dys_exer == 'On slight exertion')) {
                        dys_sex_effect = -226.09
                } else if ((input$sex == 'female') & (input$dys_exer == 'On rigorous exercise')) {
                        dys_sex_effect = 149.38
                } else if ((input$sex == 'female') & (input$dys_exer == 'On moderate exercise')) {
                        dys_sex_effect = 575.01
                } else if ((input$sex == 'female') & (input$dys_exer == 'On slight exertion')) {
                        dys_sex_effect = -368.46
                }
                DSE(input$sex, input$dys_exer, dys_sex_effect) #DOES NOT FUNCTION, get error message!!!
        })
        
        # CI = effect +/- 1.96*SE
}

# output$baseline_FEV <- function (trig, hema, 
#                                  alb, glob,
#                                  alk_phos, white_bc, 
#                                  qrs, alcohol, 
#                                  wine, cocktail, 
#                                  height, cum_smoke, 
#                                  age, age_bol, 
#                                  ba_use, ba_use_bol, 
#                                  dys_exer, dys_exer_effect,
#                                  noc_s, noc_s_bol,
#                                  dys_sex_effect, sex, 
#                                  ) {
#   b_fev =
#     i +
#     alb_sex +
#     h_sex +
#     smoke_trig +
#     female_male +
#     ACE(input$age, age_bol) +
#     BUE(input$ba_use, ba_use_bol) +
#     NSB(input$noc_s, noc_s_bol) +
#     DEE(input$dys_exer, dys_exer_effect) +
#     DSE(input$sex, input$dys_exer, dys_sex_effect) +
#     (input$trig*trig_effect) +
#     (input$hema*hema_effect) +
#     (input$alb*alb_effect) +
#     (input$glob*glob_effect) +
#     (input$alk_phos*alk_phos_effect) +
#     (input$white_bc*white_bc_effect) +
#     (input$qrs*qrs_effect) +
#     (input$alcohol*alcohol_effect) +
#     (input$wine*wine_effect) +
#     (input$cocktail*cocktail_effect) +
#     (input$height*height_effect) +
#     (input$cum_smoke*cum_smoke_effect) +
#   return(b_fev)
# }}

#Run the application
shinyApp(ui = ui, server = server)
