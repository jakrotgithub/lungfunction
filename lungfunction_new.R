
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

#age_bol <- 1 | 0

#Define UI for application that generates simulation, showing individualized
#prediction of adulthood lung function decline for Framingham offspring cohort analysis
ui <- fluidPage(
        theme = shinytheme("united"),
        tags$head(tags$script(src = "message-handler.js")),
        
        #Application title
        titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
        
        sidebarLayout(
                sidebarPanel(
                        # numericInput("inter", 
                        #              "Intercept", 
                        #              value = 1127.26,
                        #              step = 1000, 
                        #              width = 200),
                        numericInput("age", 
                                     "Age (year)", 
                                     value = 35, 
                                     min = 35, 
                                     max = 64, 
                                     step = 1, 
                                     width = 200),
                        # column(8,
                        #        selectInput("age_cat_effect",
                        #                    "Age category (vs. 20-34 y)",
                        #                    list('35-49 y', '50-64 y'),
                        #                    selected = '35-49 y',
                        #                    width = 200)),
                        # column(12,
                        #        numericInput("b_age_cat",
                        #                     "Baseline of age category (vs. 20-24 y)",
                        #                     value = -29.47,
                        #                     step = 0.01,
                        #                     width = 200),
                        #        offset = 7)),
                        numericInput("sex", 
                                     "Sex (female vs. male)",
                                     value = -660.44, 
                                     step = 100, 
                                     width = 200),
                        numericInput("trig", 
                                     "Triglycerides (mg/dl)", 
                                     value = -0.30, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("hema", 
                                     "Hematocrit (%)", 
                                     value = -12.28, 
                                     min = 0,
                                     max = 100,
                                     step = 0.01, 
                                     width = 200),
                        numericInput("alb", 
                                     "Albumin (mg/L)", 
                                     value = 11.33, 
                                     step = 0.01, 
                                     width = 200),
                        # numericInput("alb_sex", 
                        #              "Albumin*Sex (female vs. male)", 
                        #              value = -9.50,
                        #              step = 0.01, 
                        #              width = 200),
                        numericInput("glob", 
                                     "Globulin (g/L)", 
                                     value = -3.49, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("alk_phos", 
                                     "Alkaline Phosphotase (units)", 
                                     value = -1.48, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("white_bc", 
                                     "White blood cell count (10^9/L)", 
                                     value = -0.20, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("qrs", 
                                     "QRS interval (hundredth of sec)", 
                                     value = 27.37, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("alcohol", 
                                     "Alcohol index (ozs/wk)", 
                                     value = -5.99, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("wine", 
                                     "Wine intake (glasses/wk)", 
                                     value = 10.76, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("cocktail", 
                                     "Cocktail intake (drinks/wk)", 
                                     value = -0.60, 
                                     step = 0.01, 
                                     width = 200),
                        numericInput("height", 
                                     "Height square (cm^2)", 
                                     value = 0.11, 
                                     step = 0.01, 
                                     width = 200),
                        # numericInput("height_sex", 
                        #              "Height square*Sex (female vs. male)", 
                        #              value = -0.02, 
                        #              step = 0.01, 
                        #              width = 200),
                        numericInput("cum_smoke", 
                                     "Cumulative smoke pack-year", 
                                     value = -3.46, 
                                     step = 0.01, 
                                     width = 200),
                        # numericInput("smoke_py", 
                        #              "Smoke  pack-years*Triglycerides", 
                        #              value = 0.003,
                        #              step = 0.001, 
                        #              width = 200),
                        column(8,
                               selectInput("ba_use", 
                                           "Bronchodilator or aerosol (vs. no use)", 
                                           list('Current use', 'Former use'),
                                           selected = 'Current use')),
                        # column(12, 
                        #        numericInput("b_bron_or_aer", 
                        #                     "Baseline of Bronchodilator or aerosol (vs. no use)", 
                        #                     value = -264.01, 
                        #                     min = -1000,
                        #                     max = 1000,
                        #                     step = 0.01, 
                        #                     width = 200),
                        # offset = 7)),
                        column(8,
                               selectInput("bron_exer", 
                                           "Dyspnea on exertion (vs. none)", 
                                           list('On rigorous exercise', 'On moderate exercise', 'On slight exertion'),
                                           selected = 'On slight exertion')),
                        # column(12, 
                        #        numericInput("b_bron_exer", 
                        #                     "Baseline of dyspnea on exertion (vs. none)",
                        #                     value = -224.83,
                        #                     step = 0.01,
                        #                     width = 200),
                        #        offset = 7)),
                        column(8,
                               selectInput("bron_sex", 
                                           "Dyspnea*Sex (vs. male or no dyspnea)", 
                                           list('Female, on rigorous exercise', 'Female, on moderate exercise', 
                                                'Female, on slight exertion'),
                                           selected = 'On slight exertion')),
                        # column(12, 
                        #        numericInput("b_bron_sex", 
                        #                     "Baseline of dyspnea*Sex (vs. male or no dyspnea)",
                        #                     value = 149.38,
                        #                     step = 0.01,
                        #                     width = 200),
                        #        offset = 7)),
                        column(8,
                               selectInput("noc_s", 
                                           "Nocturnal symptoms (vs. none)", 
                                           list('Yes', 'Maybe'),
                                           selected = 'On slight exertion'))
                        #       column(12, 
                        #              numericInput("b_noc_symp", 
                        #                         "Baseline of nocturnal symptoms (vs. none)", 
                        #                           value = -342.92,
                        #                           step = 0.01,
                        #                           width = 200),
                        #              offset = 7))
                        # ),
                ),
                
                mainPanel (
                        # tags$p("FEV (mL)"),
                        # verbatimTextOutput("baseline_FEV"),
                        tags$p("Age Category Effect:"),
                        verbatimTextOutput("age_cat_effect"),
                        tags$p("Bronchodilator or aerosol (vs. no use) Effect:"), #for end product have to remove
                        verbatimTextOutput("ba_use_effect"), #for end product have to remove
                        tags$p("Dyspnea on exertion (vs. none):"),
                        verbatimTextOutput("noc_s_effect")
                     #   tags$p("Dyspnea*Sex (vs. male or no dyspnea):"),
                     #   verbatimTextOutput(""),
                     #   tags$p("Nocturnal symptoms (vs. none):"),
                     #   verbatimTextOutput("")
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
}

#Run the application
shinyApp(ui = ui, server = server)
