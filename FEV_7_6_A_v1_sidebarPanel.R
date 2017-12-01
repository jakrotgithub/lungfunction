  button_width <- 160
  
  sidebarPanel(
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
               column(5,div(style = "font-size: 12px;",numericInput("height","Height square (cm^2)",value = NULL,min = 0,step = 0.01,width = button_width))),
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
  