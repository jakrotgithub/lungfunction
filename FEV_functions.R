#CHANGE NOTES:
# 2017-10-08: added 'Not Selected' options for:
#             NSB_CHECK(), NSB_CHECK_RC(), BA_USE_CHECK(), BA_USE_CHECK_RC(), DYS_EXER_CHECK(), DYS_EXER_CHECK_RC()
#             updated BINARY_CODE_FROM_INPUTS() - changed sex, ba_use, dys_exer, noc_s from is.null(...) to if(...)

#List of function used in FEV program:
#AGE_CHECK(age) - checks patient's age falls into one of the two age categories: 35-49 y or 50-64 y.
#BA_USE_CHECK(ba_use) - produces boolean, indicating the use of bronchodilator or aerosol by patient (for baseline)
#BA_USE_CHECK_RC(ba_use) - produces boolean, indicating the use of bronchodilator or aerosol by patient (for FEV rate of change)
#NSB_CHECK(noc_s) - produces boolean, indicating patient's nocturnal symptoms (for baseline)
#NSB_CHECK_RC(noc_s) - produces boolean, indicating patient's nocturnal symptoms (for FEV rate of change)
#DYS_EXER_CHECK(dys_exer) - dyspnea on exertion/varying levels of exercise (for baseline)
#DYS_EXER_CHECK_RC(dys_exer) - dyspnea on exertion/varying levels of exercise (for FEV rate of change)
#SEX_CHECK(sex, dys_exer) - checks if female & dyspnea on exertion/varying levels of exercise
#AS(sex, alb) - effect of female on level of albumin (Albumin*sex)
#HS(sex, height_square) - effect of female on height (Height square * sex)
#SEX_FM(sex) - effect on sex on baseline
#SEX_FM_RC(sex) - effect of sex on FEV rate of change
#ACE(age) - effect of age on baseline (2 components: Age, y effect & Age category)
#ACE_RC(age) - effect of age on FEV rate of change (2 components: Age, y effect & Age category)
#BUE(ba_use, ba_use_bool) - bronchodilator or aerosol use by patient (for baseline)
#BUE_RC(ba_use, ba_use_bool_rc) - bronchodilator or aerosol use by patient (for rate of change)
#NSB(noc_s, noc_s_bool) - nocturnal symptoms' effect (on baseline)
#NSB_RC(noc_s, noc_s_bool_rc) - nocturnal symptoms' effect (on rate of change)
#FEV <- function (trig,...,sex) - baseline forced expiratory volume (FEV); intercept of linear regression equation
#FEV_RC <- function (follow_up_baseline,...,sex) - rate of FEV change; slope of linear regression equation
#BINARY_CODE_FROM_INPUTS(...) - binary code that tracks doctor's inputs (NULL or not NULL) & produces the name of model (i.e. binary code)

#functions that determine boolean values
AGE_CHECK <- function(age){
       if (is.null(age))              {age_bool = 3}
  else if ((35 <= age) & (age <= 49)) {age_bool = 0}
  else if ((50 <= age) & (age <= 64)) {age_bool = 1}
  else if ((age <= 34) | (age >= 65)) {age_bool = 2}
  return(age_bool)  
}

BA_USE_CHECK <- function(ba_use){
       if (ba_use == 'Current use')   {ba_use_bool = 1}
  else if (ba_use == 'Former use')    {ba_use_bool = 0}
  else if (ba_use == 'No use')        {ba_use_bool = 0}
  else if (ba_use == 'Not Selected')  {ba_use_bool = 0}
  return(ba_use_bool)
}

#######For rate of change coefficient
BA_USE_CHECK_RC <- function(ba_use){
       if (ba_use == 'Current use') {ba_use_bool_rc = 1}
  else if (ba_use == 'Former use')  {ba_use_bool_rc = 0}
  else if (ba_use == 'No use')      {ba_use_bool_rc = 0}
  else if (ba_use == 'Not Selected'){ba_use_bool_rc = 0}
  return(ba_use_bool_rc)
}

NSB_CHECK <- function(noc_s){
       if (noc_s == 'Yes')          {noc_s_bool = 0}
  else if (noc_s == 'Maybe')        {noc_s_bool = 1}
  else if (noc_s == 'No')           {noc_s_bool = 0}
  else if (noc_s == 'Not Selected') {noc_s_bool = 0}
  return(noc_s_bool)
}

#######For rate of change coefficient
NSB_CHECK_RC <- function(noc_s){
       if (noc_s == 'Yes')          {noc_s_bool_rc = 1}
  else if (noc_s == 'Maybe')        {noc_s_bool_rc = 0}
  else if (noc_s == 'No')           {noc_s_bool_rc = 0}
  else if (noc_s == 'Not Selected') {noc_s_bool_rc = 0}
  return(noc_s_bool_rc)
}

DYS_EXER_CHECK <- function(dys_exer){
       if (dys_exer == "On slight exertion")    {dys_exer_effect = -226.09}
  else if (dys_exer == "On moderate exercise")  {dys_exer_effect = -560.37}
  else if (dys_exer == "On rigorous exercise")  {dys_exer_effect = -224.83}
  else if (dys_exer == 'No dyspnea on ex.')     {dys_exer_effect = 0}
  else if (dys_exer == 'Not Selected')          {dys_exer_effect = 0}
  return(dys_exer_effect)
}

#######For rate of change coefficient
DYS_EXER_CHECK_RC <- function(dys_exer){
       if (dys_exer == "On slight exertion")    {dys_exer_effect_rc = 15.79}
  else if (dys_exer == "On moderate exercise")  {dys_exer_effect_rc = -2.43}
  else if (dys_exer == "On rigorous exercise")  {dys_exer_effect_rc = -1.81}
  else if (dys_exer == 'No dyspnea on ex.')     {dys_exer_effect_rc = 0}
  else if (dys_exer == 'Not Selected')          {dys_exer_effect_rc = 0}
  return(dys_exer_effect_rc)
}

SEX_CHECK <- function(sex, dys_exer){
       if ((sex == 'female') & (dys_exer == 'On rigorous exercise'))   {dys_sex_effect = 149.38}
  else if ((sex == 'female') & (dys_exer == 'On moderate exercise'))   {dys_sex_effect = 575.01}
  else if ((sex == 'female') & (dys_exer == 'On slight exertion'))     {dys_sex_effect = -368.46}
  else if ((sex == 'female') & (dys_exer == 'No dyspnea on ex.'))      {dys_sex_effect = 0}
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
    height_square_sex = -0.02*(height_square^2)
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
# 
# SFM <- function(dys_exer) { #DK - Right now all this function does is call the DYS_EXER_CHECK_RC function, not sure if this is correct, might need to update
#   sex_fm_rc = DYS_EXER_CHECK_RC(dys_exer)
#   return(sex_fm_rc)
# }

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

# #DEE - Dyspnea on Exertion Effect
# DEE <- function (dys_exer, dys_exer_effect) {
#   dee = dys_exer_effect
#   return(dee)
# }
# 
# #######For rate of change coefficient
# DEE_RC <- function (dys_exer, dys_exer_effect_rc) {
#   # dee_rc = dys_exer_effect_rc
#   dee_rc = DYS_EXER_CHECK_RC(dys_exer) 
#   return(dee_rc)
# }

# #DSE - Dyspnea Sex Effect
# DSE <- function(sex, dys_exer, dys_sex_effect) {
#   dse = dys_sex_effect
#   return(dse)
# }

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
    DYS_EXER_CHECK(dys_exer) +       #UPDATED
    SEX_CHECK(sex, dys_exer) +   #UPDATED
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
    DYS_EXER_CHECK_RC(dys_exer) +      
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
BINARY_CODE_FROM_INPUTS <- function(
  age,
  follow_up_baseline, 
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
  sex, #selectInput
  ba_use,#selectInput
  dys_exer,#selectInput
  noc_s#selectInput
) {
  if(is.na(age)) {F1 = 0} else {F1 = 1}
  if(is.na(follow_up_baseline)) {F2 = 0} else {F2 = 1}
  if(is.na(trig)) {F3 = 0} else {F3 = 1}
  if(is.na(hema)) {F4 = 0} else {F4 = 1}
  if(is.na(alb)) {F5 = 0} else {F5 = 1}
  if(is.na(glob)) {F6 = 0} else {F6 = 1}
  if(is.na(alk_phos)) {F7 = 0} else {F7 = 1}
  if(is.na(white_bc)) {F8 = 0} else {F8 = 1}
  if(is.na(qrs)) {F9 = 0} else {F9 = 1}
  if(is.na(alcohol)) {F10 = 0} else {F10 = 1}
  if(is.na(wine)) {F11 = 0} else {F11 = 1}
  if(is.na(cocktail)) {F12 = 0} else {F12 = 1}
  if(is.na(height_square)) {F13 = 0} else {F13 = 1}
  if(is.na(cum_smoke)) {F14 = 0} else {F14 = 1}
  if(sex == 'Not Selected') {F15 = 0} else {F15 = 1}
  if(ba_use == 'Not Selected') {F16 = 0} else {F16 = 1}
  if(dys_exer == 'Not Selected') {F17 = 0} else {F17 = 1}
  if(noc_s == 'Not Selected') {F18 = 0} else {F18 = 1}
  bc <- paste(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18)
  return(bc)
}

FEV_input_labels <- function() {
  c('trig',
    'hema',
    'alb',
    'glob',
    'alk_phos',
    'white_bc',
    'qrs',
    'alcohol',
    'wine',
    'cocktail',
    'height_square',
    'cum_smoke',
    'age',
    'follow_up_baseline',
    'ba_use',
    'dys_exer',
    'noc_s',
    'sex'
  )
}

#####################DEFINE COEFFICIENT NAMES####################################
#define coefficient_names
c1N <- "intercept"
c2N <- "trig_effect"
c3N <- "hema_effect"
c4N <- "alb_effect"
c5N <- "glob_effect"
c6N <- "alk_phos_effect"
c7N <- "white_bc_effect"
c8N <- "qrs_effect"
c9N <- "alcohol_effect"
c10N <- "wine_effect"
c11N <- "cocktail_effect"
c12N <- "height_square_effect"
c13N <- "cum_smoke_effect"
c14N <- "smoke_pack_years_trig_effect"
c15N <- "intercept_rc"
c16N <- "follow_up_baseline_effect"
c17N <- "trig_effect_rc"
c18N <- "hema_effect_rc"
c19N <- "alb_effect_rc"
c20N <- "glob_effect_rc"
c21N <- "alk_phos_effect_rc"
c22N <- "white_bc_effect_rc"
c23N <- "qrs_effect_rc"
c24N <- "alcohol_effect_rc"
c25N <- "wine_effect_rc"
c26N <- "cocktail_effect_rc"
c27N <- "height_square_effect_rc"
c28N <- "cum_smoke_effect_rc"

FEV_coeff_name_vector <- c(c1N,c2N,c3N,c4N,c5N,c6N,c7N,c8N,c9N,c10N, 
                           c11N,c12N,c13N,c14N,c15N,c16N,c17N,c18N,c19N,c20N, 
                           c21N,c22N,c23N,c24N,c25N,c26N,c27N,c28N
                           )
