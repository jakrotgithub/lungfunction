Table of content for Help Tab
Objective of Tool
Layout of Tool
I.	Sidebar panel for patient’s inputs
II.	Plot tab
III.	Help tab
IV.	Disclaimer Tab
Web Application – features, abbreviations and terms
Example which demonstrates how the application works
Creation of Tool
Last update

Objective of Tool
Chronic Obstructive Pulmonary Disease (COPD) is characterized by loss of breath, and it encompasses many different progressive lung diseases, such as refractory asthma, chronic bronchitis and emphysema, which over time lead to chronic airflow obstruction. The Individualized Prediction of Adulthood Lung Function Decline web application is a risk prediction tool that is developed to forecast lung function decline in adults over the long term. It is a tool that clinicians can use to help them visually understand the predicted lung function decline in individual patients, based on a certain set of known characteristics for a specific patient.
Layout of Tool
Add screen shot of the application
I. Sidebar panel for patient’s inputs:
Based on the available characteristics of the patient, the clinician is able to enter up to 17 inputs into the sidebar panel, located on the left side of the web application.
The measurements are labeled in parentheses after each of the inputs in the sidebar panel. However, the information regarding the individual input names, input’s measurements that need to be used in the application and minimum & maximum values possible for each specific input have all been combined for your convenience in the table, below. 
Table 1: A table of the 17 inputs and their descriptions – measurements and bounds
Input name	Measurement	Bounds on values
		Minimum Value	Maximum Value
Age	year	0	100
Triglycerides	mg/dl	0	1000
Hematocrit	%		
Albumin	mg/L		
Globulin	g/L		
Alkaline Phosphotase	units (?)		
White blood cells	109/L		
QRS interval	hundredth of second (or 0.01 sec)		
Alcohol index	ozs/wk		
Wine intake	glasses/week		
Cocktail intake	drinks/week		
Height	cm		
Cum. smoke years	daily cigarettes		
Sex	n/a		
Bronchodilator or aerosol	n/a		
Dyspnea on exertion	n/a		
Nocturnal symptoms	n/a		

II. Plot tab:
Under the Plot tab, after entering all of the known inputs in the sidebar panel, the clinician can click on the ‘Plot FEV1 decline’ button. The ‘Plot FEV1 decline’ button is the last button at the bottom of the sidebar panel. The tool will produce a graph with a linear regression and confidence intervals (CIs). The linear regression is generated using mixed-model linear regression. It predicts the adulthood lung function decline over long term. 
The user can hover his/her computer mouse over the section of graph which is of particular interest. Once the clinician places the cursor over the linear regression, a popup window will be generated, providing specific information on that particular point (i.e. value on x-axis, value on y-axis, upper CI and lower CI). In this simulation, the x-axis is ‘Time (years)’ and the y-axis is ‘FEV1 (L)’.
III. Help Tab:
Currently, you are viewing the information that is located under the Help Tab.
IV. Disclaimer Tab:
The Disclaimer Tab contains information about the terms and conditions under which the user is bound when utilizing the Individualized Prediction of Adulthood Lung Function Decline web application and copyright terms.
Web Application – features, abbreviations and terms
Forced expiratory volume (FEV1): Measured in milliliters, FEV1 is the quantity of air that an individual can exhale out in one second.
Forced vital capacity (FVC): Measured in liters, FVC is the total volume of air that an individual can exhale out of his/her lungs after taking a deep inhale.

Example which demonstrates how the application works
This is an example with screenshots which demonstrates step-by-step to the user how she/he can run the simulation. The clinician may use the following chart to fill in as may inputs as possible for the specific patient.
A non-COPD patient with a certain set of characteristics visits his physician for a yearly check-up. Assuming that we do not know all of the characteristics of the patient, the clinician has the following information after a blood test and some previous prescriptions:
Age: 50 years-old
Triglycerides: 120 mg/dl
Hematocrit: 40%
Albumin: 4 g/dl = 40,000 mg/L
Globulin: 7 g/L
White blood cells: 7 × 109/L
Height: 190 cm
Sex: male
Usage of bronchodilator or aerosol: Former use

The clinician decides to use the Individualized Prediction of Adulthood Lung Function Decline web application to run a simulation that predicts how the patient’s lung function will decline over the next 20 years. 
Step 1. Clinician enters known values into the sidebar panel and leaves the unknown blank. (screen shot with inputs)
Step 2. Clinician clicks on the ‘Plot FEV1 decline’ button, located at the very bottom of the sidebar panel.
Step 3. The clinician should see a progress bar appear at the bottom, right hand corner of the web application. The progress bar should look something like this: 
(Please note: The message in the progress bar will change as the program accomplishes certain steps of the data analysis. We kindly ask for your patience and apologize for the waiting time.)
Step 4. Once the program has finishes the computations, the clinician will see a plot under the Plot tab.
Step 5. The tool will produce a graph with a linear regression and confidence intervals (CIs). The linear regression is generated using mixed-model linear regression. It predicts the adulthood lung function decline over long term. 
Step 6. The user can hover his/her computer mouse over the section of graph which is of particular interest. Once the clinician places the cursor over the linear regression, a popup window will be generated, providing specific information on that particular point (i.e. value on x-axis, value on y-axis, upper CI and lower CI). In this simulation, the x-axis is ‘Time (years)’ and the y-axis is ‘FEV1 (L)’.

Creation of Tool
This web application was developed based on the manuscript - “Individualized prediction of adulthood lung function decline: a Framingham offspring cohort analysis.” The tool was developed using Shiny R.
Last Update of web application
December 2017

