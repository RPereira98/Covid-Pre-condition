# Covid-Pre-condition
Capstone Project using  COVID-19 patient pre-condition dataset
This algorithm use data to predict if a covid positive patient will need hospitalization or not based in his/her pre-conditions. 
The data used for this is COVID-19 patient pre-condition dataset (https://www.kaggle.com/tanmoyx/covid19-patient-precondition-dataset) acquired from the Mexican 
government. The variable that will be predicted is the type of patient (variable's name: patient_type), 1 for outpatient (a patient who receives medical treatment 
without being admitted to a hospital) and 2 for inpatient (a patient who's been admitted to hospital for medical treatment).The pre-conditions (predictors) used for
this are:sex of the patient (1 for female and 2 for male, variable's name: sex),the age of the patient (variable's name: age). In the next variables 1 indicates that
the patient has it and 2 that the patient doesn't have it: pneumonia (variable's name: pneumonia), diabetes(variable's name: diabetes),chronic obstructive pulmonary 
disease (variable's name: copd),asthma (variable's name: asthma),immunosuppression (variable's name: inmsupr),hypertension (variable's name: hypertension),other diseases
(variable's name: other_disease), cardiovascular diseases(variable's name:cardiovascular), obesity (variable's name: obesity),chronic kidney disease (variable's name:
renal_chronic) and smoking habits (variable's name: tobacco). Only the covid positive patients will be used for the models(variable's  name: covid_res)
 For the algorithm are used seven models: linear discriminant analysis (LDA), generalized linear model (LGM), quadratic discriminant analysis (QDA), classification,
 regression tree (RPART),Boosted classification tree, Conditional interference tree (CTREE) and three ensembles of the other six models.
