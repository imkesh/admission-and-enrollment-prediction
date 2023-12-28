# admission-and-enrollment-prediction
This project is centered on optimizing the college admissions process using data-driven techniques. Our objective was to assist higher education institutions in making more efficient and informed decisions
regarding the admission and enrollment of prospective students.


#Detail model development and output
Logistic Regression Model
I first developed a model for admission prediction based on exploratory data analysis, using all variables 
available in the dataset. Based on the summary of the model it is decided that the best variables to consider 
and the variables that are insignificant like White, Asian, and sex to be eliminated. 
I have developed another model with the selected variables and validated the model with a randomly split 
dataset into a training set and validation set. For logistic regression predicting admitted decision, the model 
4 is best with an accuracy of 81.75%, and a sensitivity of 89.04%. The equation of the model is AdmittedNumeric ~ 
Edu_Parent1 + Edu_Parent2 + HSGPA + SAT_ACT. The data is split into a 70-30 ratio for training and 
validation. For logistic regression predicting enrolled decisions, model 4 gives an accuracy of 80.90%, 
sensitivity of 20.27%, and specificity of 94.69%, performing the best in comparison to other models. The 
equation of the model is EnrolledNumeric ~ Edu_Parent1 + Edu_Parent2 +White+ HSGPA + 
SAT_ACT+College. The cutoff for classifying predicted probabilities in logistic regression is 0.5. I decided 
on this value after testing the model with cutoff values in the range [0.3,0.8]. I got the best accuracy at 0.5.
Classification Tree Model
Similarly, I have two classification models for Admitted and Enrolled Students. Details of both models 
are:
CART for Admitted 
I have run a Classification Model for the response variable Admitted and predictor variables are Education 
of Parent 1, Education of Parent 2, Sex of student, Ethnicity of student, High School GPA of student, SAT 
Score and College. I have initially created a Full Classification tree, but working on a Full tree not only 
creates the issue of overfitting, it also requires more resources and scalability. Therefore, I have worked 
7 on pruned trees. That eliminated all the variables that were insignificant to our analyses. The root node is 
High School GPA, the Internal node is SAT_ACT and branches are education of parents and ethnicity. 
In the second step, I ran a Confusion Matrix to look at the Accuracy, Sensitivity, Specificity and 
Prevalence of our model to analyze how accurate our model is regarding the predictability of validation 
data. At the default cutoff value of 0.5, the accuracy of the model is 81.46% and the Sensitivity is 59.09% whereas, 
with the new cut-off value of 0.307 I have increased the sensitivity to 77.13% but the accuracy of the 
model has decreased to 76.85%. I will still go with our model with a cutoff value of 0.307 as it 
corresponds to our data more than the first model. 
CART for Enrolled
I have also implemented a classification model for the response variable "Enrolled” using the predictor 
variables that represent the education level of parent 1, education level of parent 2, sex, white or not, Asian,
or not, high school GPA, SAT/ACT scores and College. Note, that here I have filtered the dataset to 
include only those applicants who have gotten an admit (Admitted = “Yes”). Since the number of 
applicants who had enrolled was far less than the number of applicants who did not enroll, I 
oversampled the dataset so that the overall class balance is achieved to represent both cases and avoid 
bias. I created a full classification tree using this and pruned it to suit our needs. The pruned tree was 
used to model the data. I ran a confusion matrix with a default cutoff value of 0.50 on the validation 
set, the classification tree had an accuracy of 66.75%, sensitivity of 64.18%, and specificity of 69.32%.
