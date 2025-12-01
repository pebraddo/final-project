#myModelingAPI.R 


#load in data and best model from Modeling.qmd
modeling_data <- diabetes_convert
final_rf_model <- extract_workflow(rf_final_fit) |>
  fit(data= modeling_data)

#Send a message
#* @get /info
function(){
  list(message='Powell Braddock ', 'https://pebraddo.github.io/final-project/')
}

#http://127.0.0.1:11158/info


#create default values
get_default <- function(input) { 
  defaults <- map(input, function(col) {
    if (is.numeric(col)) {
      mean(col, na.rm = TRUE)
    } else {
      names(sort(table(col), decreasing = TRUE))[1]
    }
  })
  return(defaults)
}
DEFAULTS<-get_default(modeling_data |>
                        select(-Diabetes_binary))



# Predict Diabetes Status
#* @param BMI
#* @param Age
#* @param GenHlth
#* @param PhysHlth
#* @param MentHlth
#* @param HighBP
#* @param HighChol
#* @param CholCheck
#* @param Smoker
#* @param Stroke
#* @param HeartDiseaseorAttack
#* @param PhysActivity
#* @param Fruits
#* @param Veggies
#* @param HvyAlcoholConsump
#* @param AnyHealthcare
#* @param NoDocbcCost
#* @param DiffWalk
#* @param Sex
#* @param Education
#* @param Income
#* @get /pred
function(BMI = DEFAULTS$BMI,
         Age = DEFAULTS$Age,
         GenHlth = DEFAULTS$GenHlth,
         PhysHlth = DEFAULTS$PhysHlth,
         MentHlth = DEFAULTS$MentHlth,
         HighBP = DEFAULTS$HighBP,
         HighChol = DEFAULTS$HighChol,
         CholCheck = DEFAULTS$CholCheck,
         Smoker = DEFAULTS$Smoker,
         Stroke = DEFAULTS$Stroke,
         HeartDiseaseorAttack = DEFAULTS$HeartDiseaseorAttack,
         PhysActivity = DEFAULTS$PhysActivity,
         Fruits = DEFAULTS$Fruits,
         Veggies = DEFAULTS$Veggies,
         HvyAlcoholConsump = DEFAULTS$HvyAlcoholConsump,
         AnyHealthcare = DEFAULTS$AnyHealthcare,
         NoDocbcCost = DEFAULTS$NoDocbcCost,
         DiffWalk = DEFAULTS$DiffWalk,
         Sex = DEFAULTS$Sex,
         Education = DEFAULTS$Education,
         Income = DEFAULTS$Income
         ) {
tryCatch({
  new_data <- tibble(
    BMI = as.numeric(BMI),
    Age = as.factor(Age),
    GenHlth = as.factor(GenHlth),
    PhysHlth = as.factor(PhysHlth),
    MentHlth = as.factor(MentHlth),
    HighBP = as.factor(HighBP),
    HighChol = as.factor(HighChol),
    CholCheck = as.factor(CholCheck),
    Smoker = as.factor(Smoker),
    Stroke = as.factor(Stroke),
    HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
    PhysActivity = as.factor(PhysActivity),
    Fruits = as.factor(Fruits),
    Veggies = as.factor(Veggies),
    HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
    AnyHealthcare = as.factor(AnyHealthcare),
    NoDocbcCost = as.factor(NoDocbcCost),
    DiffWalk = as.factor(DiffWalk),
    Sex = as.factor(Sex),
    Education = factor(Education),
    Income = factor(Income)
  )
  pred <-predict(final_rf_model, new_data, type = 'prob')
  as.data.frame(pred)

}, error = function(e) {
  list(error = e$message)
})
}

#Example Calls to Copy and Paste
#http://127.0.0.1:39005/pred?BMI=25&HvyAlcoholConsump=No%20Heavy%20Alcohol%20Consumption&AnyHealthcare=Has%20Healthcare&Sex=Female
#http://127.0.0.1:39005/pred?BMI=25&Sex=Female
#http://127.0.0.1:39005/pred?BMI=42&Sex=Male

#* Plot of confusion matrix
#* @serializer png
#* @get /plotconfusion
function(){
  preds <- predict(final_rf_model, new_data = modeling_data) |> bind_cols(modeling_data)

  preds$.pred_class <- as.factor(preds$.pred_class)
  preds$Diabetes_binary <- as.factor(preds$Diabetes_binary)
  
  cm <- conf_mat(
    data = preds,
    truth = Diabetes_binary,
    estimate = .pred_class
  )
  
  print(autoplot(cm, type = 'heatmap'))
  }
#http://localhost:PORT/plotiris?type=ggally

