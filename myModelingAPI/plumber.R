#myAPI.R 
library(GGally)
library(leaflet)

#Send a message
#* @get /info
function(){
  list(message='Powell Braddock ', 'https://pebraddo.github.io/final-project/')
}

#http://127.0.0.1:11158/info

# Choose a predictor
#* @param pred
#* @get /pred
function(predictor) {
  data <- iris
  if (is.numeric(data[[predictor]])) {
    value <- mean(data[[predictor]])
    message <- paste("The mean of", predictor, "is", value)
    return(message)
  } else if (predictor == "Species") {
    table <- table(data[[predictor]])
    return(paste0(names(table), ": ", table))
  } else {
    stop("Invalid predictor.")
  }
}



#* Find natural log of a number
#* @param num Number to find ln of
#* @get /ln
function(num){
  log(as.numeric(num))
}

#query with http://localhost:PORT/ln?num=1


#* Plot of confusion matrix
#* @serializer png
#* @param type base or ggally
#* @param color TRUE or FALSE (only for ggally)
#* @get /plotconfusion
function(type = "base", color = FALSE){
  if(tolower(type) == "ggally"){
    if(color){
      a <- GGally::ggpairs(iris, aes(color = Species))
      print(a)
    } else {
      a <- GGally::ggpairs(iris)
      print(a)
    }
  } else {
    pairs(iris)
  }
}
#http://localhost:PORT/plotiris?type=ggally


#* Plotting widget for confusion matrix
#* @serializer htmlwidget
#* @param lat latitude
#* @param lng longitude
#* @get /map
function(lng = 174.768, lat = -36.852){
  m <- leaflet::leaflet() |>
    addTiles() |>  # Add default OpenStreetMap map tiles
    addMarkers(as.numeric(lng), as.numeric(lat))
  m  # Print the map
}

#query with http://localhost:PORT/map?lng=174&lat=-36


