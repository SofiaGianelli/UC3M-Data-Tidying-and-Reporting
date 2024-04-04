# Required library

library(png)
library(shiny)
library(jpeg)
library(class)
library(glmnet)
library(shinythemes)
library(rpart)
library(randomForest)
set.seed(1999)

# Load data

load(file = "qmnist_nist.RData")

# Visualization helper

show_digit <- function(x, col = gray(255:1 / 255), ...) {
  l <- sqrt(length(x))
  image(matrix(as.numeric(x), nrow = l)[, l:1], col = col, ...)
}

# Create test images to upload to the app

for (i in 0:9) {
  # Matrix with 0-1 entries
  img_vec <- test_nist$px[which(test_nist$digit == i)[1], ] / 255
  img_mat <- matrix(as.numeric(img_vec), nrow = 28, ncol = 28,
                    byrow = TRUE) # Saves it with the right orientation
  
  # Save image
  writePNG(image = img_mat, target = paste0("test-", i, ".png"))
}

## Check that the reading is fine

# Read image

test_d <- 7 # Change me
test_img <- readPNG(paste0("test-", test_d, ".png"))

# Vectors with pixel values

vec_with_original_img <- test_nist$px[which(test_nist$digit == test_d)[1], ]
vec_with_read_img <- c(255 * t(test_img)) # Use scale 0-255 and flatten the
# rotated image so that the layout is comparable to $px
max(abs(vec_with_original_img - vec_with_read_img))
par(mfrow = c(1, 1))
show_digit(vec_with_original_img, axes = FALSE)
show_digit(vec_with_read_img, axes = FALSE)

## Define classifiers

# Average Image Classifier
# Create average images
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})
# Classifier function
avg_classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)**2)) - 1
}
# Visualize average train images
par(mfrow = c(2, 5), mar = rep(0, 4))
for (d in 1:10) {
  show_digit(avg_train_images[, d], axes = FALSE)
}

# k-Nearest Neighbors (KNN) Classifier

knn_classifier <- function(new_image, k = 3) {
  train_features <- t(apply(train_nist$px, 1, as.numeric))
  train_labels <- train_nist$digit
  
  test_features <- as.numeric(new_image)
  
  knn_predicted_label <- knn(train_features, test_features, train_labels, k = k)
  return(knn_predicted_label)
}

# Decision Tree Classifier

dt_classifier <- function(train_data, train_labels, test_data) {
  dt_model <- rpart(train_labels ~ ., data = train_data, method = "class")
  prediction <- predict(model, newdata = test_data, type = "class")
  return(prediction)
}

# Random Forest Classifier

rf_classifier <- function(train_data, train_labels, x_test_data, y_test_data,test_data, num_trees) {
  rf_model <- randomForest(x= train_data, y=train_labels, x_test = x_test_data, y_test = y_test_data, ntree = num_trees)
  prediction <- predict(model, newdata = test_data)
  return(prediction)
}

# Define UI

ui <- fluidPage(theme = shinytheme("superhero"),
                tags$div(HTML("<h1>About this app: </h1>
                <p>The aim of this app is to classify an image that you uploaded. </p>
                <p>The first step is to upload an .png or img image with a digit. 
                Then, you have to select which classifier use to obtain the predicted digit. 
                Finally, you will obtain the result of this prediction and the accuracy of the model used.<p>")),
                titlePanel("MNIST Digits Classifier"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file", label="Upload an image file", accept = c(".png", ".jpg")),
                    br(),
                    selectInput("classifier", "Select a Classifier",
                                choices = c("Average Image Classifier", "k-Nearest Neighbors", "Decision Tree", "Random Forest"))
                  ),
                  mainPanel(
                    imageOutput("uploaded_image"),
                    br(),
                    textOutput("prediction")
                  )
                )
)

# Define server logic

server <- function(input, output) {
  
  # Display uploaded image
  output$uploaded_image <- renderImage({
    inFile <- input$file
    
    if (is.null(inFile)) {
      return(list(src = "", width = "200px")) 
    } else {
      # Read image based on file extension
      file_ext <- tools::file_ext(inFile$name)
      
      if (tolower(file_ext) %in% c("png", "jpg", "jpeg")) {  # Check for PNG, JPG, and JPEG
        if (tolower(file_ext) %in% c("png")) {
          img <- readPNG(inFile$datapath)
        } else {
          img <- readJPEG(inFile$datapath)
        }
        list(src = inFile$datapath, width = "200px")
      } else {
        # Unsupported file format
        return(list(src = "", width = "200px"))
      }
    }
  }, deleteFile = FALSE)  
  
  # Classify uploaded image
  output$prediction <- renderText({
    req(input$file)
    prediction <- NULL
    
    # Extract file extension
    file_ext <- tools::file_ext(input$file$name)
    
    # Check if the file extension is supported
    if (tolower(file_ext) %in% c("png", "jpg", "jpeg")) {  
      if (tolower(file_ext) %in% c("png")) {  
        test_img <- readPNG(input$file$datapath)
      } else {
        test_img <- readJPEG(input$file$datapath)
      }
      
      vec_with_read_img <- c(255 * t(test_img))
      
      # Perform classification based on selected classifier
      if (input$classifier == "Average Image Classifier") {
        explanation <- "The Average Image Classifier calculates the average pixel values for each digit (0 to 9) 
        in the train subset. Then computes the Euclidean Distance between the average pixel values of 
        the uploaded image and the average pixel values calculated before. It finds the index of the minimum 
        euclidean distance and returns the corresponding digit as the classification result."
        prediction <- avg_classifier(vec_with_read_img)
      } else if (input$classifier == "k-Nearest Neighbors") {
        explanation <- "The k-Nearest Neighbors (KNN) Classifier predicts the digit of the input
        image by finding the 'k' closest images in the training dataset and selecting the most 
        common digit among them."
        prediction <- knn_classifier(vec_with_read_img)
      } else if (input$classifier == "Decision Tree") {
        explanation <- "The Decision Tree Classifier builds a tree-like structure based on features
        of the training data. It makes decisions by traversing this tree based on the features of 
        the input image."
        dataframe <- as.data.frame(t(vec_with_read_img))
        names(dataframe) <- paste(1:ncol(dataframe))
        prediction <- dt_classifier(train_nist$px, train_nist$digit, dataframe)
      } else if (input$classifier == "Random Forest") {
        explanation <- "The Random Forest Classifier creates an ensemble of decision trees and
        aggregates their predictions to classify the input image. It improves performance by reducing
        overfitting and increasing robustness."
        dataframe <- as.data.frame(t(vec_with_read_img))
        names(dataframe) <- paste(1:ncol(dataframe))
        prediction <- rf_classifier(as.matrix(train_nist$px), as.factor(train_nist$digit), as.matrix(test_nist$px), as.factor(test_nist$digit), dataframe, 15)
      }
    }
    paste("Predicted Digit:", prediction)
  })
}


# Run the application

shinyApp(ui = ui, server = server)