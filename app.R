# The app

## Libraries
library(png)
library(shiny)
library(jpeg)
library(class)
library(glmnet)
library(shinythemes)
library(rpart)
library(randomForest)
library(styler)
set.seed(1999)

# Load data

load(file = "qmnist_nist.RData")

## Average Image Classifier

# Create average images
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})
# Classifier function
avg_classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)**2)) - 1
}
accuracy_avg <- readRDS("Average_accuracy")

## k-Nearest Neighbors (KNN) Classifier

knn_classifier <- function(new_image, k = 3) {
  train_features <- t(apply(train_nist$px, 1, as.numeric))
  train_labels <- train_nist$digit
  test_features <- as.numeric(new_image)
  knn_predicted_label <- knn(train_features, test_features, train_labels, k = k)
  return(knn_predicted_label)
}
accuracy_knn <- readRDS("Knn_accuracy")

## Decision Tree Classifier

dt_classifier <- readRDS("Decision_Tree_Classifier")
accuracy_dt <- readRDS("Decision_tree_accuracy")

## Random Forest Classifier

rf_classifier <- readRDS("Random_Forest_Classifier")
accuracy_rf <- readRDS("Random_forest_accuracy")

################################################################################

## Define UI
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel(h1("MNIST Digits Classifier", style = "color: #8B475D; font-size: 30px;")),
  tags$div(HTML("<h4 style='color: #8B83864;'>About this app: </h4>
                <p style='color: #8B8386;'> This app allows users to upload images
                from the MNIST dataset and select one of the four classifiers to
                classify the uploaded images. The uploaded image is displayed
                along with the predicted digit and the accuracy of the selected
                classifier. In addition, the app provides an explanation of each
                classifier's methodology to help users understand how the
                classification is performed.</p>")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file",
        label = "Upload an image file",
        accept = c(".png", ".jpg")
      ),
      br(),
      selectInput("classifier", "Select a Classifier",
        choices = c(
          "Average Image Classifier", "k-Nearest Neighbors",
          "Decision Tree", "Random Forest"
        )
      ),
      br(),
      wellPanel(
        h4("Uploaded Image",
          align = "center",
          style = "color: #8B83864;font-weight: bold;"
        ),
        div(
          style = "text-align: center;",
          imageOutput("uploaded_image")
        )
      )
    ),
    mainPanel(
      fluidRow(
        column(
          width = 12,
          wellPanel(uiOutput("prediction")),
          wellPanel(uiOutput("accuracy")),
          wellPanel(uiOutput("explanation"))
        )
      )
    )
  )
)
## Define server
server <- function(input, output, session) {
  # Function to show modal loading dialog
  showModalLoading <- function() {
    showModal(modalDialog(
      title = "Loading...",
      div(
        class = "progress",
        div(
          class = "progress-bar progress-bar-striped active",
          role = "progressbar",
          style = "width: 100%; background-color: #8B475D;"
        ),
        style = "background-color: #FFF8DC;"
      )
    ))
  }

  # Function to hide modal loading dialog
  hideModalLoading <- function() {
    removeModal()
  }

  # Display uploaded image
  output$uploaded_image <- renderImage(
    {
      inFile <- input$file

      if (is.null(inFile)) {
        return(list(src = "", width = "180px"))
      } else {
        # Read image based on file extension
        file_ext <- tools::file_ext(inFile$name)

        if (tolower(file_ext) %in% c("png", "jpg", "jpeg")) { # Check for PNG & JPG
          if (tolower(file_ext) %in% c("png")) {
            img <- readPNG(inFile$datapath)
          } else {
            img <- readJPEG(inFile$datapath)
          }
          list(src = inFile$datapath, width = "180px")
        } else {
          # Unsupported file format - show error modal
          showModal(
            modalDialog(
              title = "Unsupported File Format",
              "Please upload a PNG or JPG image file.",
              easyClose = TRUE,
              footer = NULL
            )
          )
          return(list(src = "", width = "180px"))
        }
      }
    },
    deleteFile = FALSE
  )

  # Classify uploaded image
  output$prediction <- renderUI({
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

      # Show loading modal before performing classification
      showModalLoading()

      # Perform classification based on selected classifier
      if (input$classifier == "Average Image Classifier") {
        prediction <- avg_classifier(vec_with_read_img)
      } else if (input$classifier == "k-Nearest Neighbors") {
        prediction <- knn_classifier(vec_with_read_img)
      } else if (input$classifier == "Decision Tree") {
        prediction <- dt_classifier
      } else if (input$classifier == "Random Forest") {
        prediction <- predict(rf_classifier, t(vec_with_read_img))
      }

      # Hide loading modal after classification is done
      hideModalLoading()
    }

    if (is.null(prediction)) {
      showModal(
        modalDialog(
          title = "Error",
          "Please upload an image file first.",
          easyClose = TRUE,
          footer = NULL
        )
      )
      return(NULL)
    } else {
      wellPanel(
        tags$div(HTML("<b>Predicted Digit:</b>"), prediction),
        style = "padding: 6px;font-size: 24px;"
      )
    }
  })

  # Display accuracy
  output$accuracy <- renderUI({
    req(input$file)
    accuracy <- NULL

    # Accuracy for each classifier
    if (input$classifier == "Average Image Classifier") {
      p(HTML("<b>Accuracy of the model:</b>"), round(accuracy_avg, 4) * 100, "%")
    } else if (input$classifier == "k-Nearest Neighbors") {
      p(HTML("<b>Accuracy of the model:</b>"), round(accuracy_knn, 4) * 100, "%")
    } else if (input$classifier == "Decision Tree") {
      p(HTML("<b>Accuracy of the model:</b>"), round(accuracy_dt, 4) * 100, "%")
    } else if (input$classifier == "Random Forest") {
      p(HTML("<b>Accuracy of the model:</b>"), round(accuracy_rf, 4) * 100, "%")
    }
  })

  # Display explanation
  output$explanation <- renderUI({
    req(input$file)
    explanation <- NULL

    if (input$classifier == "Average Image Classifier") {
      tags$div(
        tags$h4("Classifier Methodology",
          style = "color: #8B83864;font-weight: bold;"
        ),
        p("The Average Image Classifier calculates the average pixel values
          for each digit (0 to 9) in the train subset. Then computes the Euclidean
          Distance between the average pixel values of the uploaded image and the
          average pixel values calculated before. It finds the index of the minimum
          euclidean distance and returns the corresponding digit as the
          classification result.")
      )
    } else if (input$classifier == "k-Nearest Neighbors") {
      tags$div(
        tags$h4("Classifier Methodology",
          style = "color: #8B83864;font-weight: bold;"
        ),
        p("The k-Nearest Neighbors (KNN) Classifier compares the uploaded images
            with the training images, and predicts the label (digit) based on the
            labels of the k-nearest neighbors in the training data. In this case,
            we are using 3 nearest negihbors to simplify the process.")
      )
    } else if (input$classifier == "Decision Tree") {
      tags$div(
        tags$h4("Classifier Methodology",
          style = "color: #8B83864;font-weight: bold;"
        ),
        p("The Decision Tree Classifier learns is capable of capturing descriptive
            decision-making knowledge from the uploaded images. Decision tree was
            generated from training sets. Then uses these descriptive knowledge to
            make predictions about whay digit is the uploaded image.")
      )
    } else if (input$classifier == "Random Forest") {
      tags$div(
        tags$h4("Classifier Methodology",
          style = "color: #8B83864;font-weight: bold;"
        ),
        p("The Random Forest Classifier computes multiple decision trees using
            different random subsets of the data and features. Each decision tree
            provides like a opinion on how to classify the data. When making
            predictions for the image uploaded to the system, the classifier
            collects the predictions from all the decision trees and combines
            them. It does this by calculating the prediction made by each tree for
            the uploaded image and then selecting the most common prediction as
            the final result.")
      )
    }
  })
}
## Run the application
shinyApp(ui = ui, server = server)
