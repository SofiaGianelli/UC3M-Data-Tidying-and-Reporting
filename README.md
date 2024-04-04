# UC3M-Data-Tidying-and-Reporting

This project was made for the "Data Tidying and Reporting" course in the Statistics for Data Science Master at Universidad Carlos III de Madrid. 

This is what was requested in each task:
## Task 1
Download and import the dataset qmnist_nist.RData. It contains the data frames train_nist and
test_nist, both with variables digit (a vector with digit labels), writer (a vector with the writer’s ID),
and px (a matrix with 28 × 28 = 784 columns of pixel gray levels). The digit images can be visualized with
the following function:

Do the following:
1. Transform train_nist to do a ridge logistic model (refresher here) for classifying the digits 4 and 9.
2. Fit ridge a model, with a cross-validated-chosen λ penalty. Do not standardize the predictors (the
default; why not?).
3. Plot the estimated β in a way that delivers insights about the classification.
4. Using the test_nist dataset, evaluate the prediction accuracy of the model.
5. [Optional] Tackle the 45 classification problems of one versus another digit. Report in a 10 × 10 matrix
the classification accuracy on each problem. Visualize the estimated β’s in a way similar to Point 3.

## Task 2
Create a Shiny app that allows the user to upload an image from the MNIST dataset (available in
qmnist_nist.RData). In the assessment phase, the app will be evaluated with new images that were
not used in the training. These images will be selected by the professor after the app was delivered. The
code below generates some images (test-0.png, etc.) for which the app is supposed to work, as well as helps
start designing the server side of the app.
