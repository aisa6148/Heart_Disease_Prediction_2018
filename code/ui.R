library(shiny)
library(shinythemes)
ui = fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Heart Disease Prediction System"),
  sidebarLayout (
    sidebarPanel (
      
      textInput("age", "Age"),
      
      selectInput("sex", "Gender",
                  c("",  "Male", "Female")),
      selectInput("cp", "Chest Pain Type",
                  c(" ", "Typical angina", "Atypical angina", "Non-anginal pain", "Asymtomatic")),
      textInput("trestbps", "Trest Blood Pressure"),
      textInput("chol", "Cholestrol"),
      textInput("fbs", "Fasting blood sugar"),
      selectInput("restecg", "Rest ECG",
                  c("",  "Normal", "Abnormality", "Ventricular hypertrosphy")),
      textInput("thalach", "Thalach"),
      selectInput("exang", "Exercise Angina",
                  c("",  "0", "1")),
      textInput("oldpeak", "Old peak"),
      selectInput("slope", "Slope",
                  c("",  "Upsloping", "Flat", "Downsloping")),
      sliderInput("ca", "CA", 0, 4, 2, ticks = FALSE),
      textInput("thal", "Thal"),
      actionButton("submit", "Data Overview", class = "btn-primary"),
      actionButton("submit1", "Random Forest", class = "btn-primary"),
      actionButton("submit2", "Logistic Regression", class = "btn-primary"),
      actionButton("submit3", "Support Vector Machine", class = "btn-primary"),
      actionButton("submit4", "K-Nearest Neighbors", class = "btn-primary"),
      actionButton("submit5", "Naive Bayes", class = "btn-primary"),
      actionButton("submit6", "learn", class = "btn-primary")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type="tab",
                  tabPanel("Result and Graphs",
                           plotOutput("plotx"), plotOutput("ploty"),
                           verbatimTextOutput("text1"),plotOutput("plot1"),
                           verbatimTextOutput("text2"),plotOutput("plot2"), 
                           verbatimTextOutput("text3"),plotOutput("plot3"),
                           verbatimTextOutput("text4"),plotOutput("plot4"),
                           verbatimTextOutput("text5"),plotOutput("plot5"),
                           verbatimTextOutput("text6")
                  ), tabPanel("Confusion Matrix", 
                              paste("RANDOM FOREST:"),
                                        tableOutput(outputId= "conf1"),
                              paste("LOGISTIC REGRESSION:"),
                                        tableOutput(outputId= "conf2"),
                              paste("SUPPORT VECTOR MACHINE:"),
                                        tableOutput(outputId= "conf3"),
                              paste("KNN:"),
                                        tableOutput(outputId= "conf4"),
                              paste("NAIVE BAYES: "),
                                        tableOutput(outputId= "conf5")))
    ))
)