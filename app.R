#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(plotly)
library(forecast)
library(ggplot2)
library(stargazer)
library(vars)
library(tseries)
library(forecast)
library(readr)
library (dplyr)
library(stringr)
library(DT)

fypdata <- read.csv("NEW FYP DATA.csv", check.names=F)
getwd()


#ADF TEST
#NON-DIFFERENCED 
adf_unemp<-adf.test(ts(fypdata$`UNEMPLOYMENT RATE`))
adf_gdp<-adf.test(ts(fypdata$`ln(GDP PER CAPITA)`))

adf_results <- data.frame(
  Variable = c("Unemployment Rate", "Gross Domestic Product (GDP)"),
  Hypothesis = c(
    "H₀: The unemployment rate is not stationary<br>H₁: The unemployment rate is stationary",
    "H₀: The GDP is not stationary<br>H₁: The GDP is stationary"
  ),
  `Test Statistic` = c(round(adf_unemp$statistic, 4), round(adf_gdp$statistic, 4)),
  `p-value` = c(round(adf_unemp$p.value, 4), round(adf_gdp$p.value, 4)),
  Conclusion = c(
    ifelse(adf_unemp$p.value < 0.05, "Reject H₀.<br>The unemployment rate is stationary.", 
           "Fail to reject H₀.<br>The unemployment rate is not stationary."),
    ifelse(adf_gdp$p.value < 0.05, "Reject H₀.<br>The GDP is stationary.", 
           "Fail to reject H₀.<br>The GDP is not stationary.")
  )
)

#DIFFERENCED
adf_diffunemp<-adf.test(diff(ts(fypdata$`UNEMPLOYMENT RATE`)))

adf_diffgdp<-adf.test(diff(ts(fypdata$`ln(GDP PER CAPITA)`)))

adf_results2 <- data.frame(
  Variable = c("Unemployment Rate", "Gross Domestic Product (GDP)"),
  Hypothesis = c(
    "H₀: The unemployment rate is not stationary<br>H₁: The unemployment rate is stationary",
    "H₀: The GDP is not stationary<br>H₁: The GDP is stationary"
  ),
  `Test Statistic` = c(round(adf_diffunemp$statistic, 4), round(adf_diffgdp$statistic, 4)),
  `p-value` = c(round(adf_diffunemp$p.value, 4), round(adf_diffgdp$p.value, 4)),
  Conclusion = c(
    ifelse(adf_diffunemp$p.value < 0.05, "Reject H₀.<br>The unemployment rate is stationary.", 
           "Fail to reject H₀.<br>The unemployment rate is not stationary."),
    ifelse(adf_diffgdp$p.value < 0.05, "Reject H₀.<br>The GDP is stationary.", 
           "Fail to reject H₀.<br>The GDP is not stationary.")
  )
)

#SELECTION NUMBER OF LAG(s)
data_diff<-na.omit(data.frame(diff(ts(fypdata$`UNEMPLOYMENT RATE`)), diff(ts(fypdata$`ln(GDP PER CAPITA)`))))  #na.omit to remove missing values
var_lag<-VARselect(data_diff, lag.max = 10, type="const")


fypdata2<-na.omit(data.frame(
  unemp = ts(fypdata$`UNEMPLOYMENT RATE`), 
  gdp = ts(fypdata$`ln(GDP PER CAPITA)`)
))

var_lag2<-VARselect(fypdata2, lag.max = 10, type="const")


lag_table <- data.frame(
  Data = c("Stationary Data", "Original Data"),
  AIC = c(var_lag$selection["AIC(n)"], var_lag2$selection["AIC(n)"]),
  HQ = c(var_lag$selection["HQ(n)"], var_lag2$selection["HQ(n)"]),
  SC = c(var_lag$selection["SC(n)"], var_lag2$selection["SC(n)"]),
  FPE = c(var_lag$selection["FPE(n)"], var_lag2$selection["FPE(n)"])
)

#MODEL ESTIMATION
#STATIONARY DATA
#MODEL ESTIMATION UR
unemp_diff<- diff(ts(fypdata$`UNEMPLOYMENT RATE`))
gdp_diff<-diff(ts(fypdata$`ln(GDP PER CAPITA)`))

data_diff<- na.omit(data.frame(unemp_diff, gdp_diff))

var_model<-VAR(data_diff, p=1, type="const")
var_summary <- summary(var_model)
summary_unemp <- var_summary$varresult$unemp_diff
summary_gdp <- var_summary$varresult$gdp_diff

coef_table_unemp <- as.data.frame(coef(summary_unemp))
rownames(coef_table_unemp) <- c("Unemployment Rate", "GDP", "Constant")

coef_table_unemp <- coef_table_unemp %>%
  tibble::rownames_to_column(var = "Variable")  #Store row names as a column

# Round values properly
coef_table_unemp1 <- coef_table_unemp %>%
  dplyr::mutate(
    Estimate = round(Estimate, 4),
    `Standard Error` = round(`Std. Error`, 4),
    `t Value` = round(`t value`, 4),
    `p-value` = round(`Pr(>|t|)`, 4)
  ) %>%
  dplyr::select(Variable, Estimate, `Standard Error`, `t Value`, `p-value`) 

#MODEL ESTIMATION GDP
coef_table_gdp <- as.data.frame(coef(summary_gdp))
rownames(coef_table_gdp) <- c("Unemployment Rate", "GDP", "Constant")

coef_table_gdp <- coef_table_gdp %>%
  tibble::rownames_to_column(var = "Variable")  #Store row names as a column

# Round values properly
coef_table_gdp1 <- coef_table_gdp %>%
  dplyr::mutate(
    Estimate = round(Estimate, 4),
    `Standard Error` = round(`Std. Error`, 4),
    `t Value` = round(`t value`, 4),
    `p-value` = round(`Pr(>|t|)`, 4)
  ) %>%
  dplyr::select(Variable, Estimate, `Standard Error`, `t Value`, `p-value`)

#SUMMARY STATIONARY DATA
r_squared <- c(summary_unemp$r.squared, summary_gdp$r.squared)
adj_r_squared <- c(summary_unemp$adj.r.squared, summary_gdp$adj.r.squared)
f_statistics <- c(summary_unemp$fstatistic[1], summary_gdp$fstatistic[1])
p_values <- c(
  pf(summary_unemp$fstatistic[1], summary_unemp$fstatistic[2], summary_unemp$fstatistic[3], lower.tail = FALSE),
  pf(summary_gdp$fstatistic[1], summary_gdp$fstatistic[2], summary_gdp$fstatistic[3], lower.tail = FALSE)
)

model_summary <- data.frame(
  Variable = c("Unemployment Rate", "GDP"),
  Multiple_R_Squared = round(r_squared, 4),
  Adjusted_R_Squared = round(adj_r_squared, 4),
  F_Statistic = round(f_statistics, 4),
  p_value = round(p_values, 4)
)

#ORIGINAL DATA
#MODEL ESTIMATION
fypdata2<-na.omit(data.frame(
  unemp = ts(fypdata$`UNEMPLOYMENT RATE`), 
  gdp = ts(fypdata$`ln(GDP PER CAPITA)`)
))
var_model2<-VAR(fypdata2, p=1, type="const")

#MODEL ESTIMATION UR
var_summary2 <- summary(var_model2)
orisummary_unemp <- var_summary2$varresult$unemp
orisummary_gdp <- var_summary2$varresult$gdp

ori_table_unemp <- as.data.frame(coef(orisummary_unemp))
rownames(ori_table_unemp) <- c("Unemployment Rate", "GDP", "Constant")

ori_table_unemp <- ori_table_unemp %>%
  tibble::rownames_to_column(var = "Variable")  #Store row names as a column

# Round values properly
ori_table_unemp1 <- ori_table_unemp %>%
  dplyr::mutate(
    Estimate = round(Estimate, 4),
    `Standard Error` = round(`Std. Error`, 4),
    `t Value` = round(`t value`, 4),
    `p-value` = round(`Pr(>|t|)`, 4)
  ) %>%
  dplyr::select(Variable, Estimate, `Standard Error`, `t Value`, `p-value`)


#MODEL ESTIMATION GDP
ori_table_gdp <- as.data.frame(coef(orisummary_gdp))
rownames(ori_table_gdp) <- c("Unemployment Rate", "GDP", "Constant")

ori_table_gdp <- ori_table_gdp %>%
  tibble::rownames_to_column(var = "Variable")  #Store row names as a column

# Round values properly
ori_table_gdp1 <- ori_table_gdp %>%
  dplyr::mutate(
    Estimate = round(Estimate, 4),
    `Standard Error` = round(`Std. Error`, 4),
    `t Value` = round(`t value`, 4),
    `p-value` = round(`Pr(>|t|)`, 4)
  ) %>%
  dplyr::select(Variable, Estimate, `Standard Error`, `t Value`, `p-value`)

#SUMMARY ORI DATA
r_squared2 <- c(orisummary_unemp$r.squared, orisummary_gdp$r.squared)
adj_r_squared2 <- c(orisummary_unemp$adj.r.squared, orisummary_gdp$adj.r.squared)
f_statistics2 <- c(orisummary_unemp$fstatistic[1], orisummary_gdp$fstatistic[1])
p_values2 <- c(
  pf(orisummary_unemp$fstatistic[1], orisummary_unemp$fstatistic[2], orisummary_unemp$fstatistic[3], lower.tail = FALSE),
  pf(orisummary_gdp$fstatistic[1], orisummary_gdp$fstatistic[2], orisummary_gdp$fstatistic[3], lower.tail = FALSE)
)

model_summary2 <- data.frame(
  Variable2 = c("Unemployment Rate", "GDP"),
  Multiple_R_Squared2 = round(r_squared2, 4),
  Adjusted_R_Squared2 = round(adj_r_squared2, 4),
  F_Statistic2 = round(f_statistics2, 4),
  p_value2 = round(p_values2, 4)
)

#MODEL ADEQUACY CHECKING
#Autocorrelation, Normality and Heteroscedasticity
serial_test <- serial.test(var_model2, lags.pt = 16, type = "PT.asymptotic")
normality_test <- normality.test(var_model2)
hetero_test <- arch.test(var_model2, lags.multi = 5)

serial_pval <- round(serial_test$serial$p.value, 4)
normality_pval <- round(normality_test$jb.mul$JB$p.value, 4)
hetero_pval <- round(hetero_test$arch.mul$p.value, 4)

anh_results <- data.frame(
  Variable = c("Portmanteau", "Jarque-Bera", "Heteroscedasticity"),
  Hypothesis = c(
    "H₀: The data has no autocorrelation<br>H₁: The data has autocorrelation",
    "H₀: The data is normally distributed<br>H₁: The data is not normally distributed",
    "H₀: The data has constant variance <br>H₁: The data does not have constant variance"
  ),
  p_value = c(serial_pval, normality_pval, hetero_pval),
  Conclusion = c(
    ifelse(serial_pval < 0.05, "Reject H₀.<br> The data has autocorrelation.", 
           "Fail to reject H₀.<br> The data has no autocorrelation."),
    ifelse(normality_pval < 0.05, "Reject H₀.<br> The data is not normally distributed.", 
           "Fail to reject H₀.<br> The data is normally distributed."),
    ifelse(hetero_pval < 0.05, "Reject H₀.<br>The data does not have constant variance.", 
           "Fail to reject H₀.<br>The data has constant variance.")
  )
)

# Define UI for application

ui <- dashboardPage(title = "Welcome to LabourNomics MY", skin = "black",
                    
                    # Header
                    dashboardHeader(title = tags$strong("LabourNomics MY"),titleWidth = 250),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                      tags$head(
                        tags$style(HTML("
                           .main-sidebar { 
                            width: 250px !important; /* Adjust sidebar width */
                          }
                          
                          .sidebar-menu>li>a {
                            width: 250px !important; /* Adjust menu item width */
                            display: block;
                            text-align: left;
                          }
                          
                          .main-header .logo {
                            width: 250px !important;
                          }
                          
                          .main-header .navbar {
                            margin-left: 250px !important;
                          }
                          
                          .content-wrapper, .main-footer {
                            margin-left: 250px !important;
                          }
                        "))
                      ),
                      
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("About", tabName = "about", icon = icon("info-circle")),
                        menuItem("MY Labour Insights", tabName = "ur", icon = icon("chart-line")),
                        menuItem("MY Economics Insights", tabName = "gdp", icon = icon("dollar-sign")),
                        menuItem("Data", tabName = "data", icon = icon("table")),
                        menuItem("Vector Autoregression (VAR)", tabName = "var_model", icon = icon("project-diagram"))),
                      sidebarUserPanel("LabourNomics MY")            
                    ),
                    
                    
                    # The body of the dashboard
                    dashboardBody(
                      withMathJax(),
                      tags$script(HTML("
                      $(document).ready(function(){
                        $('#toggleSidebar').on('click', function(){
                          $('.sidebar').toggleClass('hidden');
                        });
                      });
                    ")),
                      
                      tags$style(HTML("
 /* ----- General White Theme ----- */
                    body, .content-wrapper, .right-side {
                      background-color: #ffffff !important; /* White background */
                      color: black !important;
                    }
                    
                    /* ----- Turquoise Sidebar ----- */
                    .main-sidebar {
                      background-color: #40E0D0 !important; /* Turquoise */
                    }
                    
                    /* Sidebar Menu Item Text */
                    .sidebar-menu > li > a {
                      color: black !important;
                      font-weight: bold;
                    }
                    
                    /* Sidebar Menu Active Item */
                    .sidebar-menu > li.active > a {
                      background-color: #20B2AA !important; /* Slightly Darker Turquoise */
                      color: white !important;
                      width: 100% !important;
                    }
                  
                    /* Sidebar Hover Effect */
                    .sidebar-menu > li:hover > a {
                      background-color: #48D1CC !important; /* Lighter Turquoise */
                      color: black !important;
                      width: 100% !important;
                    }
                    
                    /* ----- Turquoise Box Headers ----- */
                    .box.box-solid.box-info {
                      border: 2px solid #20B2AA !important; /* Slightly Darker Turquoise */
                    }
                  
                    .box.box-solid.box-info > .box-header {
                      background-color: #40E0D0 !important; /* Turquoise */
                      color: black !important;
                      font-weight: bold;
                    }
                    
                    /* Text inside box */
                    .box.box-solid.box-info .box-body {
                      color: black !important;
                    }
                  
                    /* ----- Adjust Other Elements if Needed ----- */
                    .navbar {
                      background-color: #40E0D0 !important; /* Turquoise Top Navbar */
                      color: black !important;
                    }
                    
                    /* Navbar links */
                    .navbar .navbar-nav > li > a {
                      color: black !important;
                    }
                  
                    /* Navbar brand */
                    .navbar .navbar-brand {
                      color: black !important;
                      font-weight: bold;
                    }
                      
                     /* ----- Change Forecast Calculator Box to Turquoise ----- */
                    .box-primary {
                      background-color: #40E0D0 !important; /* Turquoise */
                      border: 2px solid #20B2AA !important; /* Slightly Darker Turquoise */
                      color: black !important;
                      font-weight: bold;
                    }
                    
                    /* Forecast Calculator Box Header */
                    .box-primary .box-header {
                      background-color: #20B2AA !important; /* Darker Turquoise */
                      color: white !important;
                      font-weight: bold;
                    }
                    
                    /* Forecast Calculator Box Body */
                    .box-primary .box-body {
                      color: black !important;
                    }
                    
                    /* Change DataTable selected row color */
                .dataTable tbody tr.selected {
                    background-color: #40E0D0 !important;  /* Turquoise */
                    color: white !important;  /* White text */
                }
                
                
                /* Change pagination button text to turquoise */
                .dataTables_wrapper .dataTables_paginate .paginate_button {
                    color: #008E8C !important;  /* Turquoise text */
                }
                
                /* Hover effect for pagination buttons */
                .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
                    background-color: #008E8C !important; /* Turquoise background */
                    color: white !important; /* White text */
                    border-radius: 5px !important; /* Rounded edges */
                }
                
                /* Active (selected) pagination button */
                .dataTables_wrapper .dataTables_paginate .paginate_button.current {
                    background-color: #008E8C !important; /* Turquoise background */
                    color: white !important; /* White text */
                    border-radius: 5px !important; /* Rounded edges */
                    font-weight: bold !important; /* Make text bold */
                }
                    
                    /* Change inactive tab color */
                    .nav-tabs > li > a { 
                        color: #009E9E !important; /* Turquoise color */
                    }
                    
                    /* Change active tab color */
                    .nav-tabs > li.active > a, 
                    .nav-tabs > li.active > a:focus, 
                    .nav-tabs > li.active > a:hover { 
                        background-color: #009E9E !important; 
                        color: white !important;
                        border: 1px solid #009E9E !important;
                    }
                    
                    /* Change hover effect */
                    .nav-tabs > li > a:hover {
                        background-color: #E0F7FA !important; /* Light turquoise on hover */
                        }
                  

                  ")),
                      
                      tabItems(
                        tabItem(tabName = "dashboard",
                                
                                fluidRow(
                                  valueBoxOutput("avg_ur", width = 4),
                                  valueBoxOutput("avg_gdp", width = 4),
                                  valueBoxOutput("avgln_gdp", width = 4)
                                ),
                                fluidRow(
                                  box(title = "VAR (1) Model Equations", width = 12, status = "info", solidHeader = TRUE,
                                      HTML("<b>UR<sub>t</sub> = 0.7991 UR<sub>t-1</sub> − 0.2294 GDP<sub>t-1</sub> + 2.7045</b><br>
                          <b>GDP<sub>t</sub> = 0.0111 UR<sub>t-1</sub> + 0.9933 GDP<sub>t-1</sub> + 0.0633</b><br>
                          <p>Where:</p>
                          <ul>
                            <li>UR<sub>t</sub> is the unemployment rate at time t</li>
                            <li>GDP<sub>t</sub> is the Gross Domestic Product (GDP) at time t</li>
                            <li>UR<sub>t-1</sub> is the unemployment rate at lag 1</li>
                            <li>GDP<sub>t-1</sub> is the Gross Domestic Product (GDP) at lag 1</li>
                            <br>
                            *Note that the GDP<sub>t</sub> and GDP<sub>t-1</sub> values are in logarithmic form*
                            
                          </ul>")
                                  )
                                ),
                                fluidRow(
                                  box(title = "Forecast Calculator", width = 4, status = "primary", solidHeader = TRUE,
                                      numericInput("ur_lag", "Enter UR (t-1):", value = "", step = 0.1),
                                      numericInput("gdp_lag", "Enter GDP (t-1) (Enter GDP in logarithmic value):", value = "", step = 0.1),
                                      actionButton("calculate", "Calculate")
                                  ),
                                  valueBoxOutput("ur_result", width = 4),
                                  valueBoxOutput("gdp_result", width = 4)
                                ),
                                fluidRow(
                                  box(title = "Interpretation", width = 12, status = "info", solidHeader = TRUE,
                                      uiOutput("interpretation"))
                                )
                        ),
                        
                        tabItem(tabName = "about",
                                h2(strong("Never heard of LabourNomics MY?")),
                                br(),
                                fluidRow(
                                  column(4, align = "center", 
                                         imageOutput("lmy_image", inline = TRUE) # Image on the left
                                  ),
                                  column(8, 
                                         h3(strong("Absolutely!")),
                                         p(" LabourNomics MY is a brand new platform designed to provide insights into the Malaysian economic landscape based on a study titled \"Dynamic Interactions Between Unemployment Rate and Malaysia's Gross Domestic Product (GDP): Forecasting Using Vector Autoregression (VAR) Approach\". The information displayed in this web app can be used by policymakers, economists, businesses, and even economics students for reference and any improvements in future studies."),
                                         p("Some details about this web app ⇓"),
                                         p("▶ The name combines the words 'Labour' and 'Economics,' referring to a focus on the two (2) crucial economic indicators—the unemployment rate and Gross Domestic Product (GDP)."),
                                         p("▶ Vector Autoregression (VAR) methodology was employed to investigate the dynamic relationships and impacts of these two variables."),
                                         p("▶ This study is based on Malaysian data from 1962 to 2022 sourced from OpenDOSM and the World Bank websites.")
                                  )
                                ),
                                hr(), br(),
                                
                                h2(strong("What is Shiny?")),
                                br(),
                                fluidRow(
                                  column(4, align = "center", 
                                         imageOutput("rs_image", inline = TRUE) # Image on the left
                                  ),
                                  column(8, 
                                         p("Shiny is a web application framework for R used in the production of LabourNomics MY web apps. 
                                          It allows for seamless integration of statistical 
                                          models with interactive visualizations by leveraging the 
                                          powerful forecasting libraries in R. As a result, the app can analyze historical unemployment rate 
                                          and GDP data, identify trends, and more importantly, determine the dynamic relationships and impacts 
                                          of the two economic variables. Users can interact with the app to adjust 
                                          model parameters, visualize forecasted trends, and explore
                                          various scenarios that affect Malaysia's unemployment rate and GDP.")
                                  )
                                ),
                                hr()
                        ),
                        
                        tabItem(tabName = "ur",
                                h2(strong("Exploration of Malaysia's Unemployment Rate")),
                                plotlyOutput("ur_plot"),
                                br(),
                                p("The unemployment rate was constantly decreasing starting 1960s
                                  until the early 1980s. This decline pattern indicates that Malaysia is expanding into a
                                  healthy and productive economic state. Next, noticeable spikes occurred around the
                                  mid-1980s and early 1990s, which may correspond to economic downturns or crises that
                                  impacted employment levels. From approximately 2000 onward, the unemployment rate
                                  exhibits minor fluctuations, maintaining a low and stable level. However, around 2020,
                                  there was a slight increase in Malaysia’s unemployment rate, reflecting the economic
                                  impact of the COVID-19 pandemic. The SARS-CoV-2 outbreak began in December 2019
                                  in Wuhan City, Hubei Province, China, and spread globally (Elengoe, 2020).")),
                        
                        tabItem(tabName = "gdp",
                                h2(strong("Exploration of Malaysia's Gross Domestic Product (GDP)")),
                                plotlyOutput("gdp_plot"),
                                br(),
                                p("There is a consistent increase in GDP per capita from the year 1962 to the year
                                  1980 which illustrates a period of sustained economic expansion in Malaysia. While the
                                  overall trend is upward, there are a few minor declines over this time. These changes
                                  could be related to economic downturns or worldwide crises, such as the Asian financial
                                  crisis of 1997-1998, which affected many countries in the region, including Malaysia. The
                                  1997 currency crisis, which caused the ringgit (RM) to plummet, was a significant shock
                                  (Ariff & Yap, 2001). Despite these downturns, the overall trend remains positive,
                                  indicating economic resiliency. GDP per capita has risen steadily since the early 2000s,
                                  indicating a period of sustained expansion. Overall, the graph indicates that Malaysia has
                                  seen stable long-term economic growth, with only a few disruptions due to economic
                                  crises.")),
                        
                        tabItem(tabName = "data",
                                h2(strong("Dataset Overview")),
                                DTOutput("data_table")
                        ),
                        
                        tabItem(tabName = "var_model",
                                
                                titlePanel(strong("Vector Autoregression (VAR) Methodology")),
                                tabsetPanel(type = "tabs",
                                            
                                            tabPanel(strong("What is VAR?"),
                                                     h4(strong("What is Vector Autoregression (VAR)?")),
                                                     hr(),
                                                     p("Vector Autoregression (VAR) is a time series model
                                                       used in a study when there is a relationship between two or more time series variables where
                                                       GDP and unemployment rate are explored in this study. Essentially, the VAR model is an
                                                       extension of the univariate autoregressive model (ARIMA), which analyses the evolution of
                                                       multivariate linear time series with k endogenous variables. In this model, all variables, the
                                                       unemployment rate and GDP are endogenous and a function of the lagged values of all the
                                                       variables will be investigated. Characterization relies heavily on the model’s order, which
                                                       refers to the number of preceding periods used. For instance, a VAR (3) model uses a linear
                                                       combination of the system’s variables across the last three periods (lags) (Katris, 2021)."),
                                                     hr(),
                                                     br(),
                                                     h4(strong("Process of Constructing the VAR Model")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("flowchart_image", inline = TRUE)),
                                                     br(),
                                                     p(strong("Note:"), "In this study, the GDP data have been transformed into logarithmic form in the analysis."),
                                                     br(),
                                                     
                                                     h5(strong("1. Stationarity")),
                                                     hr(),
                                                     p("The term ’stationarity’ describes a series where there is no
                                                       obvious pattern in the means or variations over time. One of the main challenges in
                                                       estimating models using non-stationary data is the possibility of generating inaccurate test
                                                       statistics, which could result in the wrong model being chosen. The Dickey-Fuller test is a form of a unit root test
                                                       commonly used to assess the stationarity of a series."),
                                                     hr(),
                                                     
                                                     h5(strong("2. Number of Lags (p)")),
                                                     hr(),
                                                     p("The selection of an appropriate lag length is a critical step in ensuring that the model
                                                     represents accurate temporal dynamics of the variables involved. Choosing too few lags
                                                     might result in missing important information, while too many can lead to overfitting and
                                                     inefficiency. When estimating the number of lags (p) in a VAR model, information
                                                     criteria such as AIC (Akaike Information Criterion), HQIC (Hannan-Quinn Information
                                                     Criterion), SC (Schwarz Information Criterion), and FPE (Final Prediction Error) are
                                                     essential. These criteria serve to balance model complexity and goodness of fit, assisting
                                                     in the selection of the best lag order for the VAR model. Lower values of all AIC, HQIC, SC,
                                                     and FPE imply a better model fit."),
                                                     hr(),
                                                     
                                                     h5(strong("3. Model Estimation")),
                                                     hr(),
                                                     p("The model estimation step in a VAR model involves determining the coefficients (φ) of
                                                       the system of equations that describe the variables’ interactions over time. The structure
                                                       of the VAR model usually requires the application of Ordinary Least Squares (OLS) for each equation independently.
                                                       The unemployment rate and GDP variables in the VAR model are considered endogenous,
                                                       which means they can be classified as either an independent or dependent variable. Before
                                                       using OLS to estimate the coefficients, the system of equations needs to be defined."),
                                                     hr(),
                                                     
                                                     h5(strong("4. Model Adequacy Checking")),
                                                     hr(),
                                                     p("Model adequacy testing in a VAR model is critical to ensure its validity and dependability.
                                                     This includes several diagnostic tests and evaluations to ensure that the model assumptions
                                                     are met and that the model fits the data well."),
                                                     hr(),
                                                     
                                                     h5(strong("5. Variance Decomposition")),
                                                     hr(),
                                                     p("The variance decomposition step in a VAR model quantifies the contribution of each
                                                     unexpected, one-time change or disturbance that impacts a variable in the system (shock)
                                                     to the variable’s prediction error variance (variability). It is also known as Forecast Error
                                                     Variance Decomposition. In simple terms, it enables researchers to determine how much
                                                     of the future variability in a variable is due to shocks in the variable itself or shocks in
                                                     other variables in the system. For example, in a model that includes unemployment and
                                                     GDP, variance decomposition aids in determining how much future uncertainty (variance)
                                                     in GDP can be attributed to shocks in GDP itself versus shocks in unemployment, and
                                                     vice versa"),
                                                     hr(),
                                                     
                                                     h5(strong("6. Forecasting")),
                                                     hr(),
                                                     p("After the VAR model has been fitted and proven reliable, forecasting is performed by
                                                     subsequently using the obtained model equations to estimate future values. Forecasting is
                                                     projecting future values based on past data and model interactions. This method of
                                                     analysis provides useful insights into the variables’ future paths while taking into account
                                                     their historical connection, making it an effective decision-making tool in a variety of
                                                     sectors."),
                                                     hr(),
                                                     
                                                     h5(strong("7. Impulse Response Function (IRF)")),
                                                     hr(),
                                                     p("Individual parameter values in a Vector Autoregressive (VAR) model provide only limited
                                                       insight into how the system will react to a shock because all the variables are associated.
                                                       Consequently, figuring out the Impulse Response Function (IRF) is crucial to fully
                                                       understand the dynamic behavior of the model. The IRF analyses how the present and
                                                       future values of the endogenous variables are affected by a one-time shock to one of the
                                                       innovations.
                                                       When the response variable’s trajectory is plotted, the wavy curves
                                                       commonly seen in macroeconomic literature are usually constructed."),
                                                     p("The VAR model is continuously used to trace out the impacts of the shock to determine
                                                       the IRF. A shock to the i-th variable at time t is observed over time to see how it affects
                                                       the i-th variable and other variables in the system. The formula, which takes into account
                                                       the interactions recorded by the VAR model, explains how the initial change caused by the
                                                       shock spreads across the system over time. "),
                                                     hr(),
                                                     
                                            ),
                                            
                                            tabPanel(strong("Step 1"),
                                                     h4(strong("Step 1: Augmented Dickey-Fuller (ADF) Test of Stationarity")),
                                                     p(strong("ADF Test Before Differencing")),
                                                     DTOutput("nondiffadf_table"),
                                                     hr(),
                                                     p("The ADF test before differencing show that both p-values
                                                      of the unemployment rate and GDP exceed the significance level of 0.05. Hence, we fail to reject the null hypothesis of
                                                      non-stationarity. This finding confirms that the unemployment rate and GDP data are
                                                      not stationary and require differencing process to achieve stationarity."),
                                                     hr(),
                                                     p(strong("ADF Test After Differencing")),
                                                     DTOutput("diffadf_table"),
                                                     hr(),
                                                     p("The ADF test after differencing show that both p-values
                                                      of the unemployment rate and GDP do not exceed the significance level of 0.05. Hence, we reject the null hypothesis of
                                                      non-stationarity. This finding confirms that the unemployment rate and GDP data are stationary."),
                                                     p("However, it is also essential to consider non-stationary data even after differencing, as it
                                                      may still yield results suggesting its persistence in the model estimation of a VAR
                                                      framework."),
                                                     hr(),
                                                     br()
                                            ),
                                            
                                            tabPanel(strong("Step 2"),
                                                     h4(strong("Step 2: Selection of the Optimal Number of Lags")),
                                                     hr(),
                                                     p(strong("Statistical Criterion:")),
                                                     p("1. Akaike Information Criterion (AIC)"),
                                                     p("2. Hannan-Quinn Information Criterion (HQ)"),
                                                     p("3. Schwarz Criterion (SC)"),
                                                     p("4. Final Prediction Error (FPE)"),
                                                     hr(),
                                                     br(),
                                                     DTOutput("lag_table"),
                                                     hr(),
                                                     p("From the result, lag 1 is the best choice for modeling because it balances
                                                     model complexity and fits. The criteria AIC, HQ, SC, and FPE show that lag 1 is optimal,
                                                     indicating a well-fitting model with minimal complexity. This enables the model to
                                                     capture the key dynamics in time series data for accurate forecasting and analysis."),
                                                     hr(),
                                                     br()
                                            ),
                                            
                                            tabPanel(strong("Step 3"),
                                                     h4(strong("Step 3: Model Estimation")),
                                                     h4(strong("Stationary Data")),
                                                     p(strong("Model Estimation of Stationary Unemployment Rate Data")),
                                                     DTOutput("var_table"),
                                                     hr(),
                                                     p("The coefficient of the unemployment rate is 0.1266, indicating a positive
                                               relationship between the previous period’s change in the unemployment rate and its
                                               current value. Conversely, the GDP coefficient is-0.4955, suggesting a negative
                                               relationship between the previous period’s change in GDP and the current value of the
                                               dependent variable, the unemployment rate. However, with p-values of 0.3887 for the
                                               unemployment rate and 0.5299 for GDP, both values are above the significant level of
                                               0.05, indicating that these relationships are not statistically significant"),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("Model Estimation of Stationary GDP Data")),
                                                     DTOutput("var_table2"),
                                                     hr(),
                                                     p("The coefficient for the unemployment rate is -0.0062 indicating a negative
                                               relationship between the previous period’s change in unemployment and the current value
                                               of the dependent variable, GDP. Conversely, the GDP coefficient is 0.2056, suggesting a
                                               positive relationship between the previous period’s change in GDP and the current value of
                                               GDP. However, with p-values of 0.8137 for the unemployment rate and 0.1510 for GDP,
                                               both values are above the significant level of 0.05, indicating that these relationships are
                                               not statistically significant"),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("Model Summary of Stationary Data")),
                                                     DTOutput("model_summary_table"),
                                                     hr(),
                                                     p("From the table, it can be observed that the R-squared value for the unemployment rate is
                                               0.0334 and the R-squared value for the GDP is 0.0503. This means that the lagged values
                                               of the variables in the VAR model explain only about 3.34% of the variation in the
                                               unemployment rate, while the lagged values of the variables in the VAR model explain
                                               only about 5.03% of the variation in the GDP. This low R-squared indicates that the model
                                               does not account for much of the variability in the data, implying weak predictive power."),
                                                     p("Furthermore, a p-value of 0.3858 for the unemployment rate model and 0.2358 for the GDP
                                               model is used to test the overall significance of the model. Since both p-values are greater
                                               than 0.05, we donotreject the null hypothesis where all coefficients are zero, indicating that
                                               the predictors do not significantly improve the model’s ability to explain the relationships
                                               between the dependent and the independent variables. The models are not significant."),
                                                     hr(),
                                                     br(),         
                                                     
                                                     h4(strong("Original Data")),
                                                     p(strong("Model Estimation of Original Unemployment Rate Data")),
                                                     DTOutput("ori_var_table"),
                                                     hr(),
                                                     p("The unemployment rate has a coefficient of 0.7991, which
                                               means that when the unemployment rate changes in the previous period, it positively
                                               affects its current value. On the other hand, the GDP coefficient is -0.2294, showing that a
                                               change in GDP from the previous period negatively affects the current value of the
                                               unemployment rate. The p-value for the unemployment rate is 0.0000, which is lower than
                                               the 0.05 significance level, meaning that the unemployment rate is a strong predictor of
                                               the dependent variable. In comparison, the GDP’s p-value is 0.0971, which is close to
                                               being significant, but it suggests that its effect is not as clear. Overall, these results show
                                               that the previous value of the unemployment rate has a stronger impact on its current value
                                               compared to the impact made by the previous value of GDP."),
                                                     hr(),
                                                     br(), 
                                                     
                                                     p(strong("Model Estimation of Original GDP Data")),
                                                     DTOutput("ori_var_table2"),
                                                     hr(),
                                                     p("The unemployment rate has a coefficient of 0.0111, which means
                                               that when the unemployment rate changes in the previous period, it positively affects the
                                               current value of GDP. Similarly, the GDP coefficient is 0.9933, showing that a change in
                                               GDP from the previous period has a positive effect on its current value. The p-value for
                                               the unemployment rate is 0.4960, which is much greater than the 0.05 significance level,
                                               meaning that the unemployment rate’s effect is not as clear. In comparison, the GDP’s p
                                               value is 0.0000, which indicates a strong predictor of the dependent variable, GDP. Overall,
                                               these results show that the previous value of GDP has a stronger impact on its current value
                                               compared to the impact of the unemployment rate on the GDP’s current value."),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("Model Summary of Original Data")),
                                                     DTOutput("ori_summary_table"),
                                                     hr(),
                                                     p("From the result, the multiple R-squared value of the unemployment rate is
                                               0.9033 while the multiple R-squared value for the GDP is 0.9917. This means that lagged
                                               values of the variables in the VAR model explain about 90.33% of the variation in the
                                               unemployment rate, while the lagged values of the variables in the VAR model explain
                                               about 99.17% of the variation in the GDP. These high R-squared indicate that the models
                                               account for much of the variability in the data, implying strong predictive power. The
                                               adjusted R-squared for both models also are quite high as the R-squared values, indicating
                                               that the included predictors are meaningful."),
                                                     p("The p-values of 0.0000 for the unemployment rate and the GDP models are
                                               used to test the overall significance of the model. Since the p-values are less than the
                                               significance level of 0.05, the null hypothesis of all coefficients equal to zero is rejected,
                                               indicating that the predictors significantly improve the model’s ability to explain the
                                               variability in the GDP. The models are significant."),
                                                     hr(),
                                                     br(),
                                                     
                                                     h4(strong("The Best Model Estimation")),
                                                     p(HTML("<b>UR<sub>t</sub> = 0.7991 UR<sub>t-1</sub> − 0.2294 GDP<sub>t-1</sub> + 2.7045</b><br>
                                              <b>GDP<sub>t</sub> = 0.0111 UR<sub>t-1</sub> + 0.9933 GDP<sub>t-1</sub> + 0.0633</b><br>
                                              <p>Where:</p>
                                              <ul>
                                                <li>UR<sub>t</sub> is the unemployment rate at time t</li>
                                                <li>GDP<sub>t</sub> is the Gross Domestic Product (GDP) at time t</li>
                                                <li>UR<sub>t-1</sub> is the unemployment rate at lag 1</li>
                                                <li>GDP<sub>t-1</sub> is the Gross Domestic Product (GDP) at lag 1</li>
                                              </ul>"),
                                                       hr(),
                                                       p("In the UR equation, by holding the previous
                                               year’s unemployment rate constant, on average, a one percent increase in the previous
                                               year’s GDP (in logarithmic form) will decrease the current unemployment rate by
                                               0.2294% suggesting a moderate inverse relationship. Meanwhile in the GDP Equation, by
                                               holding the previous year’s GDP constant, on average, a one percent increase in the
                                               previous year’s unemployment rate will increase the current GDP by 0.0111% (in
                                               logarithmic form) suggesting a very weak positive relationship and contradicts Okun’s
                                               law."),
                                                       p("According to Okun’s Law, higher unemployment usually corresponds to a lower GDP.
                                               The contradiction of Okun’s law in the GDP Equation is consistent with the findings of a study
                                               by Foroni and Furlanetto (2022), where variances from Okun’s law are linked to shocks
                                               such as positive labour supply, technological advancements, or negative structural factors.
                                               These dynamics can provide scenarios in which unemployment and GDP move in the same
                                               direction, as demonstrated in the GDP Equation model."),
                                                       hr(),
                                                       br()
                                                     )),
                                            
                                            tabPanel(strong("Step 4"),
                                                     h4(strong("Step 4: Model Adequacy Checking")),
                                                     h4(strong("Tests for Autocorrelation, Normality, and Heteroscedasticity of Residuals")),
                                                     DTOutput("anh_table"),
                                                     hr(),
                                                     p("From the model adequacy checking analysis, the Portmanteau test indicates
                                               no significant autocorrelation in the residuals as indicated by a p-value of 0.4392. The
                                               heteroscedasticity test, with a p-value of 0.1901, suggests constant variance
                                               (homoscedasticity) in the residuals, supporting the model’s assumption of uniform
                                               variance. However, the Jarque-Bera test produces a very low p-value of 0.0000, indicating
                                               that the residuals deviate significantly from normality, which could affect the reliability of
                                               statistical inferences. In forecasting, the normality assumption is not crucial because the
                                               focus is on predicting future values, not on statistical inferences such as hypothesis testing
                                               or confidence intervals."),
                                                     hr(),
                                                     br(),
                                                     
                                                     h4(strong("Stability Test")),
                                                     p(strong("Stability Test for Unemployment Rate")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("stability_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "The CUSUM plot shows that the black line remains within the red boundaries 
                                                  indicating the model is stable with no significant structural breaks. Hence, 
                                                  the stability assumption for the unemployment rate data is satisfied."
                                                     ),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("Stability Test for GDP")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("stability2_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "The CUSUM plot shows that the black line remains within the red boundaries 
                                                  indicating the model is stable with no significant structural breaks. Hence, 
                                                  the stability assumption for the GDP data is satisfied."
                                                     ),
                                                     hr(),
                                                     br()
                                            ),
                                            
                                            tabPanel(strong("Step 5"),
                                                     h4(strong("Step 5: Variance Decomposition")),
                                                     p(strong("Variance Decomposition of Unemployment Rate")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("vd_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "The unemployment rate is predominantly influenced by its past
                                                   values, with nearly 100% of its future shifts being explained by unemployment itself in
                                                   the early years (as shown by the green bars). This indicates that, in the short term, changes
                                                   in the unemployment rate are largely independent of GDP, and past trends mainly
                                                   determine its future values. However, as the forecast horizon extends to around seven
                                                   years, a small portion of the bars begins to be attributed to GDP (indicated by the purple
                                                   section). This suggests that, over time, GDP contributes slightly to the variation in the
                                                   unemployment rate, highlighting a growing, but still minor, relationship between the two
                                                   variables in the longer term."
                                                     ),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("Variance Decomposition of GDP")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("vd2_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "The variance decomposition shows that GDP is mostly influenced by its past
                                               values, with about 90% of its future changes being explained by itself over the next 10 years
                                               (as seen in the purple bars). This means that in the short to medium term, GDP is mainly
                                               driven by its trends, with only a small effect from other factors like the unemployment
                                               rate. The small green sections at the bottom of each bar show that unemployment slightly
                                               impacts GDP. While this impact is small, it implies a weak relationship between the two,
                                               but GDP’s future movement is mostly independent of the unemployment rate during the
                                               forecast period."
                                                     ),
                                                     hr(),
                                                     br()
                                            ),
                                            
                                            tabPanel(strong("Step 6"),
                                                     h4(strong("Step 6: Forecasting")),
                                                     p(strong("10-Year Unemployment Rate Forecast Analysis")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("f_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "Figure above depicts the unemployment rate forecast, which includes both historical data and
                                               predicted numbers for the next ten years. The solid black line depicts the historical values
                                               of unemployment rates over time, which fluctuate until reach the end of the observation
                                               period. The solid blue line represents the forecast trend, suggesting a stable and
                                               decreasing trend of the unemployment rate for the upcoming years. This suggests that the
                                               model predicts no significant increase in the unemployment rate, with the possibility of a
                                               small decrease."),
                                                     br(),
                                                     p("Furthermore, as the forecast period extends, the confidence interval widens, indicating
                                              increased uncertainty regarding future projections. This increased uncertainty is the result of
                                              prospective policy changes or unforeseen events that may have an impact on employment."),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("10-Year GDP Forecast Analysis")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("f2_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "Figure above shows the GDP per capita (in logarithmic form) forecast, including past data
                                               and predicted values for the next ten years. The solid black line to the left of the vertical
                                               dashed line represents the historical values of the GDP data. Although there are some ups
                                               and downs, the line generally goes up, showing that GDP per capita has increased over
                                               time. The predicted values are shown to the right of the dashed line. The solid blue line
                                               represents the forecast, which suggests that GDP per capita will keep growing slowly,
                                               indicating steady growth in the future."),
                                                     br(),
                                                     p("The blue forecast line is surrounded by the confidence interval reflecting that as the forecast
                                               period gets longer, the confidence interval gets wider, meaning there is more uncertainty
                                               about future predictions. This widening shows that actual values might differ from the
                                               forecast due to unexpected events, such as changes in the economy or government policies."),
                                                     hr(),
                                                     br()
                                            ),
                                            
                                            tabPanel(strong("Step 7"),
                                                     h4(strong("Step 7: Impulse Response Function (IRF)")),
                                                     p(strong("10-Year Impact of Unemployment Rate on GDP")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("irf_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "The blue IRF line initially falls below zero, showing a positive
                                     shock in the unemployment rate affects GDP negatively, which aligns with Okun’s Law.
                                     Okun (1962) discovered that an increase in the unemployment rate leads to a decrease in
                                     GDP (Pata et al., 2018). Over time, the negative effect fades and the IRF line approaches
                                     zero, indicating that the economy is beginning to recover. The confidence intervals expand
                                     over time, reflecting greater uncertainty in the long-term impact. Since the effect is most
                                     significant in the early years until year 2, it appears that the impact of unemployment rate
                                     shocks on GDP is primarily a short-run effect."
                                                     ),
                                                     hr(),
                                                     br(),
                                                     
                                                     p(strong("10-Year Impact of GDP on Unemployment Rate")),
                                                     div(style = "text-align: center;",
                                                         imageOutput("irf2_image", inline = TRUE)),
                                                     hr(),
                                                     p(style = "margin-top: -10px;",
                                                       "The blue IRF line initially falls below zero, showing a positive
                                     shock in the GDP affects the unemployment rate negatively, which aligns with Okun’s
                                     Law. An increase in GDP is generally linked to a decrease in unemployment, according
                                     to Okun’s Law, indicating the relationship between job creation and economic growth.
                                     The confidence intervals expand over time, reflecting greater uncertainty in the long-term
                                     impact. Since the effect is most significant in the early years until year 4, it appears that the
                                     impact of GDP shocks on the unemployment rate is primarily a short-run effect."
                                                     ),
                                                     hr()
                                            )
                                            
                                ))
                      )))


# Define server logic required

server <- function(input, output, session) {
  tab_list <- NULL
  
  observe({
    showNotification("Created by Yusra BSc. (Hons.) Statistics", duration = NULL, type = "message")
  })
  
  # Average Unemployment Rate (server) ------------------------------------------
  output$avg_ur <- renderValueBox({
    fypdata %>%
      summarise(avg_unemployment = mean(`UNEMPLOYMENT RATE`, na.rm = TRUE)) %>%  # Calculate average UR
      pull() %>%
      round(2) %>%  # Round to 2 decimal places
      valueBox(
        icon = icon("chart-line"), 
        color = "teal", 
        subtitle = "Average Unemployment Rate (% of Total Labour Force)"
      )
  })
  
  # Average GDP (server) ------------------------------------------
  output$avg_gdp <- renderValueBox({
    fypdata %>%
      summarise(avg_gdp = mean(`GDP PER CAPITA`, na.rm = TRUE)) %>%  # Calculate average GDP
      pull() %>%
      round(2) %>%  # Round to 2 decimal places
      prettyNum(big.mark = ",") %>%  # Format with commas
      valueBox(
        icon = icon("dollar-sign"), 
        color = "green", 
        subtitle = "Average GDP Per Capita (in US Dollars)"
      )
  })
  
  # Average ln(GDP) (server) ------------------------------------------
  output$avgln_gdp <- renderValueBox({
    fypdata %>%
      summarise(avgln_gdp = mean(`ln(GDP PER CAPITA)`, na.rm = TRUE)) %>%  # Calculate average GDP
      pull() %>%
      round(2) %>%  # Round to 2 decimal places
      prettyNum(big.mark = ",") %>%  # Format with commas
      valueBox(
        icon = icon("money-bill"), 
        color = "olive", 
        subtitle = "Average GDP Per Capita (in Logarithmic Form)"
      )
  })
  
  # Reactive calculations based on user inputs
  results <- eventReactive(input$calculate, {
    ur_t <- 0.7991 * input$ur_lag - 0.2294 * input$gdp_lag + 2.7045
    gdp_t <- 0.0111 * input$ur_lag + 0.9933 * input$gdp_lag + 0.0633
    list(ur = ur_t, gdp = gdp_t)
  })
  
  # Display Predicted UR
  output$ur_result <- renderValueBox({
    valueBox(
      value = round(results()$ur, 2), 
      subtitle = "Predicted Unemployment Rate (% of Total Labour Force)",
      icon = icon("chart-line")
    )
  })
  
  # Display Predicted GDP
  output$gdp_result <- renderValueBox({
    valueBox(
      value = round(results()$gdp, 2), 
      subtitle = "Predicted GDP Per Capita (in Logarithmic Form)",
      icon = icon("money-bill")
    )
  })
  
  # Provide Interpretation
  output$interpretation <- renderUI({
    req(results())
    
    gdp_change <- ifelse(results()$gdp > input$gdp_lag, "greater", "less")
    gdp_change2 <- ifelse(results()$gdp > input$gdp_lag, "increasing", "decreasing")
    
    HTML(paste0("If the lagged unemployment rate value is ", input$ur_lag, 
                " and the lagged logarithmic GDP value is ", input$gdp_lag, ", the conclusions that can be made are:<br>",
                "▶ The forecasted unemployment rate is ", round(results()$ur, 2), "% of the total labour force.<br>",
                "▶ The GDP is ",gdp_change2, " since the forecasted logarithmic GDP of ", round(results()$gdp, 2), " is ", gdp_change, " than the previous value of GDP (t-1).<br>",
                "A higher GDP generally indicates a growing economy, while a lower unemployment rate suggests better job opportunities."))
  })
  
  
  output$ur_plot <- renderPlotly({
    plot_ly(fypdata, type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~YEAR, y = ~`UNEMPLOYMENT RATE`, name = "Unemployment Rate (% of Total Labour Force)") %>%
      layout(
        showlegend = F,
        xaxis = list(title = "Year"),  # Set x-axis label
        yaxis = list(title = "Unemployment Rate (% of Total Labour Force)")  # Set y-axis label
      )
  })
  
  output$gdp_plot<-renderPlotly({
    plot_ly(fypdata, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~YEAR, y = ~`ln(GDP PER CAPITA)`, name = "ln(GDP Per Capita) (in US Dollars)")%>%
      layout(showlegend = F,
             xaxis = list(title = "Year"),  # Set x-axis label
             yaxis = list(title = "ln(GDP Per Capita) (in US Dollars)")  # Set y-axis label       
      )
  })
  
  output$ur_eq <- renderValueBox({
    valueBox(
      value = "UR Equation",  
      subtitle = HTML("$$UR_{t} = 0.7991UR_{t-1} - 0.2294GDP_{t-1} + 2.7045$$"),
      icon = icon("calculator")
    )
  })
  
  output$gdp_eq <- renderValueBox({
    valueBox(
      value = "GDP Equation",  
      subtitle = HTML("$$GDP_{t} = 0.0111UR_{t-1} + 0.9933GDP_{t-1} + 0.0633$$"),
      icon = icon("chart-line")
    )
  })
  
  output$lmy_image <- renderImage({
    list(
      src = file.path("yyy", "LABOURNOMICS.png"),
      contentType = "image/png",
      width = 160,
      height = 160,
      alt = " "
    )
  }, deleteFile = FALSE)
  
  output$rs_image <- renderImage({
    list(
      src = file.path("yyy", "shiny_logo.png"),
      contentType = "image/png",
      width = 160,
      height = 160,
      alt = " "
    )
  }, deleteFile = FALSE)
  
  output$data_table <- renderDT({
    datatable(fypdata, 
              options = list(
                pageLength = 10,   # Show 10 rows per page
                searchHighlight = TRUE,  # Highlight search results
                autoWidth = TRUE,  # Auto adjust column width
                dom = 'ltipr'  # Remove global search box, keep column filters
              ),
              filter = "top",  # Place individual search bars above each column
              selection = "single"
    )
  })
  
  output$flowchart_image <- renderImage({
    list(
      src = file.path("yyy", "FLOWCHART.png"),
      contentType = "image/png",
      width = 300,
      height = 700,
      alt = "Process of Constructing the VAR Model"
    )
  }, deleteFile = FALSE)
  
  output$nondiffadf_table <- renderDT({
    datatable(
      adf_results,  # Display ADF results
      rownames = FALSE,
      escape = FALSE,
      options = list(
        dom = 't',  # Hide search, pagination
        paging = FALSE),
      colnames = c("Variable", "Hypothesis", "Test Statistic", "p-value", "Conclusion"),
      width = "80%"
    ) %>%
      formatStyle(columns = colnames(adf_results), 'text-align' = 'l')
  })
  
  output$diffadf_table <- renderDT({
    datatable(
      adf_results2,  # Display ADF results
      rownames = FALSE,
      escape = FALSE,
      options = list(
        dom = 't',  # Hide search, pagination
        paging = FALSE),
      colnames = c("Variable", "Hypothesis", "Test Statistic", "p-value", "Conclusion"),
      width = "60%"
    ) %>%
      formatStyle(columns = colnames(adf_results2), 'text-align' = 'l')
  })
  
  output$lag_table <- renderDT({
    datatable(lag_table, options = list(
      dom = 't',  # Remove search bar, only show table
      paging = FALSE,  # Disable pagination
      ordering = FALSE,  # Disable sorting
      autoWidth = TRUE,
      width = "30%"
    ))
  })
  
  output$var_table <- renderDT({
    datatable(coef_table_unemp1, 
              options = list(dom = 't', paging = FALSE, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle(columns = colnames(coef_table_unemp1), textAlign = "left")
  })
  
  output$var_table2 <- renderDT({
    datatable(coef_table_gdp1, 
              options = list(dom = 't', paging = FALSE, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle(columns = colnames(coef_table_gdp1), textAlign = "left")
  })
  
  
  output$model_summary_table <- renderDT({
    datatable(model_summary, options = list(dom = 't', paging = FALSE, autoWidth = TRUE), rownames = FALSE, width = "80%",
              colnames = c("Variable", "Multiple R-Squared", "Adjusted R-Squared", "F Statistic", "p-value"))%>%
      formatStyle(columns = seq_along(colnames(model_summary2)), textAlign = "left")
  })
  
  output$ori_var_table <- renderDT({
    datatable(ori_table_unemp1, 
              options = list(dom = 't', paging = FALSE, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle(columns = colnames(ori_table_unemp1), textAlign = "left")
  })
  
  output$ori_var_table2 <- renderDT({
    datatable(ori_table_gdp1, 
              options = list(dom = 't', paging = FALSE, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle(columns = colnames(ori_table_gdp1), textAlign = "left")
  })
  
  output$ori_summary_table <- renderDT({
    datatable(model_summary2, options = list(dom = 't', paging = FALSE, autoWidth = TRUE), rownames = FALSE, width = "80%",
              colnames = c("Variable", "Multiple R-Squared", "Adjusted R-Squared", "F Statistic", "p-value"))%>%
      formatStyle(columns = seq_along(colnames(model_summary2)), textAlign = "left")
  })
  
  output$anh_table <- renderDT({
    datatable(
      anh_results,  
      rownames = FALSE,
      escape = FALSE,
      options = list(
        dom = 't',  # Hide search, pagination
        paging = FALSE),
      colnames = c("Test", "Hypothesis", "p-value", "Conclusion"),
      width = "80%"
    ) %>%
      formatStyle(columns = colnames(anh_results), 'text-align' = 'l')
  })
  
  output$stability_image <- renderImage({
    list(
      src = file.path("yyy", "STABILITY_UNEMP.png"),
      contentType = "image/png",
      width = 500,
      height = 270,
      alt = "Stability Test for Unemployment Rate"
    )
  }, deleteFile = FALSE)
  
  output$stability2_image <- renderImage({
    list(
      src = file.path("yyy", "STABILITY_GDP.png"),
      contentType = "image/png",
      width = 500,
      height = 270,
      alt = "Stability Test for GDP"
    )
  }, deleteFile = FALSE)
  
  output$vd_image <- renderImage({
    list(
      src = file.path("yyy", "VD UNEMP.png"),
      contentType = "image/png",
      width = 530,
      height = 270,
      alt = "Variance Decomposition for Unemployment Rate"
    )
  }, deleteFile = FALSE)
  
  output$vd2_image <- renderImage({
    list(
      src = file.path("yyy", "VD GDP.png"),
      contentType = "image/png",
      width = 530,
      height = 270,
      alt = "Variance Decomposition for GDP"
    )
  }, deleteFile = FALSE)
  
  output$f_image <- renderImage({
    list(
      src = file.path("yyy", "FORECAST UNEMP.png"),
      contentType = "image/png",
      width = 500,
      height = 300,
      alt = "10-Year Unemployment Rate Forecast"
    )
  }, deleteFile = FALSE)
  
  output$f2_image <- renderImage({
    list(
      src = file.path("yyy", "FORECAST GDP.png"),
      contentType = "image/png",
      width = 500,
      height = 300,
      alt = "10-Year GDP Per Capita Forecast"
    )
  }, deleteFile = FALSE)
  
  output$irf_image <- renderImage({
    list(
      src = file.path("yyy", "IRF_UR ON GDP.png"),
      contentType = "image/png",
      width = 460,
      height = 300,
      alt = "10-Year Impact of Unemployment Rate on GDP"
    )
  }, deleteFile = FALSE)
  
  output$irf2_image <- renderImage({
    list(
      src = file.path("yyy", "IRF_GDP ON UR.png"),
      contentType = "image/png",
      width = 460,
      height = 300,
      alt = "10-Year Impact of GDP on Unemployment Rate"
    )
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
