library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(bslib)
library(mlt)
library(tram)
library(cotram)
library(ggplot2)
library(ggpubr)
library(mlbench)
library(ordinal)
library(tidyr)
library(dplyr)

# Get last modified date of app file for the footer
last_modified <- file.info("app.R")$mtime

# Load datasets ----------------------------------------------------------------
data("faithful")
data("BostonHousing2", package = "mlbench")
data("wine", package = "ordinal")

# Load and preprocess deer-vehicle collisions data
# Code copied from the cotram package vignette's R code, chunks "DVC-data" and "DVC-setup":
# "https://cran.r-project.org/web/packages/cotram/vignettes/cotram.R".

obs <- NULL
if (!file.exists("analysis/DVC.rda")) {
  op <- options(timeout = 120)
  dwnfl <- try(download.file("https://zenodo.org/record/17179/files/DVC.tgz",
                             "DVC.tgz"))
  options(op)
  if (!inherits(dwnfl, "try-error")) {
    untar("DVC.tgz", file = "analysis/DVC.rda")
    load("analysis/DVC.rda")
  }
} else {
  load("analysis/DVC.rda")
}

# set-ups
loc <- Sys.setlocale("LC_ALL", "en_US.UTF-8")
rm(loc)

# data-frame setup
dvc <- data.frame(margin.table(obs[,,"wild"], 2))
colnames(dvc) <- c("day", "DVC")
dvc$day <- as.Date(dvc$day)
dvc$weekday <- factor(format(dvc$day, "%A"),
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                 "Friday", "Saturday", "Sunday"))
dvc$weekday[weekdays$ArbZeitFaktor == 0] <- "Sunday"
dvc$year <- factor(format(dvc$day, "%Y"))
dvc$time <- as.numeric(difftime(dvc$day, start, unit = "days"))

# harmonics
sfm <- function(timevar, freq = 365, S = 10) {
  S <- 1:S * 2
  paste("I(", rep(c("sin", "cos"), length(S)), "(",
        rep(S, rep(2, length(S))), "* pi *", timevar, "/", freq, "))",
        collapse = "+")
}

Xtime <- model.matrix(as.formula(paste("~", sfm("time"))), data = dvc)[,-1]
Xtime <- as.data.frame(Xtime)
colnames(Xtime) <- paste0("tvar", 1:ncol(Xtime))

# Final deer-vehicle collisions dataframe
dvc <- cbind(dvc, Xtime)

# Default values for models ----------------------------------------------------

# Old Faithful
defaults_faithful <- list(
  bernstein = list(
    order = 8,
    ui_constraint = "increasing"
  ),
  distribution = "Normal",
  params = list(
    support_min = 40,
    support_max = 100,
    bound_min_choice = "Numeric input",
    bound_min_num = 0,
    bound_max_choice = "Inf",
    bound_max_num = 200
  )
)

# Boston Linear
defaults_linear <- list(
  bernstein = list(
    order = 6,
    ui_constraint = "increasing"
  ),
  distribution = "Normal",
  params = list(
    support_min = 5,
    support_max = 50,
    bound_min = 0,
    bound_max = Inf
  )
)

# Boston Stratified
defaults_stratified <- list(
  bernstein = list(
    order = 6
  ),
  distribution = "Normal",
  params = list(
    support_min = 10,
    support_max = 40,
    bound_min = 0,
    bound_max = Inf
  )
)

# Boston Conditional
defaults_conditional <- list(
  bernstein = list(
    order = 6
  ),
  distribution = "Normal"
)

# Wine
defaults_cat <- list(
  method = "logistic"
)

# DVC (Deer-Vehicle Collisions)
defaults_count <- list(
  order = 6,
  method = "cloglog",
  log_first = TRUE
)

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  
  ## HTML appearance ------------------------------------------------------------
  
  # Load MathJax to render mathematical equations
  tags$head(tags$script(src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")),
  
  # Custom CSS style
  tags$head(
    tags$style(HTML("
      .navbar { margin-bottom: 0; }
      .main-content { 
        padding: 20px; 
        min-height: 100vh;
      }
      .row {
        display: flex !important;
        flex-wrap: nowrap !important;
      }
      .sidebar-panel { 
        background-color: #F5F5F5; 
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-right: 20px;
        height: fit-content;
        position: sticky;
        top: 20px;
        flex: 0 0 33.333333% !important;
        max-width: 33.333333% !important;
      }
      .content-panel {
        padding-left: 20px;
        flex: 0 0 66.666667% !important;
        max-width: 66.666667% !important;
      }
      .nav-tabs > li > a {
        padding: 10px 20px;
        margin-right: 5px;
      }
      .reset-button {
        display: flex;
        text-align: left;
        background-color: white;
        border-color: #dedede;
        color: black;
        margin-top: 15px;
        margin-bottom: 5px
      }
      .reset-button:hover {
        background-color: #dedede;
        border-color: #dbdbdb;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
      }
      .popover {
      max-width: 500px !important;
      width: 500px;
      }
      .data-section {
        margin-top: 30px;
        padding: 20px;
        background-color: #F5F5F5;
        border-radius: 8px;
        border: 1px solid #dee2e6;
      }
      @media (max-width: 768px) {
        .row {
          flex-wrap: wrap !important;
        }
        .content-panel, .sidebar-panel {
          flex: 0 0 100% !important;
          max-width: 100% !important;
        }
        .sidebar-panel {
          margin-right: 0;
          margin-top: 20px;
          position: relative !important
        }
      }
    "))
  ),
  
  ## Tabs ----------------------------------------------------------------------
  
  navbarPage(
    title = "Interactive Transformation Models",
    id = "main_nav",
    
    ### Home tab ----
    tabPanel(
      "Home", 
      value = "home",
      div(class = "main-content",
          h3("Welcome!"),
          p("This application is designed to help you understand transformation models as defined by", a("Hothorn, Möst & Bühlmann, 2018.", href = "https://doi.org/10.1111/sjos.12291"), br(),
            br(),
            "The purpose of a transformation model is to predict the distribution of a response \\(Y\\).", br(), "Increasingly complex transformation models for a continuous response are presented on their respective page:"
          ),
          tags$ul(
            tags$li("The unconditional case, where the distribution of \\(Y\\) is modeled without involving any predictor."),
            tags$li("The linear case, where predictors \\(X\\) are introduced in the model as a linear shift term with a fixed effect."),
            tags$li("The stratified linear case, where each stratum gets a different transformation but the regession coefficients of the shift term stay the same."),
            tags$li("The conditional case, where  the distribution of \\(Y\\) is modeled fully interacting with \\(X\\). The predictors no longer have a fixed effect, but rather an effect that can vary depending on the response.")
          ),
          p("Additionaly, transformation models for other types of response variables are presented:"),
          tags$ul(
            tags$li("Categorical response variable (unconditional case)."),
            tags$li("Count response variable.")
          ),
          br(),
          h4("Features"),
          p("I recommend opening the application in full-screen.", br(),
            br(),
            "On each model's page, a transformation model is fitted to a pre-loaded dataset. You can display the dataset and information about it at the bottom of the page.", br(),
            br(),
            "In the left-side menu, the model status displays the current parameters. Below, you can change some parameters of the model, which is instantly fitted again. This might take a few seconds.", br(),
            br(),
            "In the centre of the page, a summary of the currently fitted model and several plots are shown. The plots are updated at the same time as the model.", br(),
            br(),
            "On the last page, you can build a transformation model adapted to your specific needs. Describe your data, and obtain R code ready to be copied into your environment. This feature is limited at the moment."),
          br(),
          h4("Acknowledgements"),
          p("This application was created as a Master Thesis project in Applied Information and Data Science at Lucerne University of Applied Sciences and Arts."),
          p("I would like to express my sincere gratitude to the people who contributed to its developement:"),
          tags$ul(
            tags$li(a("Dr. Luisa Barbanti", href = "https://orcid.org/0000-0001-5352-5802"), "for her kind and insightful guidance throughout this project."),
            tags$li(a("Nisia Trisconi", href = "https://orcid.org/0009-0004-2765-0801"), "for co-supervising the thesis and testing the application."),
            tags$li(a("Dr. Torsten Hothorn", href = "https://orcid.org/0000-0001-8301-0471"), "for providing ideas of what to implement in the application, and testing it."),
            tags$li(a("Dr. Sandra Siegfried", href = "https://orcid.org/0000-0002-7312-1001"), "for testing the application."),
            tags$li(a("Dr. Balint Tamasi", href = "https://orcid.org/0000-0002-2629-7362"), "for testing the application."),
            tags$li(a("Dr. Lucas Kook", href = "https://orcid.org/0000-0002-7546-7356"), "for providing the code of the categorical plots originally found in", a("Kook et al., 2020.", href = 
                                                                                                                                                                    "https://doi.org/10.48550/arXiv.2010.08376"))
          ),
          br(),
          h4("Copyrights and Reproducibility"),
          p(HTML('This work is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY-NC-SA 4.0</a><img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;"><img src="https://mirrors.creativecommons.org/presskit/icons/by.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;"><img src="https://mirrors.creativecommons.org/presskit/icons/nc.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;"><img src="https://mirrors.creativecommons.org/presskit/icons/sa.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;">')),
          p("Main packages versions:"),
          tags$ul(
            tags$li(a("mlt", href = "https://cran.r-project.org/package=mlt"), "(T. Hothorn, 2025): 1.7-1"),
            tags$li(a("tram", href = "https://cran.r-project.org/package=tram"), "(T. Hothorn, L. Barbanti, S. Siegfried, L. Kook, 2025): 1.2-5"),
            tags$li(a("cotram", href = "https://cran.r-project.org/package=cotram"), "(S. Siegfried, L. Barbanti, T. Hothorn, 2025): 0.5-3"),
            tags$li(a("shiny", href = "https://cran.r-project.org/package=shiny"), "(W. Chang, J. Cheng, JJ. Allaire, C. Sievert, B. Schloerke, G. Aden-Buie, Y. Xie, J. Allen, J. McPherson, A. Dipert, B.Borges, 2025): 1.11.1"),
            tags$li(a("ggplot2", href = "https://cran.r-project.org/package=ggplot2"), "(H. Wickham, W. Chang, L. Henry, T. Pedersen, K. Takahashi, C. Wilke, K. Woo, H. Yutani, D. Dunnington, T. van den Brand, 2025): 4.0.0")
          ),
          p("GitHub directory:", a("https://github.com/jugwen/interactive-transformation-models")),
          p("Contact: Gwen Junod - gwen.junod@gmail.com"),
          tags$footer(
            style = "margin-top: 30px; text-align:center; font-size:0.9em; color: #777;",
            paste("App last modified:", format(last_modified, "%d.%m.%Y"))
          ),
      )
    ),
    
    ### Old Faithful tab ----
    tabPanel(
      "Unconditional Transformation Model", 
      value = "faithful",
      div(class = "main-content",
          h3("Unconditional Transformation Model"),
          p("In the unconditional case, the distribution is defined by a transformation function \\(h(y)\\) and a distribution function \\(F_Z\\), so we can write \\(\\mathbb{P}(Y \\leq y) = F_Z(h(y))\\).",
            br(), br(),
            "The transformation function \\(h(y)\\) is parameterised using a basis function \\(a\\), so we can write \\(h(y) = a(y)^\\top \\theta\\) where \\(\\theta\\) denotes parameters to be estimated. So, $$ \\mathbb{P}(Y \\leq y) = F_Z(h(y)) = F_Z(a(y)^\\top \\theta) $$
            To specify a transformation model, we must define the basis function \\(a\\), choose the link function \\(F_Z\\), and estimate the parameter vector \\(\\theta\\).",
            br(),
            tags$ul(
              tags$li("\\(a\\) is a vector of Bernstein polynomials of order \\(M\\) that must be defined on an interval corresponding to the range of \\(Y\\). To do so, a numeric variable representing \\(Y\\) is created. The author of the", em("mlt"), "package recommends choosing \\(M\\) between 5 and 10."),
              tags$li("\\(F_Z\\) is a link function that defines the distribution to transfom \\(Y\\) to. It can be chosen freely and influences the interpretation of the regression coefficients."),
              tags$li("\\(\\theta\\) is the vector of parameters estimated by the model depending on \\(a\\) and \\(F_Z\\).")
            ),
            p("The resulting transformation model can be fitted to the data."),
          ),
          br(),
          h4("Interactive Model"),
          p("A case of unconditional transformation model, more precisely a continuous model for a continuous response, is fitted to the Old Faithful dataset with the", em("mlt"), "package."),
          p("In this model,", em("waiting"), "is defined as the response variable."),
          p(strong("Bernstein Basis"), br(),
            "If you change the Bernstein Basis to a lower order, the first mode is less or even not represented in the PDF plot. You could set the order to 1 and increase it 1 by 1 with the arrow. The first mode slowly forms as the model captures more complexity. If you keep increasing the order, the modeled density stays stable until around 15."),
          p(strong("Distribution"), br(),
            "The distribution parameter defines \\(F_Z\\). It is the distribution we want to transform \\(Y\\) to. For unconditional transformation models, that choice is not really important since there are no coefficients to interpret. However, the Bernstein basis order must be large enough so that the model captures enough complexity to estimate a correct shape."),
          p(strong("Numeric Variable"), br(),
            "A transformation model is parametrised without seeing the data. Instead, a numeric variable representing the response variable is defined for the Bernstein basis. This is the only reference to the data before the fitting.", br(), "The", em("support"), "argument represents the range of the observed response, so from the smallest to the largest response value in the dataset. The", em("bounds"), "argument specifies the range of all possible values for the response variable. Theoretically and generally, a duration such as the waiting time variable can only be positive (there is no such thing as a negative duration), and can be infinitely large (there is no such thing as a too long duration)."),
          fluidRow(
            #### Left sidebar ----
            column(4, class = "sidebar-panel",
                   h4("Model Status"),
                   verbatimTextOutput("model_status_f", placeholder = TRUE),
                   
                   hr(),
                   
                   h4("Bernstein Basis", style = "color: #007bff;"),
                   numericInput("order_f", "Order (\\(M\\)):",
                                value = defaults_faithful$bernstein$order,
                                min = 1),
                   p(em("Must be an integer >= 1")),
                   
                   # Reset button for Bernstein Basis Parameters
                   actionButton("reset_bernstein_f", "Reset Bernstein Basis", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   h4("Model Options", style = "color: #007bff;"),
                   selectInput("distribution_f", "Distribution (\\(F_Z\\)):",
                               choices = c("Normal", "Logistic", "MinExtrVal",
                                           "MaxExtrVal"),
                               selected = defaults_faithful$distribution),
                   
                   # Reset button for Model Options
                   actionButton("reset_model_options_f", "Reset Model Options", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   h4("Numeric Variable", style = "color: #007bff; margin-top: 0;"),
                   p(em("The support range must be contained into the bounds limits")),
                   numericInput("support_min_f", "Support Min:",
                                value = defaults_faithful$params$support_min),
                   numericInput("support_max_f", "Support Max:",
                                value = defaults_faithful$params$support_max),
                   
                   useShinyjs(), # To grey out parameter choices  
                   radioButtons("bound_min_inf_f", "Lower Bound:",
                                choices = c("-Inf", "Numeric input"),
                                selected = defaults_faithful$params$bound_min_choice,
                                inline = TRUE
                   ),
                   numericInput("bound_min_num_f", "Enter value:",
                                value = defaults_faithful$params$bound_min_num),
                   
                   radioButtons("bound_max_inf_f", "Upper Bound:",
                                choices = c("Inf", "Numeric input"),
                                selected = defaults_faithful$params$bound_max_choice,
                                inline = TRUE
                   ),
                   
                   numericInput("bound_max_num_f", "Enter value:",
                                value = defaults_faithful$params$bound_max_num),
                   
                   # Reset button for Parameter Configuration
                   actionButton("reset_params_config_f", "Reset Numeric Variable", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   
                   # Show data
                   checkboxInput("show_data_f", "Show Dataset", value = FALSE)
            ),
            
            #### Main plot area ----
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Fitted Model Summary"),
                     verbatimTextOutput("model_summary_f")
                   ),
                   wellPanel(
                     h4("Baseline Transformation Function"),
                     p("Baseline transformation \\(h(y)\\) estimated by the model.
                       It is the transformation applied to the response variable
                       to make it behave like the chosen distribution \\(F_Z\\)."),
                     plotOutput("transformation_plot_f", height = "300px")
                   ),                   
                   wellPanel(
                     h4("Probability Density Function"),
                     p("Likelihood for the response variable to have a certain value."),
                     plotOutput("density_plot_f", height = "300px")
                   ),
                   wellPanel(
                     h4("Cumulative Distribution Function"),
                     p("Area under the PDF  curve at x."),
                     plotOutput("distribution_plot_f", height = "300px")
                   )
            )
          )
      ),
      
      #### Data section ----
      conditionalPanel(
        condition = "input.show_data_f == true",
        div(class = "data-section",
            h4("Dataset"),
            p(a("Old Faithful Geyser Data",
                href = "https://cran.r-project.org/doc/manuals/r-patched/packages/datasets/refman/datasets.html#faithful",
                title = "Source"),
              "— contains 272 observations on 2 variables of the Old Faithful geyser in Yellowstone National Park."),
            p(strong("eruptions"), "— duration of an eruption in mins", br(),
              strong("waiting"), "— time until the next eruption in mins"),
            DT::dataTableOutput("data_table_f"),
            
            br(),
            h4("Summary"),
            verbatimTextOutput("summary_f")
        )
      ) 
    ),
    
    ### Boston Linear tab ----
    tabPanel(
      "Linear Transformation Model", 
      value = "linear",
      div(class = "main-content",
          h3("Linear Transformation Model"),
          p("In the linear case, predictors are introduced in the model as linear shift terms. In transformation models, predictors do not influence the slope, as they would in standard regression, but they shift the entire distribution along the response scale. Note that 'linear' refers to the way the predictors enter the model, not to the transformation function. The transformation itself can be (and usually is) non-linear.",
            br(), br(),
            "We introduce \\(X\\) in the formula by writing \\(\\mathbb{P}(Y \\leq y | X = x) = F_Z(h(y | x)) = F_Z(h_Y(y)-\\tilde{x}^\\top \\beta)\\), with \\(\\beta\\) being estimated coefficients. Put simply, we want to find the distribution of \\(Y\\), given \\(X = x \\).
            As for the unconditional case, the transformation model is defined by \\(a\\), \\(F_Z\\) and \\(\\theta\\)."
          ),
          br(),
          h4("Interactive Model"),
          p("A case of linear transformation model is fitted to the BostonHousing2 dataset with the", em("mlt"), "package."),
          p("In this model,", em("cmedv"), "is defined as the response variable depending on the other variables.", em("medv"), "is not included as predictor as it is collinear with", em("cmedv"), ".", br(),
            "Note that", em("cmedv"), "is right-censored, meaning that the maximum value 50 doesn't mean '= 50', but '>= 50'. This is not taken into account in the model on this page as it has more of a demonstrative purpose, but in a real-life scenario, censoring of the data should be addressed to obtain a model representing the data more accurately."),
          p(strong("Bernstein Basis"), br(),
            "As stated above, the transformation function \\(h(y)\\) is usually non-linear. You can force it to be linear by setting the Berstein basis order to 1. If you also set \\(F_Z\\) to Normal, the resulting transformation model is equivalent to a normal linear regression model. That can be confirmed by looking at the quantile plots at the bottom of the page."),
          p(strong("Distribution"), br(),
            "The choice of the link function is now important because it defines the scale on which to interpret regression coefficients. Still, we can choose any \\(F_Z\\) that interests us."),
          p(strong("Numeric Variable"), br(),
            "The numeric variable is not interactive here. You can still see the values of", em("support"), "and", em("bounds"), "in the model status."),
          p(strong("Predictors"), br(),
            "You can choose the predictor variables to include in the model. Each predictor has its own effect on the distribution, but all effects are summed into a single linear predictor term."),
          fluidRow(
            #### Left sidebar ----
            column(4, class = "sidebar-panel",
                   h4("Model Status"),
                   verbatimTextOutput("model_status_lin", placeholder = TRUE),
                   
                   hr(),
                   h4("Bernstein Basis", style = "color: #007bff;"),
                   numericInput("order_lin", "Order (\\(M\\)):",
                                value = defaults_linear$bernstein$order,
                                min = 1),
                   p(em("Must be an integer >= 1")),
                   
                   # Reset button for Bernstein Basis Parameters
                   actionButton("reset_bernstein_lin", "Reset Bernstein Basis", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   h4("Model Options", style = "color: #007bff;"),
                   selectInput("distribution_lin", "Distribution (\\(F_Z\\)):",
                               choices = c("Normal", "Logistic", "MinExtrVal",
                                           "MaxExtrVal"),
                               selected = defaults_linear$distribution),
                   
                   # Reset button for Model Options
                   actionButton("reset_model_options_lin", "Reset Model Options", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   h4("Predictors", style = "color: #007bff;"),
                   checkboxGroupInput("vars_lin", "Select predictors to include:",
                                      choices = c("crim", "zn", "indus", "chas",
                                                  "nox", "rm", "age", "dis",
                                                  "rad", "tax", "ptratio", "b",
                                                  "lstat"),
                                      selected = c("crim", "zn", "indus", "chas",
                                                   "nox", "rm", "age", "dis",
                                                   "rad", "tax", "ptratio", "b",
                                                   "lstat")),
                   
                   # Select all button for Predictors
                   actionButton("select_vars_lin", "Select All Predictors", 
                                class = "reset-button btn-xs"),
                   # Deselect all button for Predictors
                   actionButton("deselect_vars_lin", "Deselect All Predictors", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   # Show data
                   checkboxInput("show_data_lin", "Show Dataset", value = FALSE)
            ),
            
            #### Main plot area ----
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Fitted Model Summary"),
                     verbatimTextOutput("model_summary_lin"),
                     p("When \\(F_Z\\) is Normal, the coefficients can be interpreted as effects on the transformation, in standard deviation units. A one-unit increase in predictor \\(x\\) shifts \\(h(Y)\\) by one standard deviation on the normal scale.
                       So with the default parameters of this page, we can see that", em("nox"), "is the predictor that has the largest effect on the transformation. When", em("nox"), "increases by 1, the transformation function shifts by 4.77 standard deviations on the normal scale. We can also see that", em("zn"), ", ", em("age"), ", ", em("tax"), "and", em("b"), "have very small effects on the estimated transformation."),
                     p("Another way to look at the coefficients is through probability. The predictor", em("rm"), "is the number of rooms. For each additional room in a house, the transformed median price shifts by -0.48 standard deviation on the normal scale. If we compare the probabilities of a 2-room house and a 4-room house to have a median value of 20'000 or less, we write:", br(),
                       "\\(\\mathbb{P}(cmedv ≤ 20) = Φ(h(20) - (-0.48 × 2)) = Φ(h(20) + 0.96)\\)", br(), "and", br(),
                       "\\(\\mathbb{P}(cmedv ≤ 20) = Φ(h(20) - (-0.48 × 4)) = Φ(h(20) + 1.92)\\)", br(),
                       "The 4-room house shifts the distribution more (1.92 > 0.96), making it less likely that 4-room houses have a median value of 20'000 or less compared to 2-room houses."),
                     p("When \\(F_Z\\) is Logistic, the coefficients can be interpreted as odds ratio. For a one-unit increase in the predictor, the exponential of its coefficient multiplies the probability of \\(Y \\leq y\\).", em("lstat"), "is the percentage of lower status people in the population. The exponential of 0.28 is about 1.32. This means that for an increase of 1 percent in the predictor, the odds of any median price being under any threshold is multiplied by 1.32. This makes sense, as the poorer an area, the lower the house prices."),
                     p("Explanations for the other link functions have yet to be implemented.")
                   ),
                   wellPanel(
                     h4("Baseline Transformation Function"),
                     p("Baseline transformation \\(h(y)\\) estimated by the model. It is the transformation applied to the response variable to make it behave like the chosen distribution \\(F_Z\\)."),
                     plotOutput("transformation_plot_lin", height = "300px")
                   ),                   
                   wellPanel(
                     h4("Probability Density Function"),
                     p("A transformation model estimates the whole distribution for each observation. The PDF and CDF plots are created by predicting for the 506 observations of the dataset at 200 equally spaced values in the range of", em("cmedv"), "(0,50)."),
                     plotOutput("density_plot_lin", height = "300px"),
                     p(),
                     p("With the default parameters of this page, each line's position on the \\(x\\) axis is influenced by the 13 predictor values of that specific observation. If you deselect all predictors but a discrete one, there will only be as many lines (distributions) as there are levels of that predictor. For example, the", em("rad"), "predictor can only take one of nine values. If you inlcude it alone in the model, every observation with the same", em("rad"), "values will be predicted the same distribution, resulting in only nine distribution functions. This illustrates how the distribution depends on \\(X\\).")
                   ),
                   wellPanel(
                     h4("Cumulative Distribution Function"),
                     p("Area under the PDF  curve at x."),
                     plotOutput("distribution_plot_lin", height = "300px")
                   ),
                   fluidRow(
                     column(6,
                            wellPanel(
                              h4("Quantile Distribution - Fitted Model"),
                              p("Each observation is plotted according to its observed", em("cmedv"), "(y axis) and linear predictor (x axis). The confidence bands indicate the probability of having a home value at or below the observed value, given that observation's linear predictor.",
                                tags$sup(
                                  icon("circle-question", class = "text-info", style = "cursor: pointer;"),
                                  id = "info_icon_qplot_lin"
                                )),
                              bsPopover("info_icon_qplot_lin", 
                                        title = "Interpretation of the quantile plot",
                                        content = "For example, a datapoint in the (0.5, 0.6) confidence band means that given that observation’s predictor values, the model predicts a 50-60% probability of observing a home value at or below the actual observed value. Following that logic, houses that are in the darker confidence bands and above can be considered too expensive given their features, whereas houses in the lighter confidence bands and below are cheap given their features.",
                                        placement = "right", 
                                        trigger = "click"),
                              plotOutput("quantile_plot_lin", height = "300px"),
                              p(),
                              p("If you set the Bernstein basis order to 1 and \\(F_Z\\) to Normal, the transformation model is equivalent to the normal linear model on the right.", br(),
                                "A transformation model with a higher order captures the larger values of", em("cmedv"), "better.")
                            )
                     ),
                     column(6,
                            wellPanel(
                              h4("Quantile Distribution - Normal Linear Model"),
                              p("A normal linear model is fitted with the same predictors as the transformation model. The interpretation of this plot is equivalent to that on the left.", br(), br()),
                              plotOutput("quantile_plot_norm", height = "300px"),
                              p(),
                              p("This model fitted with ", em("lm()"), " underestimates larger values of", em("cmedv"), "as they are all in the darkest confidence band or above.",
                                tags$sup(
                                  icon("circle-question", class = "text-info", style = "cursor: pointer;"),
                                  id = "info_icon_qplot_norm"
                                )),
                              bsPopover("info_icon_qplot_norm", 
                                        title = "Explanation",
                                        content = "If we take one of the points near the upper right corner, so a house that has an observed value of about 49’000 $, and go down vertically to the middle of the confidence bands, we can say “this house with an observed value of 49’000 $ should instead have a value of about 40’000 $ to be in the norm of other houses with the same features”. Now if we look at the left plot for the default model on this page, we see that most of those same observations in the upper right corner are captured in the middle of the confidence bands, so their value is considered in the norm given their features.",
                                        placement = "right", 
                                        trigger = "click")
                            )
                     )
                   )
            )
          )
      ),
      
      #### Data section ----
      conditionalPanel(
        condition = "input.show_data_lin == true",
        div(class = "data-section",
            h4("Dataset"),
            p(a("BostonHousing2",
                href = "https://search.r-project.org/CRAN/refmans/mlbench/html/BostonHousing.html",
                title = "Source"),
              "— contains 506 observations on 19 variables of the 1970 United States census in the Boston area."),
            p(strong("town"), "— name of town", br(),
              strong("tract"), "— census tract", br(),
              strong("lon"), "— longitude of census tract", br(),
              strong("lat"), "— latitude of census tract", br(),
              strong("medv"), "— median value of owner-occupied homes in USD 1000's", br(),
              strong("cmedv"), "— corrected median value of owner-occupied homes in USD 1000's", br(),
              strong("crim"), "— per capita crime rate by town", br(),
              strong("zn"), "— proportion of residential land zoned for lots over 25,000 sq.ft", br(),
              strong("indus"), "— proportion of non-retail business acres per town", br(),
              strong("chas"), "— Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)", br(),
              strong("nox"), "— nitric oxides concentration (parts per 10 million)", br(),
              strong("rm"), "— average number of rooms per dwelling", br(),
              strong("age"), "— proportion of owner-occupied units built prior to 1940", br(),
              strong("dis"), "— weighted distances to five Boston employment centres", br(),
              strong("rad"), "— index of accessibility to radial highways", br(),
              strong("tax"), "— full-value property-tax rate per USD 10,000", br(),
              strong("ptratio"), "— pupil-teacher ratio by town", br(),
              strong("b"), "— \\(1000(B-0.63)^2\\) where \\(B\\) is the proportion of blacks by town", br(),
              strong("lstat"), "— percentage of lower status of the population"),
            DT::dataTableOutput("data_table_lin"),
            
            br(),
            h4("Summary"),
            verbatimTextOutput("summary_lin")
        )
      ) 
    ),
    
    ### Boston Stratified tab ----
    tabPanel(
      "Stratified Linear Transformation Model", 
      value = "stratified",
      div(class = "main-content",
          h3("Stratified Linear Transformation Model"),
          p("In a stratified transformation model, one of the predictors is defined as strata, and a transformation function is estimated for each level of the strata. The regression coefficients of the shift term stay constant across all levels.", br(), br(),
            "We introduce the strata \\(S\\) in the formula by writing \\(\\mathbb{P}(Y \\leq y | X = x, S = s) = F_Z(h(y | x, s)) = F_Z(h_Y(y | s)-\\tilde{x}^\\top \\beta)\\)."),
          br(),
          h4("Interactive Model"),
          p("A case of stratified linear transformation model is fitted to the BostonHousing2 datastet with the", em("tram"), "package."),
          p("In this model,", em("cmedv"), "is defined as the response variable depending on the other variables.", em("medv"), "is not included as predictor as it is collinear with", em("cmedv"), ". The chosen strata variable", em("chas"), "is a dummy variable that indicates proximity with the Charles River.", br(),
            "Note that", em("cmedv"), "is right-censored, meaning that the maximum value 50 doesn't mean '= 50', but '>= 50'. This is not taken into account in the model on this page as it has more of a demonstrative purpose, but in a real-life scenario, censoring of the data should be addressed to obtain a model representing the data more accurately."),
          p(strong("Bernstein Basis"), br(),
            "The Bernstein basis properties are the same as the ones described in the Unconditional and Linear pages."),
          p(strong("Distribution"), br(),
            "That part of the model is not interactive on this page. It is set to Normal."),
          p(strong("Numeric Variable"), br(),
            "With the", em("tram"), "package, fitting is more straightforward and there is no need to create a numeric variable."),
          p(strong("Predictors"), br(),
            "You can choose the predictor variables to include in the model. Each predictor has its own effect on the distribution, but all effects are summed into a single linear predictor term."),
          fluidRow(
            #### Left sidebar ----
            column(4, class = "sidebar-panel",
                   h4("Model Status"),
                   verbatimTextOutput("model_status_strat", placeholder = TRUE),
                   
                   hr(),
                   h4("Bernstein Basis", style = "color: #007bff;"),
                   numericInput("order_strat", "Order (\\(M\\)):",
                                value = defaults_stratified$bernstein$order,
                                min = 1),
                   p(em("Must be an integer >= 1")),
                   
                   # Reset button for Bernstein Basis Parameters
                   actionButton("reset_bernstein_strat", "Reset Bernstein Basis", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   h4("Predictors", style = "color: #007bff;"),
                   checkboxGroupInput("vars_strat", "Select predictors to include:",
                                      choices = c("crim", "zn", "indus", "nox", 
                                                  "rm", "age", "dis", "rad",
                                                  "tax", "ptratio", "b", "lstat"),
                                      selected = c("crim", "zn", "indus", "nox", 
                                                   "rm", "age", "dis", "rad",
                                                   "tax", "ptratio", "b", "lstat")),
                   
                   # Select all button for Predictors
                   actionButton("select_vars_strat", "Select All Predictors", 
                                class = "reset-button btn-xs"),
                   # Deselect all button for Predictors
                   actionButton("deselect_vars_strat", "Deselect All Predictors", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   # Show data
                   checkboxInput("show_data_strat", "Show Dataset",
                                 value = FALSE)
            ),
            
            #### Main plot area ----
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Fitted Model Summary"),
                     verbatimTextOutput("model_summary_strat"),
                     p("The coefficients can be interpreted as in the linear case.")
                   ),
                   wellPanel(
                     h4("Baseline Transformation Function"),
                     p("A transformation function is estimated independently for each level of the strata."),
                     plotOutput("transformation_plot_strat", height = "300px")
                   ),                   
                   wellPanel(
                     h4("Probability Density Function"),
                     p("The PDF and CDF plots are created by predicting for the 506 observations of the dataset at 200 equally spaced values in the range of cmedv (0,50)."),
                     plotOutput("density_plot_strat", height = "300px")
                   ),
                   wellPanel(
                     h4("Cumulative Distribution Function"),
                     p("Area under the PDF  curve at x."),
                     plotOutput("distribution_plot_strat", height = "300px")
                   ),
                   wellPanel(
                     h4("Quantile Distribution for each stratum"),
                     p("Each observation is plotted according to its observed", em("cmedv"), "(y axis) and linear predictor (x axis). The confidence bands indicate the probability of having a home value at or below the observed value, given that observation's linear predictor."),
                     plotOutput("quantile_plot_strat", height = "300px")
                   )
            )
          )
      ),
      
      #### Data section ----
      conditionalPanel(
        condition = "input.show_data_strat == true",
        div(class = "data-section",
            h4("Dataset"),
            p(a("BostonHousing2",
                href = "https://search.r-project.org/CRAN/refmans/mlbench/html/BostonHousing.html",
                title = "Source"),
              "— contains 506 observations on 19 variables of the 1970 United States census in the Boston area."),
            p(strong("town"), "— name of town", br(),
              strong("tract"), "— census tract", br(),
              strong("lon"), "— longitude of census tract", br(),
              strong("lat"), "— latitude of census tract", br(),
              strong("medv"), "— median value of owner-occupied homes in USD 1000's", br(),
              strong("cmedv"), "— corrected median value of owner-occupied homes in USD 1000's", br(),
              strong("crim"), "— per capita crime rate by town", br(),
              strong("zn"), "— proportion of residential land zoned for lots over 25,000 sq.ft", br(),
              strong("indus"), "— proportion of non-retail business acres per town", br(),
              strong("chas"), "— Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)", br(),
              strong("nox"), "— nitric oxides concentration (parts per 10 million)", br(),
              strong("rm"), "— average number of rooms per dwelling", br(),
              strong("age"), "— proportion of owner-occupied units built prior to 1940", br(),
              strong("dis"), "— weighted distances to five Boston employment centres", br(),
              strong("rad"), "— index of accessibility to radial highways", br(),
              strong("tax"), "— full-value property-tax rate per USD 10,000", br(),
              strong("ptratio"), "— pupil-teacher ratio by town", br(),
              strong("b"), "— \\(1000(B-0.63)^2\\) where \\(B\\) is the proportion of blacks by town", br(),
              strong("lstat"), "— percentage of lower status of the population"),
            DT::dataTableOutput("data_table_strat"),
            
            br(),
            h4("Summary"),
            verbatimTextOutput("summary_strat")
        )
      ) 
    ),
    
    ### Boston Conditional tab ----
    tabPanel(
      "Conditional Transformation Model", 
      value = "conditional",
      div(class = "main-content",
          h3("Conditional Transformation Model"),
          p("A (fully) conditional transformation model is more flexible in estimating the transformation: the transformation can vary depending on the response. So far, one regression coefficient was estimated per predictor. Now, each element of the Bernstein basis (number of elements = \\(M\\) + 1) gets a coefficient for each of the predictors."),
          br(),
          h4("Interactive Model"),
          p("A case of conditional transformation model can be fitted to the BostonHousing2 datastet with the", em("tram"), "package."),
          p("In this model,", em("cmedv"), "is defined as the response variable depending on the other variables.", em("medv"), "is not included as predictor as it is collinear with", em("cmedv"), ".", br(),
            "Note that", em("cmedv"), "is right-censored, meaning that the maximum value 50 doesn't mean '= 50', but '>= 50'. This is not taken into account in the model on this page as it has more of a demonstrative purpose, but in a real-life scenario, censoring of the data should be addressed to obtain a model representing the data more accurately."),
          p(strong("Bernstein Basis"), br(),
            "The Bernstein basis properties are the same as the ones described in the Unconditional and Linear pages."),
          p(strong("Distribution"), br(),
            "That part of the model is not interactive on this page. It is set to Normal."),
          p(strong("Predictors"), br(),
            "You can choose the predictor variables to include in the model. Each predictor has its own effect on the distribution, but all effects are summed into a single linear predictor term."),
          p(strong("Plots"), br(),
            "This feature is not developped yet."),
          fluidRow(
            #### Left sidebar ----
            column(4, class = "sidebar-panel",
                   p("WARNING: fitting this model may take several minutes. The rest of the application is not usable during that time."),
                   actionButton("fit_model_cond", "Fit Model", 
                                class = "btn-primary btn-lg", 
                                style = "width: 100%; margin-bottom: 10px;"),
                   
                   hr(),
                   h4("Model Status"),
                   verbatimTextOutput("model_status_cond", placeholder = TRUE),
                   
                   hr(),
                   h4("Bernstein Basis", style = "color:#007bff;"),
                   numericInput("order_cond", "Order (\\(M\\)):",
                                value = defaults_conditional$bernstein$order,
                                min = 1),
                   p(em("Must be an integer >= 1")),
                   
                   # Reset button for Bernstein Basis Parameters
                   actionButton("reset_bernstein_cond", "Reset Bernstein Basis", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   h4("Predictors", style = "color:#007bff;"),
                   checkboxGroupInput("vars_cond", "Select predictors to include:",
                                      choices = c("crim", "zn", "indus", "chas",
                                                  "nox", "rm", "age", "dis",
                                                  "rad", "tax", "ptratio", "b",
                                                  "lstat"),
                                      selected = c("crim", "zn", "indus", "chas",
                                                   "nox", "rm", "age", "dis",
                                                   "rad", "tax", "ptratio", "b",
                                                   "lstat")),
                   
                   # Select all button for Predictors
                   actionButton("select_vars_cond", "Select All Predictors", 
                                class = "reset-button btn-xs"),
                   # Deselect all button for Predictors
                   actionButton("deselect_vars_cond", "Deselect All Predictors", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   # Show data
                   checkboxInput("show_data_cond", "Show Dataset",
                                 value = FALSE)
            ),
            
            #### Main plot area ----
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Fitted Model Summary"),
                     verbatimTextOutput("model_summary_cond"),
                     p("For the default model on this page, there are 7 Bernstein elements, 13 predictors selected, and 1 intercept, which equals to 13 x (7 + 1) = 98 coefficients. This explains the longer fitting time of this model.")
                   )
            )
          )
      ),
      
      #### Data section ----
      conditionalPanel(
        condition = "input.show_data_cond == true",
        div(class = "data-section",
            h4("Dataset"),
            p(a("BostonHousing2",
                href = "https://search.r-project.org/CRAN/refmans/mlbench/html/BostonHousing.html",
                title = "Source"),
              "— contains 506 observations on 19 variables of the 1970 United States census in the Boston area."),
            p(strong("town"), "— name of town", br(),
              strong("tract"), "— census tract", br(),
              strong("lon"), "— longitude of census tract", br(),
              strong("lat"), "— latitude of census tract", br(),
              strong("medv"), "— median value of owner-occupied homes in USD 1000's", br(),
              strong("cmedv"), "— corrected median value of owner-occupied homes in USD 1000's", br(),
              strong("crim"), "— per capita crime rate by town", br(),
              strong("zn"), "— proportion of residential land zoned for lots over 25,000 sq.ft", br(),
              strong("indus"), "— proportion of non-retail business acres per town", br(),
              strong("chas"), "— Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)", br(),
              strong("nox"), "— nitric oxides concentration (parts per 10 million)", br(),
              strong("rm"), "— average number of rooms per dwelling", br(),
              strong("age"), "— proportion of owner-occupied units built prior to 1940", br(),
              strong("dis"), "— weighted distances to five Boston employment centres", br(),
              strong("rad"), "— index of accessibility to radial highways", br(),
              strong("tax"), "— full-value property-tax rate per USD 10,000", br(),
              strong("ptratio"), "— pupil-teacher ratio by town", br(),
              strong("b"), "— \\(1000(B-0.63)^2\\) where \\(B\\) is the proportion of blacks by town", br(),
              strong("lstat"), "— percentage of lower status of the population"),
            DT::dataTableOutput("data_table_cond"),
            
            br(),
            h4("Summary"),
            verbatimTextOutput("summary_cond")
        )
      ) 
    ),
    
    ### Wine Categorical tab ----
    tabPanel(
      "Categorical Transformation Model", 
      value = "categorical",
      div(class = "main-content",
          h3("Categorical Transformation Model"),
          p("Categorical transformation models deal with ordered discrete response variables. Here, only the unconditional case is presented.",
            br(), br(),
            "Explanations about how such a model is implemented should be added."),
          br(),
          h4("Model"),
          p("A case of unconditional categorical transformation model is fitted with the", em("tram"), "package."),
          p("In this model,", em("rating"), "is defined as the response variable."),
          p(strong("Link Function"), br(),
            "That part of the model is not interactive on this page. It is set to Logistic, and since it is an unconditional model, that choice is of little importance."),
          fluidRow(
            #### Left sidebar ----
            column(4, class = "sidebar-panel",
                   h4("Model Status"),
                   verbatimTextOutput("model_status_cat", placeholder = TRUE),
                   
                   hr(),
                   # Show data
                   checkboxInput("show_data_cat", "Show Dataset", value = FALSE)
            ),
            
            #### Main plot area ----
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Fitted Model Summary"),
                     verbatimTextOutput("model_summary_cat")
                   ),
                   wellPanel(
                     h4("Density and Distribution Functions"),
                     p("Plot A is the PDF, showing the probability for a response to belong to each category. For example, there is a 36% chance that a rating is 3.", br(), br(),
                       "Plot B is the CDF, so the probability for a response to belong to the observed category or any category below. For example, there is a 90% chance that a rating is 4 or below. The height of a step corresponds to the chance of belonging to that category, as depicted in the PDF. For example, the PDF shows that there is a 31% chance that a rating is 2. In the CDF, the corresponding step from 0.07 to 0.38 is 0.31.", br(), br(),
                       "Plots C and D show the PDF and CDF of the latent variable \\(Z\\), which is an unobserved continuous variable used in the computation of the model. The transformation function maps the discrete response variable to \\(Z\\). More precise information should be added here.", br(), "
                       These two plots can be read in parallel to A and B, because they depict the same relationship but from a different point of view. For example, the area under the curve in C corresponds to the probability in A, so we know that the area between \\(h_1\\) and \\(h_2\\) is 0.31 (value of category 2 in A)."),
                     plotOutput("plots1_cat", height = "500px")
                   ),
                   wellPanel(
                     h4("Transformation Function Mapping the Discrete Response Variable to the Latent Variable \\(Z\\)"),
                     p("This is another representation of the relationship between \\(Y\\) and \\(Z\\). Plot C is the PDF again, equivalent to plot A above.", br(),
                       "The density of \\(Y\\) (plot C) is mapped to the density of \\(Z\\) (plot A, equivalent to plot C above) through the transformation step function \\(h\\) (plot B)."),
                     plotOutput("plots2_cat", height = "500px")
                   )
            )
          )
      ),
      
      #### Data section ----
      conditionalPanel(
        condition = "input.show_data_cat == true",
        div(class = "data-section",
            h4("Dataset"),
            p(a("Bitterness of wine",
                href = "https://cran.r-project.org/web/packages/ordinal/refman/ordinal.html#wine",
                title = "Source"),
              "— dataframe containing 72 observations on 6 variables of a tasting experiment on the bitterness of wine."),
            p(strong("response"), "— scorings of wine bitterness on a 0-100 continuous scale", br(),
              strong("rating"), "— ordered factor with 5 levels; a grouped version of", em("response"), "", br(),
              strong("temp"), "— temperature during production as a factor with two levels", br(),
              strong("contact"), "— contact between juice and skins during production as a factor with two levels", br(),
              strong("bottle"), "— factor with eight levels", br(),
              strong("judge"), "— factor with nine levels"),
            DT::dataTableOutput("data_table_cat"),
            
            br(),
            h4("Summary"),
            verbatimTextOutput("summary_cat")
        )
      ) 
    ),
    
    ### Deer Count tab ----
    tabPanel(
      "Count Transformation Model", 
      value = "count",
      div(class = "main-content",
          h3("Count Transformation Model"),
          p("Count transformation models are specifically designed for count response variables.",
            br(), br(),
            "They are expressed by \\(F_{Y|X=x}(y | x) = \\mathbb{P}(Y \\leq y | x) = F(h(\\lfloor y \\rfloor) - x^\\top \\beta)\\), with \\(F\\) being the link function and \\(y\\) being rounded to the nearest integer."),
          br(),
          h4("Interactive Model"),
          p("A case of count transformation model is fitted with the", em("cotram"), "package."),
          p("In this model,", em("DVC"), "is defined as the response variable depending on all the other variables."),
          p(strong("Bernstein Basis"), br(),
            "The Bernstein basis is interactive in this model."),
          p(strong("Link Function"), br(),
            "The choice of the link function is important because it defines the scale on which to interpret regression coefficients. Still, we can choose any \\(F\\) that interests us."),
          p(strong("Log-first"), br(),
            "When it is set to TRUE, the model transforms the response with \\(log(y+1)\\) before the Berstein basis is applied. That changes the interpretation scale of the coefficients from the response scale to the log scale, meaning that the coefficients have a multiplicative effect."),
          fluidRow(
            #### Left sidebar ----
            column(4, class = "sidebar-panel",
                   h4("Model Status"),
                   verbatimTextOutput("model_status_count", placeholder = TRUE),
                   
                   hr(),
                   h4("Bernstein Basis", style = "color: #007bff;"),
                   numericInput("order_count", "Order (\\(M\\)):",
                                value = defaults_count$order, min = 1),
                   p(em("Must be an integer >= 1")),
                   
                   # Reset button for Bernstein Basis Parameters
                   actionButton("reset_bernstein_count", "Reset Bernstein Basis", 
                                class = "reset-button btn-xs"),
                   
                   hr(), 
                   h4("Model Options", style = "color: #007bff;"),
                   selectInput("method_count", "Link function (\\(F\\)):",
                               choices = c("logit", "cloglog", "loglog",
                                           "probit"),
                               selected = defaults_count$method),
                   
                   # Reset button for Method
                   actionButton("reset_method_count", "Reset Model Options", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   radioButtons("logfirst_count", "Log-first:",
                                choices = c(TRUE, FALSE),
                                selected = defaults_count$log_first,
                                inline = TRUE
                   ),
                   
                   # Reset button for Log-first
                   actionButton("reset_logfirst_count", "Reset Log-first", 
                                class = "reset-button btn-xs"),
                   
                   hr(),
                   # Show data
                   checkboxInput("show_data_count", "Show Dataset",
                                 value = FALSE)
            ),
            
            #### Main plot area ----
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Fitted Model Summary"),
                     verbatimTextOutput("model_summary_count"),
                     p("When the link function is cloglog, the linear predictor is interpreted as discrete hazard ratio. We can interpret the sign of the coefficients: if it is positive, there is a higher risk of having a collision compared to the baseline. For example, the baseline for", em("weekday"), "is Monday. All other weekdays, there is a higher risk of having a collision. However, in the weekend, there is a smaller risk of having a collision."),
                     p("Explanations for the other link functions have yet to be implemented.")
                     
                   ),
                   wellPanel(
                     h4("Hazard Ratio for the Year 2011"),
                     p("Evolution of the collision risk across a year, estimated for each day, with fixed effects."),
                     plotOutput("hazard_plot_count", height = "300px"),
                     p(),
                     p("The changes in the hazard ratio are relative to the baseline of January 1st, so a higher ratio means a higher risk of collision. For example, we see that there is a peak of collision risk in May with about 12.5 times more risk to have a collision than on January 1st.")
                   ),
                   wellPanel(
                     h4("Baseline Transformation Function"),
                     p("Baseline transformation \\(h(y)\\) estimated by the model. It is the transformation applied to the response variable to make it behave like the chosen distribution \\(F\\)."),
                     plotOutput("transformation_plot_count", height = "300px")
                   ),                   
                   wellPanel(
                     h4("Probability Density Function by Year"),
                     p("The PDF and CDF plots represent the isolated", em("year"), "effect on the collision count."),
                     plotOutput("density_plot_count", height = "300px"),
                   ),
                   wellPanel(
                     h4("Cumulative Distribution Function by Year"),
                     p("Area under the PDF curve at x."),
                     plotOutput("distribution_plot_count", height = "300px")
                   )
            )
          )
      ),
      
      #### Data section ----
      conditionalPanel(
        condition = "input.show_data_count == true",
        div(class = "data-section",
            h4("Dataset"),
            p(a("Deer-Vehicle Collisions",
                href = "https://zenodo.org/records/17179",
                title = "Source"),
              "preprocessed according to the", a(em("cotram"), "package vignette code",
                                                 href = "https://cran.r-project.org/web/packages/cotram/vignettes/cotram.R"), "(DVC-data and DVC-setup chunks) — time series containing 3'652 observations on 25 variables of collisions between roe deer and vehicles between 2002 and 2011 in Bavaria, Germany."),
            p(strong("day"), "— date", br(),
              strong("DVC"), "— number of deer-vehicle collisions that day", br(),
              strong("weekday"), "— day of the week", br(),
              strong("year"), "— year", br(),
              strong("time"), "— days since beginning (01-01-2002)", br(),
              strong("tvar1 - tvar20"), "— sine-cosine transformed times (allow modelling of periodic (yearly) effects)"),
            p(em("tvar"), "variables rounded to the fifth decimal in this view."),
            DT::dataTableOutput("data_table_count"),
            
            br(),
            h4("Summary"),
            verbatimTextOutput("summary_count")
        )
      ) 
    ),
    
    ### Model Builder tab ----
    tabPanel(
      "Model Builder", 
      value = "builder",
      div(class = "main-content",
          h3("Build a Transformation Model"),
          p("On this page, you can generate a made-to-measure transformation model. Define the parameters of the model in the left menu, generate the model's code, and copy it into your R environment. At the moment, this feature offers only limited options."),
          
          fluidRow(
            # Left sidebar with model specifications
            column(4, class = "sidebar-panel",
                   h4("Response Variable", style = "color: #007bff;"),
                   p("Describe your response variable \\(Y\\)."),
                   selectInput("response_type", "Type:",
                               choices = c("Continuous" = "continuous",
                                           "Ordered" = "ordered",
                                           "Count" = "count"),
                               selected = "continuous"),
                   
                   textInput("response_name", "Name:", value = "y"),
                   
                   conditionalPanel(
                     condition = "input.response_type == 'continuous'",
                     p(em("Support is the range of Y in your dataset.")),
                     numericInput("support_min_builder", "Support Min:",
                                  value = 1),
                     numericInput("support_max_builder", "Support Max:",
                                  value = 100),
                     p(em("Bounds is the range of all possible values for Y.")),
                     radioButtons("bound_min_type", "Lower Bound:",
                                  choices = c("-Inf" = "neg_inf",
                                              "Numeric Value" = "number"),
                                  selected = "number"),
                     conditionalPanel(
                       condition = "input.bound_min_type == 'number'",
                       numericInput("bound_min_builder", "Lower Bound Value:",
                                    value = 0)
                     ),
                     radioButtons("bound_max_type", "Upper Bound:",
                                  choices = c("Inf" = "pos_inf",
                                              "Numeric Value" = "number"),
                                  selected = "number"),
                     conditionalPanel(
                       condition = "input.bound_max_type == 'number'",
                       numericInput("bound_max_builder", "Upper Bound Value:",
                                    value = 300)
                     )
                   ),
                   
                   hr(),
                   h4("Distribution", style = "color: #007bff;"),
                   p("Decide the distribution \\(F_Z\\) you want to transform \\(Y\\) to. This will influence the interpretation of coefficients."),
                   uiOutput("link_function_ui"),
                   
                   hr(),
                   h4("Covariates", style = "color: #007bff;"),
                   p("Enter all the predictors you want to include and name them as in your dataset."),
                   actionButton("add_covariate", "Add Covariate", 
                                icon = icon("plus"),
                                class = "btn-success",
                                style = "margin-bottom: 15px;"),
                   uiOutput("covariate_inputs"),
                   
                   hr(),
                   conditionalPanel(
                     condition = "input.response_type == 'continuous'",
                     h4("Model Structure", style = "color: #007bff;"),
                     p("Define how the covariates interact."),
                     radioButtons("model_structure", "Variables interaction:",
                                  choices = c("Shifting terms only (linear effects)" = "shifting",
                                              "Stratified by one variable" = "stratified",
                                              "Conditional (all variables interact)" = "conditional"),
                                  selected = "shifting")
                   ),
                   
                   conditionalPanel(
                     condition = "input.response_type == 'ordered'",
                     h4("Model Structure", style = "color: #007bff;"),
                     p("Define how the covariates enter the model."),
                     radioButtons("model_structure_ordered", "Variables interaction:",
                                  choices = c("Shifting terms only" = "proportional",
                                              "Stratified by one variable" = "stratified"),
                                  selected = "proportional")
                   ),
                   
                   conditionalPanel(
                     condition = "(input.response_type == 'continuous' && input.model_structure == 'stratified') || (input.response_type == 'ordered' && input.model_structure_ordered == 'stratified')",
                     uiOutput("stratification_var_ui")
                   ),
                   
                   hr(),
                   conditionalPanel(
                     condition = "input.response_type == 'continuous' || input.response_type == 'count'",
                     h4("Bernstein Basis", style = "color: #007bff;"),
                     p("Choose how much complexity the model can capture. Usually, an order between 5 and 10 is a good compromise between flexibility and computing time."),
                     numericInput("builder_order", "Order:", 
                                  value = 6, min = 1),
                     conditionalPanel(
                       condition = "input.link_function == 'Lm' || input.link_function == 'BoxCox'",
                       p(em("To create a model equivalent to a normal linear regression model, set to 1."))
                     )
                   ),
                   
                   hr(),
                   actionButton("generate_code", "Generate Code", 
                                class = "btn-primary btn-lg", 
                                style = "width: 100%; font-weight: bold;")
            ),
            
            # Generated code
            column(8, class = "content-panel",
                   wellPanel(
                     h4("Your R Code"),
                     p("Copy the code below and replace the placeholder names:"),
                     verbatimTextOutput("generated_code")
                   ),
                   wellPanel(
                     h4("Placeholders"),
                     tags$ul(
                       tags$li(tags$code("my_data"), ": Your data frame name")
                     )
                   )
            )
          )
          
      )
    )
  )
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Old Faithful --------------------------------------------------------------
  
  ### Old Faithful reset buttons ----
  observeEvent(input$reset_params_config_f, {
    updateSliderInput(session, "support_min_f",
                      value = defaults_faithful$params$support_min)
    updateSliderInput(session, "support_max_f",
                      value = defaults_faithful$params$support_max)
    
    updateRadioButtons(session, "bound_min_inf_f",
                       selected = defaults_faithful$params$bound_min_choice)
    updateNumericInput(session, "bound_min_num_f",
                       value = defaults_faithful$params$bound_min_num)
    
    updateRadioButtons(session, "bound_max_inf_f",
                       selected = defaults_faithful$params$bound_max_choice)
    updateNumericInput(session, "bound_max_num_f",
                       value = defaults_faithful$params$bound_max_num)
  })
  
  observeEvent(input$reset_bernstein_f, {
    updateSliderInput(session, "order_f",
                      value = defaults_faithful$bernstein$order)
  })
  
  observeEvent(input$reset_model_options_f, {
    updateSelectInput(session, "distribution_f",
                      selected = defaults_faithful$distribution)
  })
  
  ### Old Faithful bounds ----
  observe({
    req(input$bound_min_inf_f)
    if (input$bound_min_inf_f == "-Inf") {
      shinyjs::disable("bound_min_num_f")
    } else {
      shinyjs::enable("bound_min_num_f")
    }
  })
  
  observe({
    req(input$bound_max_inf_f)
    if (input$bound_max_inf_f == "Inf") {
      shinyjs::disable("bound_max_num_f")
    } else {
      shinyjs::enable("bound_max_num_f")
    }
  })
  
  bound_min_f <- reactive({
    if (input$bound_min_inf_f == "-Inf") -Inf else input$bound_min_num_f
  })
  
  bound_max_f <- reactive({
    if (input$bound_max_inf_f == "Inf") Inf else input$bound_max_num_f
  })
  
  ### Old Faithful reactive model ----
  model_data_f <- reactive({
    req(input$support_min_f, input$support_max_f)
    
    tryCatch({
      # Create variable - use .0 to ensure continuous variable
      support_range_f <- c(as.numeric(input$support_min_f) + 0.0,
                           as.numeric(input$support_max_f) + 0.0)
      bounds_range_f <- c(bound_min_f(),
                          bound_max_f())
      
      var_f <- numeric_var("waiting", 
                           support = support_range_f,
                           add = c(-0.1, 0), # Not reactive, better x-axis scale
                           bounds = bounds_range_f)
      
      # Create Bernstein basis
      B_f <- Bernstein_basis(var_f,
                             order = input$order_f,
                             ui = defaults_faithful$bernstein$ui_constraint)
      
      # Create CTM
      ctm_f <- ctm(response = B_f,
                   todistr = input$distribution_f)
      
      # Fit model
      mlt_f <- mlt(ctm_f, data = faithful)
      
      list(
        model = mlt_f,
        variable = var_f,
        data = faithful,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        variable = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback()),
        input_values = list(
          support_min = input$support_min_f,
          support_max = input$support_max_f,
          order = input$order_f,
          distribution = input$distribution_f
        )
      )
    })
  })
  
  ### Old Faithful model status ----
  output$model_status_f <- renderText({
    model_info <- model_data_f()
    if (model_info$success) {
      formula_text <- paste(deparse(model_info$formula, width.cutoff = 30),
                            collapse = "\n")
      paste(
        "Fitting: SUCCESS\n",
        "--DATASET--\n",
        "Observations:", nrow(faithful), "\n",
        "Response Range: (", round(min(faithful$waiting), 2), ",",
        round(max(faithful$waiting), 2), ")\n",
        "--MODEL--\n",
        "Y:", "waiting\n",
        "Bernstein Basis Order:", input$order_f, "\n",
        "Distribution:", input$distribution_f, "\n",
        "Support: (", input$support_min_f, ",", input$support_max_f, ")\n",
        "Bounds: (", bound_min_f(), ",", bound_max_f(), ")\n"
      )
    } else {
      paste("Fitting: FAILED\n", model_info$message)
    }
  })
  
  ### Old Faithful model summary ----
  output$model_summary_f <- renderText({
    model_info <- model_data_f()
    if (model_info$success) {
      
      # Capture information
      model_summary <- summary(model_info$model)
      model_type <- model_summary$type
      coef_text <- capture.output(coef(model_info$model))
      loglik_text <- model_summary$logLik
      
      # Combine into a formatted block
      summary_text <- paste(
        "--TYPE--\n",
        model_type, "\n",
        "\n\n--LOG-LIKELIHOOD--\n",
        paste(loglik_text, collapse = "\n"),
        sep = ""
      )
      
      return(summary_text)
    } else {
      cat("Model fitting failed:\n", model_info$message)
    }
  })
  
  ### Old Faithful predictions ----
  pred_data_f <- reactive({
    model_info <- model_data_f()
    validate(need(model_info$success, "Model fitting failed"))
    
    nd_f <- mkgrid(model_info$variable, 200)
    nd_f <- as.data.frame(nd_f)
    nd_f$tra <- predict(model_info$model, newdata = nd_f, type = "trafo")
    nd_f$den <- predict(model_info$model, newdata = nd_f, type = "density")
    nd_f$dis <- predict(model_info$model, newdata = nd_f, type = "distribution")
    nd_f
  })
  
  ### Old Faithful plots ----
  # Old Faithful transformation plot
  output$transformation_plot_f <- renderPlot({
    df <- pred_data_f()
    ggplot(df, aes(x = waiting, y = tra)) +
      geom_line(color = "#36338e", linewidth = 1.2) +
      labs(x = "Waiting time", y = "Transformation") +
      theme_minimal()
  })
  
  # Old Faithful density plot
  output$density_plot_f <- renderPlot({
    df <- pred_data_f()
    ggplot(df, aes(x = waiting, y = den)) +
      geom_line(color = "#f6a06d", linewidth = 1.2) +
      labs(x = "Waiting time", y = "Density") +
      theme_minimal()
  })
  
  # Old Faithful distribution plot
  output$distribution_plot_f <- renderPlot({
    df <- pred_data_f()
    ggplot(df, aes(x = waiting, y = dis)) +
      geom_line(color = "#9938ad", linewidth = 1.2) +
      labs(x = "Waiting time", y = "Distribution") +
      theme_minimal()
  })
  
  ### Old Faithful dataset ----
  # Data table output
  output$data_table_f <- DT::renderDataTable({
    data("faithful")
    DT::datatable(
      faithful
    ) %>% 
      DT::formatRound(columns = c('eruptions', 'waiting'), digits = 2)
  })
  
  # Summary statistics output  
  output$summary_f <- renderText({
    data("faithful")
    capture.output({
      print(summary(faithful))
    }) %>% paste(collapse = "\n")
  })
  
  ## Boston Linear--------------------------------------------------------------
  
  ### Boston Linear reset and select buttons ----
  observeEvent(input$reset_bernstein_lin, {
    updateSliderInput(session, "order_lin",
                      value = defaults_linear$bernstein$order)
  })
  
  observeEvent(input$reset_model_options_lin, {
    updateSelectInput(session, "distribution_lin",
                      selected = defaults_linear$distribution)
  })
  
  observeEvent(input$select_vars_lin, {
    updateCheckboxGroupInput(session, "vars_lin",
                             selected = c("crim", "zn", "indus", "chas", "nox",
                                          "rm", "age", "dis", "rad", "tax",
                                          "ptratio", "b", "lstat"))
  })
  
  observeEvent(input$deselect_vars_lin, {
    updateCheckboxGroupInput(session, "vars_lin", selected = character(0))
  })
  
  ### Boston Linear reactive model ----
  model_data_lin <- reactive({
    req(input$vars_lin)
    
    tryCatch({
      # Create variable
      var_lin <- numeric_var("cmedv", 
                             support = c(defaults_linear$params$support_min,
                                         defaults_linear$params$support_max),
                             bounds = c(defaults_linear$params$bound_min,
                                        defaults_linear$params$bound_max),
                             add = c(-4.9, 0)) # So no predictions for y = 0 in the plots.
      
      # Create Bernstein basis
      B_lin <- Bernstein_basis(var_lin, order = input$order_lin,
                               ui = defaults_linear$bernstein$ui_constraint)
      
      # Create formula
      formula_vars_lin <- paste(input$vars_lin, collapse = " + ")
      formula_lin <- as.formula(paste("cmedv ~", formula_vars_lin))
      
      # Create CTM
      ctm_lin <- ctm(response = B_lin, shifting = formula_lin[-2L],
                     data = BostonHousing2, todistr = input$distribution_lin)
      
      # Fit model
      mlt_lin <- mlt(ctm_lin, data = BostonHousing2, scale = TRUE)
      
      list(
        model = mlt_lin,
        formula = formula_lin,
        variable = var_lin,
        data = BostonHousing2,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        formula = NULL,
        variable = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback())
      )
    })
  })
  
  ### Boston Linear model status ----
  output$model_status_lin <- renderText({
    model_info <- model_data_lin()
    if (model_info$success) {
      formula_text <- paste(deparse(model_info$formula, width.cutoff = 30),
                            collapse = "\n") # Format long formula
      paste(
        "Fitting: SUCCESS\n",
        "--DATASET--\n",
        "Observations:", nrow(BostonHousing2), "\n",
        "Response Range: (", round(min(BostonHousing2$cmedv), 2), ",",
        round(max(BostonHousing2$cmedv), 2), ")\n",
        "--MODEL--\n",
        "Formula:", formula_text, "\n",
        "Bernstein Basis Order:", input$order_lin, "\n",
        "Distribution:", input$distribution_lin, "\n",
        "Support: (", defaults_linear$params$support_min, ",",
        defaults_linear$params$support_max, ")\n",
        "Bounds: (", defaults_linear$params$bound_min, ",",
        defaults_linear$params$bound_max, ")\n"
      )
    } else {
      paste("Fitting: FAILED\n", model_info$message)
    }
  })
  
  ### Boston Linear model summary ----
  output$model_summary_lin <- renderText({
    model_info <- model_data_lin()
    if (model_info$success) {
      
      # Capture information
      model_summary <- summary(model_info$model)
      model_type <- model_summary$type
      loglik_text <- model_summary$logLik
      
      # Skip Bernstein coefficients
      n <- input$order_lin + 1
      coef_values <- coef(model_info$model)
      if (length(coef_values) > n) {
        coef_values <- coef_values[-(1:n)]
      }
      coef_text <- capture.output(coef_values)
      
      # Combine into a formatted block
      summary_text <- paste(
        "--TYPE--\n",
        model_type, "\n\n",
        "--COEFFICIENTS--\n",
        paste(coef_text, collapse = "\n"),
        "\n\n--LOG-LIKELIHOOD--\n",
        paste(loglik_text, collapse = "\n"),
        sep = ""
      )
      
      return(summary_text)
    } else {
      cat("Model fitting failed:\n", model_info$message)
    }
  })
  
  ### Boston Linear predictions ----
  pred_data_lin <- reactive({
    model_info <- model_data_lin()
    validate(need(model_info$success, "Model fitting failed"))
    
    # Create prediction grid
    nd_lin <- mkgrid(model_info$variable, n = 200)
    
    # Get predictions
    pred_tra_lin <- predict(model_info$model, q = nd_lin$cmedv, type = "trafo",
                            terms = "bresponse")
    pred_den_lin <- predict(model_info$model, q = nd_lin$cmedv,
                            type = "density", terms = "bresponse")
    pred_dis_lin <- predict(model_info$model, q = nd_lin$cmedv,
                            type = "distribution",terms = "bresponse")
    
    # Transformation
    # The transformation function is the same for every observation, so we just choose the first column
    tra_data_lin <- data.frame(cmedv = nd_lin$cmedv, tra = pred_tra_lin[, 1])
    
    # Reshape density predictions to long format
    den_data_long_lin <- pred_den_lin %>%
      as.data.frame() %>%
      mutate(cmedv = nd_lin$cmedv) %>%
      pivot_longer(cols = -cmedv, 
                   names_to = "observation", 
                   values_to = "density")
    
    # Reshape distribution predictions to long format
    dis_data_long_lin <- pred_dis_lin %>%
      as.data.frame() %>%
      mutate(cmedv = nd_lin$cmedv) %>%
      pivot_longer(cols = -cmedv, 
                   names_to = "observation", 
                   values_to = "distribution")
    
    # Return all data as a list
    list(
      transformation = tra_data_lin,
      density = den_data_long_lin,
      distribution = dis_data_long_lin
    )
  })
  
  ### Boston Linear plots ----
  # Boston Linear transformation plot
  output$transformation_plot_lin <- renderPlot({
    df <- pred_data_lin()$transformation
    ggplot(df, aes(x = cmedv, y = tra)) +
      geom_line(color = "#36338e", linewidth = 1.2) +
      labs(x = "Median home value (in $1000s)", y = "Transformation") +
      theme_minimal()
  })
  
  # Boston Linear density plot
  output$density_plot_lin <- renderPlot({
    df <- pred_data_lin()$density
    ggplot(df, aes(x = cmedv, y = density, group = observation)) +
      geom_line(alpha = 0.3, color = "#f6a06d", linewidth = 0.6) +
      labs(x = "Median home value (in $1000s)", y = "Density") +
      theme_minimal()
  })
  
  # Boston Linear distribution plot
  output$distribution_plot_lin <- renderPlot({
    df <- pred_data_lin()$distribution
    ggplot(df, aes(x = cmedv, y = distribution, group = observation)) +
      geom_line(alpha = 0.3, color = "#9938ad", linewidth = 0.6) +
      labs(x = "Median home value (in $1000s)", y = "Distribution") +
      theme_minimal()
  })
  
  # Boston Linear quantile plot
  output$quantile_plot_lin <- renderPlot({
    # Get model info
    model_info <- model_data_lin()
    validate(need(model_info$success, "Model fitting failed"))
    
    # Code copied and adapted from the mlt package vignette's R code, chunk "mlt-BostonHousing-plot": "https://cran.r-project.org/web/packages/mlt.docreg/vignettes/mlt.R"
    
    q <- 3:52  # Quantiles
    
    # Predict distribution for each quantile
    d <- predict(model_info$model, q = q, type = "distribution")
    # Linear predictors
    lp <- c(predict(model_info$model, q = 1, terms = "bshift"))
    
    # Build long data frame
    nd <- expand.grid(q = q, lp = -lp)
    nd$d <- as.vector(d)
    
    # Observed data
    observed_df <- data.frame(
      lp = -lp,
      cmedv = model_info$data$cmedv
    )
    
    # Plot
    ggplot() +
      geom_contour_filled(
        data = nd,
        aes(x = lp, y = q, z = d),
        breaks = seq(0.1, 0.9, by = 0.1),
        alpha = 0.8
      ) +
      geom_point(
        data = observed_df,
        aes(x = lp, y = cmedv),
        alpha = 0.25, size = 1.5
      ) +
      scale_fill_viridis_d(option = "C", name = "Probability", direction = -1) +
      labs(
        x = "Linear predictor",
        y = "Observed cmedv"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right"
      )
  })
  
  ### Normal Linear reactive model ----
  model_data_norm <- reactive({
    req(input$vars_lin)
    
    tryCatch({
      # Load BostonHousing2 data
      data(BostonHousing2, package = "mlbench")
      
      formula_vars_norm <- paste(input$vars_lin, collapse = " + ")
      formula_norm <- as.formula(paste("cmedv ~", formula_vars_norm))
      lm_norm <- lm(formula_norm, data = BostonHousing2)
      
      list(
        model = lm_norm,
        formula = formula_norm,
        data = BostonHousing2,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        formula = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback())
      )
    })
  })
  
  ### Normal Linear Model quantile plot ----
  output$quantile_plot_norm <- renderPlot({
    # Get model info
    model_info <- model_data_norm()
    validate(need(model_info$success, "Model fitting failed"))
    
    # Code copied and adapted from the mlt package vignette's R code, chunk "mlt-BostonHousing-plot": "https://cran.r-project.org/web/packages/mlt.docreg/vignettes/mlt.R"
    
    q <- 3:52  # Quantiles
    
    m <- predict(model_info$model, data = model_info$data)
    s <- summary(model_info$model)$sigma
    d <- sapply(m, function(mu) pnorm(q, mean = mu, sd = s))
    
    # Build long data frame
    nd <- expand.grid(q = q, lp = m)
    nd$d <- c(d)
    
    # Observed data
    observed_df <- data.frame(
      lp = m,
      cmedv = model_info$data$cmedv
    )
    
    # Plot
    ggplot() +
      geom_contour_filled(
        data = nd,
        aes(x = lp, y = q, z = d),
        breaks = seq(0.1, 0.9, by = 0.1),
        alpha = 0.8
      ) +
      geom_point(
        data = observed_df,
        aes(x = lp, y = cmedv),
        alpha = 0.25, size = 1.5
      ) +
      scale_fill_viridis_d(option = "C", name = "Probability", direction = -1) +
      labs(
        x = "Linear predictor",
        y = "Observed cmedv"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right"
      )
  })
  
  ### Boston Linear dataset ----
  # Data table output
  output$data_table_lin <- DT::renderDataTable({
    data("BostonHousing2")
    DT::datatable(
      BostonHousing2,
      options = list(scrollX = TRUE,
                     columnDefs = list(
                       list(className = 'dt-center', targets = 10) # Center 10th column
                     )
      )
    ) %>% 
      DT::formatRound(columns = c('tract', 'lon', 'lat', 'medv', 'cmedv',
                                  'crim', 'zn', 'indus', 'nox', 'rm', 'age',
                                  'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat'),
                      digits = 2)
  })
  
  # Summary statistics output  
  output$summary_lin <- renderText({
    data("BostonHousing2")
    capture.output({
      print(summary(BostonHousing2))
    }) %>% paste(collapse = "\n")
  })
  
  ## Boston Stratified -----------------------------------------------------------
  
  ### Boston Stratified reset and select buttons ----
  observeEvent(input$reset_bernstein_strat, {
    updateSliderInput(session, "order_strat",
                      value = defaults_stratified$bernstein$order)
  })
  
  observeEvent(input$select_vars_strat, {
    updateCheckboxGroupInput(session, "vars_strat",
                             selected = c("crim", "zn", "indus", "nox", "rm",
                                          "age", "dis", "rad", "tax", "ptratio",
                                          "b", "lstat"))
  })
  
  observeEvent(input$deselect_vars_strat, {
    updateCheckboxGroupInput(session, "vars_strat", selected = character(0))
  })
  
  ### Boston Stratified reactive model ----
  model_data_strat <- reactive({
    req(input$vars_strat)
    
    tryCatch({
      
      # Create formula
      formula_vars_strat <- paste(input$vars_strat, collapse = " + ")
      formula_strat <- as.formula(paste("cmedv | 0 + chas ~",
                                        formula_vars_strat))
      
      # Fit model
      tram_strat <- BoxCox(
        formula = formula_strat,
        data = BostonHousing2,
        order = input$order_strat
      )
      
      list(
        model = tram_strat,
        formula = formula_strat,
        data = BostonHousing2,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        formula = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback())
      )
    })
  })
  
  ### Boston Stratified model status ----
  output$model_status_strat <- renderText({
    model_info <- model_data_strat()
    if (model_info$success) {
      formula_text <- paste(deparse(model_info$formula, width.cutoff = 30),
                            collapse = "\n") # Format long formula
      paste(
        "Fitting: SUCCESS\n",
        "--DATASET--\n",
        "Observations:", nrow(BostonHousing2), "\n",
        "Response Range: (", round(min(BostonHousing2$cmedv), 2), ",",
        round(max(BostonHousing2$cmedv), 2), ")\n",
        "--MODEL--\n",
        "Formula:", formula_text, "\n",
        "Strata: chas\n",
        "Bernstein Basis Order:", input$order_strat, "\n",
        "Distribution:", defaults_stratified$distribution, "\n"
      )
    } else {
      paste("Fitting: FAILED\n", model_info$message)
    }
  })
  
  ### Boston Stratified model summary ----
  output$model_summary_strat <- renderText({
    model_info <- model_data_strat()
    if (model_info$success) {
      
      # Capture information
      model_summary <- summary(model_info$model)
      model_type <- model_summary$tram
      coef_text <- capture.output(coef(model_info$model))
      loglik_text <- logLik(model_info$model)
      
      # Combine into a formatted block
      summary_text <- paste(
        "--TYPE--\n",
        model_type, "\n\n",
        "--COEFFICIENTS--\n",
        paste(coef_text, collapse = "\n"),
        "\n\n--LOG-LIKELIHOOD--\n",
        paste(loglik_text, collapse = "\n"),
        sep = ""
      )
      
      return(summary_text)
    } else {
      cat("Model fitting failed:\n", model_info$message)
    }
  })
  
  ### Boston Stratified predictions ----
  pred_data_strat <- reactive({
    model_info <- model_data_strat()
    validate(need(model_info$success, "Model fitting failed"))
    
    # Transformation
    # Create a grid of cmedv values
    cmedv_grid <- seq(0, 50, length.out = 200)
    
    # Predict transformation function for both strata
    pred_tra_strat <- predict(model_info$model, q = cmedv_grid, 
                              type = "trafo", terms = "binteracting")
    
    # Define the two levels
    chas_0 <- which(BostonHousing2$chas == 0)
    chas_1 <- which(BostonHousing2$chas == 1)
    
    # Prepare data for plotting
    plot_data_tra <- data.frame(
      cmedv = cmedv_grid,
      chas_0 = pred_tra_strat[, chas_0[1]],
      chas_1 = pred_tra_strat[, chas_1[1]]
    )
    
    # Reshape to long format
    plot_data_long_tra <- plot_data_tra %>%
      pivot_longer(cols = c(chas_0, chas_1),
                   names_to = "chas",
                   values_to = "transformation")
    
    # Recode group labels
    plot_data_long_tra$chas <- ifelse(plot_data_long_tra$chas == "chas_1", 
                                      "1", "0")
    
    # Density and distribution
    # Predict density and distribution
    pred_den_strat <- predict(model_info$model, q = cmedv_grid,
                              type = "density")
    pred_dis_strat <- predict(model_info$model, q = cmedv_grid,
                              type = "distribution")
    
    # Get the chas values for each observation
    chas_values <- BostonHousing2$chas
    
    # Reshape density predictions
    plot_data_long_den <- pred_den_strat %>%
      as.data.frame() %>%
      mutate(cmedv = cmedv_grid) %>%
      pivot_longer(cols = -cmedv, 
                   names_to = "observation", 
                   values_to = "density") %>%
      mutate(obs_index = as.numeric(gsub("V", "", observation)),
             chas = chas_values[obs_index])
    
    # Reshape distribution predictions
    plot_data_long_dis <- pred_dis_strat %>%
      as.data.frame() %>%
      mutate(cmedv = cmedv_grid) %>%
      pivot_longer(cols = -cmedv, 
                   names_to = "observation", 
                   values_to = "distribution") %>%
      mutate(obs_index = as.numeric(gsub("V", "", observation)),
             chas = chas_values[obs_index])
    
    list(
      transformation = plot_data_long_tra,
      density = plot_data_long_den,
      distribution = plot_data_long_dis
    )
  })  
  
  ### Boston Stratified plots ----
  # Stratum labels
  chas_labels <- c("0" = "Away from Charles River", "1" = "Near Charles River")
  # Boston Stratified transformation plot
  output$transformation_plot_strat <- renderPlot({
    df <- pred_data_strat()$transformation
    ggplot(df, aes(x = cmedv, y = transformation, color = chas)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("1" = "#36338e", 
                                    "0" = "#d24e71"),
                         labels = chas_labels) +
      labs(x = "Median home value (in $1000s)",
           y = "Transformation",
           color = NULL) +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.15),
            legend.text = element_text(size = 10),
            legend.background = element_blank())
  })
  
  # Boston Stratified density plot
  output$density_plot_strat <- renderPlot({
    df <- pred_data_strat()$density
    ggplot(df, aes(x = cmedv, y = density, group = observation)) +
      geom_line(alpha = 0.3, color = "#f6a06d", linewidth = 0.6) +
      labs(x = "Median home value (in $1000s)", y = "Density") +
      facet_wrap(~ chas, labeller = labeller(chas = chas_labels)) +
      theme_minimal() +
      theme(strip.text = element_text(size = 12))
  })
  
  # Boston Stratified distribution plot
  output$distribution_plot_strat <- renderPlot({
    df <- pred_data_strat()$distribution
    ggplot(df, aes(x = cmedv, y = distribution, group = observation)) +
      geom_line(alpha = 0.3, color = "#9938ad", linewidth = 0.6) +
      labs(x = "Median home value (in $1000s)", y = "Distribution") +
      facet_wrap(~ chas, labeller = labeller(chas = chas_labels)) +
      theme_minimal() +
      theme(strip.text = element_text(size = 12))
  })
  
  # Boston Stratified quantile plot
  output$quantile_plot_strat <- renderPlot({
    # Get model info
    model_info <- model_data_strat()
    validate(need(model_info$success, "Model fitting failed"))
    
    # Code copied and adapted from the tram package vignette's R code, chunk "tram-BostonHousing-Colr-1-plot": "https://cran.r-project.org/web/packages/tram/vignettes/tram.R"
    
    # Get predictions
    nd <- BostonHousing2
    nd$cmedv <- NULL
    q <- 0:50
    d <- predict(model_info$model, newdata = nd, q = q, type = "distribution")
    lp <- c(predict(model_info$model, newdata = nd, type = "lp"))
    
    # Build long data frame
    nd2 <- expand.grid(q = q, id = seq_along(lp))
    nd2$lp <- lp[nd2$id]
    nd2$chas <- nd$chas[nd2$id]
    nd2$d   <- c(d)
    
    # Observed data for point overlay
    BHtmp <- BostonHousing2 %>%
      mutate(lp = lp)
    
    # Plot
    ggplot() +
      geom_contour_filled(data = nd2,
                          aes(x = lp, y = q, z = d),
                          breaks = seq(0.1, 0.9, by = 0.1),
                          alpha = 0.8) +
      geom_point(
        data = BHtmp,
        aes(x = lp, y = cmedv),
        inherit.aes = FALSE,
        alpha = 0.25, size = 1.5
      ) +
      scale_fill_viridis_d(option = "C", name = "Probability", direction = -1) +
      facet_wrap(~ chas, labeller = labeller(chas = chas_labels)) +
      labs(
        x = "Linear predictor",
        y = "Observed cmedv"
      ) +
      theme_minimal() +
      theme(strip.text = element_text(size = 12)) # Facet labels size
  })
  
  ### Boston Stratified dataset ----
  # Data table output
  output$data_table_strat <- DT::renderDataTable({
    data("BostonHousing2")
    DT::datatable(
      BostonHousing2,
      options = list(scrollX = TRUE,
                     columnDefs = list(
                       list(className = 'dt-center', targets = 10) # Center 10th column
                     )
      )
    ) %>% 
      DT::formatRound(columns = c('tract', 'lon', 'lat', 'medv', 'cmedv',
                                  'crim', 'zn', 'indus', 'nox', 'rm', 'age',
                                  'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat'),
                      digits = 2)
  })
  
  # Summary statistics output  
  output$summary_strat <- renderText({
    data("BostonHousing2")
    capture.output({
      print(summary(BostonHousing2))
    }) %>% paste(collapse = "\n")
  })
  
  ## Boston Conditional ----------------------------------------------------------
  
  ### Boston Conditional reset and select buttons ----
  observeEvent(input$reset_bernstein_cond, {
    updateSliderInput(session, "order_cond",
                      value = defaults_conditional$bernstein$order)
  })
  
  observeEvent(input$select_vars_cond, {
    updateCheckboxGroupInput(session, "vars_cond",
                             selected = c("crim", "zn", "indus", "chas", "nox",
                                          "rm", "age", "dis", "rad", "tax",
                                          "ptratio", "b", "lstat"))
  })
  
  observeEvent(input$deselect_vars_cond, {
    updateCheckboxGroupInput(session, "vars_cond", selected = character(0))
  })
  
  ### Boston Conditional reactive model ----
  model_data_cond <- eventReactive(input$fit_model_cond, {
    req(input$vars_cond)
    
    # Notification that fitting has started
    showNotification("Fitting model... This may take several minutes.", 
                     type = "message", 
                     duration = NULL, 
                     id = "fitting_notification")
    
    tryCatch({
      
      # Create formula
      formula_vars_cond <- paste(input$vars_cond, collapse = " + ")
      formula_cond <- as.formula(paste("cmedv | ", formula_vars_cond, "~ 0"))
      
      # Fit model
      tram_cond <- BoxCox(
        formula = formula_cond,
        data = BostonHousing2,
        order = input$order_cond
      )
      
      list(
        model = tram_cond,
        formula = formula_cond,
        data = BostonHousing2,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        formula = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback())
      )
    })
  })
  
  ### Boston Conditional model status ----
  output$model_status_cond <- renderText({
    # Check if model has been fitted
    if (input$fit_model_cond == 0) {
      return("No model fitted yet.")
    }
    
    model_info <- model_data_cond()
    if (model_info$success) {
      formula_text <- paste(deparse(model_info$formula, width.cutoff = 30),
                            collapse = "\n") # Format long formula
      paste(
        "Fitting: SUCCESS\n",
        "--DATASET--\n",
        "Observations:", nrow(BostonHousing2), "\n",
        "Response Range: (", round(min(BostonHousing2$cmedv), 2), ",",
        round(max(BostonHousing2$cmedv), 2), ")\n",
        "--MODEL--\n",
        "Formula:", formula_text, "\n",
        "Bernstein Basis Order:", input$order_cond, "\n",
        "Distribution:", defaults_conditional$distribution, "\n"
      )
    } else {
      paste("Fitting: FAILED\n", model_info$message)
    }
  })
  
  ### Boston Conditional model summary ----
  output$model_summary_cond <- renderText({
    # Check if model has been fitted
    if (input$fit_model_cond == 0) {
      return("No model fitted yet.")
    }
    
    model_info <- model_data_cond()
    if (model_info$success) {
      
      # Capture information
      model_summary <- summary(model_info$model)
      model_type <- model_summary$tram
      coef_text <- capture.output(model_info$model$coef)
      model_obj <- model_info$model
      loglik_text <- logLik(model_info$model)
      
      # Combine into a formatted block
      summary_text <- paste(
        "--TYPE--\n",
        model_type, "\n\n",
        "--COEFFICIENTS--\n",
        paste(coef_text, collapse = "\n"),
        "\n\n--LOG-LIKELIHOOD--\n",
        paste(loglik_text, collapse = "\n"),
        sep = ""
      )
      
      return(summary_text)
    } else {
      cat("Model fitting failed:\n", model_info$message)
    }
  })
  
  ### Boston Linear dataset ----
  # Data table output
  output$data_table_cond <- DT::renderDataTable({
    data("BostonHousing2")
    DT::datatable(
      BostonHousing2,
      options = list(scrollX = TRUE,
                     columnDefs = list(
                       list(className = 'dt-center', targets = 10) # Center 10th column
                     )
      )
    ) %>% 
      DT::formatRound(columns = c('tract', 'lon', 'lat', 'medv', 'cmedv',
                                  'crim', 'zn', 'indus', 'nox', 'rm', 'age',
                                  'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat'),
                      digits = 2)
  })
  
  # Summary statistics output  
  output$summary_cond <- renderText({
    data("BostonHousing2")
    capture.output({
      print(summary(BostonHousing2))
    }) %>% paste(collapse = "\n")
  })
  
  ## Wine ----------------------------------------------------------------------
  
  ### Wine model ----
  model_data_cat <- reactive({
    
    tryCatch({
      
      # Fit model
      tram_cat <- Polr(rating ~ 1, data = wine, method = defaults_cat$method)
      
      list(
        model = tram_cat,
        data = wine,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback())
      )
    })
  })
  
  ### Wine model status ----
  output$model_status_cat <- renderText({
    model_info <- model_data_cat()
    if (model_info$success) {
      paste(
        "Fitting: SUCCESS\n",
        "--DATASET--\n",
        "Observations:", nrow(model_info$data), "\n",
        "Response Range: (", min(model_info$data$rating), ",",
        max(model_info$data$rating), ")\n",
        "--MODEL--\n",
        "Y:", "rating\n",
        "Link Function:", defaults_cat$method, "\n"
      )
    } else {
      paste("Fitting: FAILED\n", model_info$message)
    }
  })
  
  ### Wine model summary ----
  output$model_summary_cat <- renderText({
    model_info <- model_data_cat()
    if (model_info$success) {
      
      # Capture information
      model_summary <- summary(model_info$model)
      model_type <- model_summary$tram
      coef_text <- capture.output(coef(model_info$model))
      loglik_text <- logLik(model_info$model)
      
      # Combine into a formatted block
      summary_text <- paste(
        "--TYPE--\n",
        model_type, "\n\n",
        "\n--LOG-LIKELIHOOD--\n",
        paste(loglik_text, collapse = "\n"),
        sep = ""
      )
      
      return(summary_text)
    } else {
      cat("Model fitting failed:\n", model_info$message)
    }
  })
  
  ### Wine predictions ----
  pred_data_cat <- reactive({
    model_info <- model_data_cat()
    validate(need(model_info$success, "Model fitting failed"))
    
    # Code provided by Lucas Kook and modified by GJ
    
    res <- 1e3
    add <- 3
    object <- model_info$model
    y <- variable.names(object, "response")
    qs <- mkgrid(object, n = res)[[y]]  # y-values
    trafo <- predict(object, type = "trafo", q = qs)  # z-values
    pdf <- predict(object, type = "density", q = qs)  # lik-contributions
    cdf <- predict(object, type = "distribution", q = qs)
    pdfz <- diff(c(0, object$model$todistr$p(trafo)))  # lik-contributions
    
    dat <- data.frame(y = qs, cdf = cdf, pdf = pdf, trafo = trafo, pdfz = pdfz)
    dat$xaxis <- sprintf("y[%i]", dat$y)
    dat$xaxis <- paste0(dat$y, "\n", dat$xaxis)
    dat$y <- as.numeric(qs)
    dat$ystart <- c(0, dat$cdf[1:4])
    dat$xend <- dat$y+1
    dat$col <- factor(dat$y)
    
    # make grid on z-scale with res=1000 points in range of z-values plus add-margin
    trafo2 <- seq(min(trafo) - add, max(trafo[is.finite(trafo)]) + add,
                  length.out = res)
    # determine density on the z-grid
    pdfz2 <- object$model$todistr$d(trafo2)  # same as dlogis(trafo2)
    # produce z-intervals corresponding to the taken z-values and -Inf, +Inf at border
    cols2 <- cut(trafo2, breaks = c(-Inf, trafo))
    # determine CDF on z-grid
    cdfz2 <- plogis(trafo2)
    
    aux <- data.frame(trafo2 = trafo2, pdfz2 = pdfz2, cols2 = cols2, cdfz2 = cdfz2)
    
    list(
      y = y,
      dat = dat,
      aux = aux
    )
  })
  
  ### Wine plots ----
  
  # Code provided by Lucas Kook and modified by GJ
  
  # Density and Distribution
  output$plots1_cat <- renderPlot({
    dat <- pred_data_cat()$dat
    aux <- pred_data_cat()$aux
    
    # f_y(y) and f_z(z)
    
    pfy <- ggplot(dat) +
      geom_segment(aes(x = y, xend = y, y = 0, yend = pdf, color = col)) +
      geom_point(aes(x = y, y = pdf, color = col)) +
      ylab(expression(italic(f[Y](y[k]~"|"~x)))) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.line = element_line())+
      scale_y_continuous(breaks = round(c(0, dat$pdf), 2), limits = c(0, 0.4))
    
    pfz <- ggplot(aux) +
      geom_line(aes(x = trafo2, y = pdfz2)) +
      geom_area(aes(x = trafo2, y = pdfz2, fill = cols2), show.legend = FALSE) +
      xlab(expression(italic(z))) +
      ylab(expression(italic(f[Z](z)))) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.line = element_line()) +
      scale_x_continuous(breaks = round(dat$trafo[1:4], 1),
                         labels = expression(italic(h[1]), italic(h[2]),
                                             italic(h[3]), italic(h[4])))
    
    
    # F_y(y) and F_z(z)
    
    # F_y(y)
    
    pFy <- ggplot(dat) +
      geom_hline(aes(yintercept = cdf), linetype = "dashed", color = "darkgrey") +
      geom_segment(aes(x = y, y = cdf, xend = xend, yend = cdf), size = 1.2,
                   show.legend = FALSE, color = "gray") +
      geom_segment(aes(x = y, y = ystart, xend = y, yend = cdf, color = col),
                   size = 1.2, show.legend = FALSE) +
      geom_point(aes(x = y, y = cdf, color = col), show.legend = FALSE) +
      ylab(expression(italic(F[Y](y[k]~"|"~x)))) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.line = element_line()) +
      coord_cartesian(xlim=c(1,5)) +
      scale_y_continuous(breaks=round(c(0, dat$cdf),2), limits=c(0,1))
    
    # F_z(z)
    dat4 <- dat[1:4,]
    dat$tstart <- c(min(aux$trafo2), dat$trafo[1:4])
    dat$trafo[5] <- max(aux$trafo2)
    
    pFz <- ggplot(aux) +
      geom_hline(yintercept=dat$cdf, linetype="dashed", color = "darkgrey") +
      geom_step(data = aux, mapping = aes(x = trafo2, y = cdfz2)) +
      geom_segment(data = dat, aes(x = tstart, y = ystart, xend = trafo,
                                   yend = ystart, color = col),
                   size = 1.2) +
      geom_segment(data = dat[-5,], aes(x = trafo, y = ystart, xend = trafo,
                                        yend = cdf, color = col),
                   size = 1.2, show.legend = FALSE) +
      geom_point(data = dat4, mapping = aes(x = trafo, y = cdf, color = col),
                 show.legend = FALSE) +
      xlab(expression(italic(z))) +
      ylab(expression(F[z](z))) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.line = element_line()) +
      scale_y_continuous(breaks = round(c(0, dat$cdf), 2), limits = c(0, 1))+
      scale_x_continuous(breaks = round(dat$trafo[1:4], 1),
                         labels = expression(italic(h[1]), italic(h[2]),
                                             italic(h[3]), italic(h[4])))
    
    # Arrange plots
    pfy <- pfy + scale_color_viridis_d(end = 0.9, option = "C") +
      scale_x_continuous(breaks = 1:5)
    pfz <- pfz + scale_fill_viridis_d(labels = parse(text = paste0("y[", 1:5, "]")),
                                      end = 0.9, option = "C")
    pFy <- pFy + scale_color_viridis_d(guide = "none", end = 0.9, option = "C") +
      scale_x_continuous(breaks = 1:5)
    pFz <- pFz + scale_color_viridis_d(guide = "none", end = 0.9, option = "C")
    
    fig1 <- ggarrange(pfy, pfz, pFy, pFz, ncol = 2, nrow = 2,
                      labels = c("A", "C", "B", "D"))
    fig1
  })
  
  # Likelihoods
  output$plots2_cat <- renderPlot({
    dat <- pred_data_cat()$dat
    aux <- pred_data_cat()$aux
    
    fig2_p0 <- ggplot() + theme_void()
    
    fig2_p1 <- ggplot(dat, aes(color = factor(y))) +
      geom_segment(aes(x = y, xend = y, y = trafo, yend = -Inf), 
                   linetype = "dashed") +
      geom_segment(aes(x = y, xend = -Inf, y = trafo, yend = trafo), 
                   linetype = "dashed") +
      geom_point(aes(x = y, y = trafo)) +
      geom_text(aes(x = y, y = trafo,
                    label = paste0("italic(h(y[", 1:5, "]~'|'~x))")),
                parse = TRUE, size = 4, color = "black", hjust = -0.2,
                vjust = 0.5, show.legend = FALSE) +
      scale_x_continuous(limits = c(0.5, max(dat$y) + 0.5),
                         breaks = dat$y) +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank()) +
      ylim(range(aux$trafo2)) +
      xlab(" ") +
      ylab(" ") +
      scale_color_viridis_d(end = 0.9, option = "C")
    
    fig2_p2 <- ggplot(aux) +
      geom_line(aes(x = trafo2, y = pdfz2)) +
      geom_area(aes(x = trafo2, y = pdfz2, fill = cols2), show.legend = FALSE) +
      coord_flip() +
      scale_y_reverse() +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.line = element_line(),
            panel.grid = element_blank()) +
      xlim(range(aux$trafo2)) +
      xlab(expression(italic(z))) +
      ylab(expression(italic(f[Z](z)))) +
      scale_fill_viridis_d(end = 0.9, option = "C")
    
    fig2_p3 <- ggplot(dat) +
      geom_segment(aes(x = y, xend = y, y = 0, yend = pdf, color = col)) +
      geom_point(aes(x = y, y = pdf, color = col)) +
      scale_y_reverse() +
      expand_limits(x = c(min = 0.5, max(dat$y) + 0.5)) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line = element_line(),
            panel.grid = element_blank()) +
      ylab(expression(italic(f[Y](y[k]~"|"~x)))) +
      scale_color_viridis_d(end = 0.9, option = "C")
    
    fig2_cat <- ggarrange(fig2_p2, fig2_p1, fig2_p0, fig2_p3, ncol = 2, nrow = 2,
                          labels = c("A", "B", " ", "C"))
    fig2_cat
  })
  
  ### Wine dataset ----
  # Data table output
  output$data_table_cat <- DT::renderDataTable({
    data("wine")
    DT::datatable(
      wine)
  })
  
  # Summary statistics output  
  output$summary_cat <- renderText({
    data("wine")
    capture.output({
      print(summary(wine))
    }) %>% paste(collapse = "\n")
  })
  
  ## DVC -----------------------------------------------------------------------
  
  ### DVC reset buttons ----
  observeEvent(input$reset_bernstein_count, {
    updateSliderInput(session, "order_count", value = defaults_count$order)
  })
  
  observeEvent(input$reset_method_count, {
    updateSelectInput(session, "method_count", selected = defaults_count$method)
  })
  
  observeEvent(input$reset_logfirst_count, {
    updateRadioButtons(session, "logfirst_count",
                       selected = defaults_count$log_first)
  })
  
  ### DVC reactive model ----
  model_data_count <- reactive({
    
    tryCatch({
      # Create formula
      formula_count <- as.formula("DVC ~ year + weekday + tvar1 + tvar2 + tvar3 +tvar4 + tvar5 +
                                  tvar6 + tvar7 + tvar8 + tvar9 + tvar10 + tvar11 + tvar12 + tvar13 +
                                  tvar14 + tvar15 + tvar16 + tvar17 + tvar18 + tvar19 + tvar20")
      
      # Fit model
      cotram_count <- cotram(formula_count, data = dvc,
                             method = input$method_count,
                             log_first = as.logical(input$logfirst_count),
                             prob = c(0, .9), order = input$order_count)
      
      list(
        model = cotram_count,
        formula = formula_count,
        data = dvc,
        success = TRUE,
        message = "Model fitted successfully"
      )
    }, error = function(e) {
      list(
        model = NULL,
        formula = NULL,
        data = NULL,
        success = FALSE,
        message = paste("Error:", e$message),
        error_trace = capture.output(traceback())
      )
    })
  })
  
  ### DVC model status ----
  output$model_status_count <- renderText({
    model_info <- model_data_count()
    formula_text <- paste(deparse(model_info$formula, width.cutoff = 30),
                          collapse = "\n")
    if (model_info$success) {
      paste(
        "Fitting: SUCCESS\n",
        "--DATASET--\n",
        "Observations:", nrow(model_info$data), "\n",
        "Response Range: (", min(model_info$data$DVC), ",",
        max(model_info$data$DVC), ")\n",
        "--MODEL--\n",
        "Formula:", formula_text, "\n",
        "Bernstein Basis Order:", input$order_count, "\n",
        "Link Function:", input$method_count, "\n",
        "Log-first:", input$logfirst_count, "\n"
      )
    } else {
      paste("Fitting: FAILED\n", model_info$message)
    }
  })
  
  ###  DVC model summary ----
  output$model_summary_count <- renderText({
    model_info <- model_data_count()
    if (model_info$success) {
      
      # Capture information
      model_summary <- summary(model_info$model)
      model_type <- model_summary$tram
      coef_text <- capture.output(coef(model_info$model))
      loglik_text <- model_summary$ll
      
      # Combine into a formatted block
      summary_text <- paste(
        "--TYPE--\n",
        model_type, "\n\n",
        "--COEFFICIENTS--\n",
        paste(coef_text, collapse = "\n"),
        "\n\n--LOG-LIKELIHOOD--\n",
        paste(loglik_text, collapse = "\n"),
        sep = ""
      )
      
      return(summary_text)
    } else {
      cat("Model fitting failed:\n", model_info$message)
    }
  })
  
  ### DVC predictions ----
  pred_data_count <- reactive({
    model_info <- model_data_count()
    if (!model_info$success) return(NULL)
    
    model <- model_info$model
    data  <- model_info$data
    
    # Base model frame once
    mf <- model.frame(model)
    
    # One observation (the first row)
    nd_count_ref <- mf[1, ]
    
    # Hazard
    nd_count_haz <- mf[data$year == "2011", -1]
    nd_count_haz$day <- data$day[data$year == "2011"]
    nd_count_haz$weekday <- factor("Monday",
                                   levels = levels(nd_count_haz$weekday))
    
    nd_count_haz$haz <- predict(model, type = "lp", newdata = nd_count_haz) -
      predict(model, type = "lp", newdata = nd_count_haz)[1]
    
    # Transformation
    supp <- sort(unique(data$DVC))
    
    tra_vals <- vapply(supp, function(q) {
      nd_count_tra <- nd_count_ref
      nd_count_tra$DVC <- q
      as.numeric(predict(model, newdata = nd_count_tra, type = "trafo", q = q))
    }, numeric(1))
    
    pred_count_tra <- data.frame(DVC = supp, tra = tra_vals)
    
    # Density and distribution
    years <- levels(mf$year)
    df_years <- mf[match(years, mf$year), , drop = FALSE]
    df_years$year <- factor(years, levels = years)
    df_years$weekday <- factor("Monday", levels = levels(df_years$weekday))
    df_years[, grep("tvar", colnames(df_years))] <- 0
    
    q_count <- 0:200
    
    # Density
    den_list <- lapply(levels(df_years$year), function(yr) {
      nd_count_den <- df_years[df_years$year == yr, , drop = FALSE]
      den_vals <- as.numeric(predict(model, newdata = nd_count_den, q = q_count,
                                     type = "density"))
      data.frame(q = q_count, den = den_vals, year = yr)
    })
    pred_count_den <- do.call(rbind, den_list)
    
    # Distribution
    dis_list <- lapply(levels(df_years$year), function(yr) {
      nd_count_dis <- df_years[df_years$year == yr, , drop = FALSE]
      dis_vals <- as.numeric(predict(model, newdata = nd_count_dis, q = q_count,
                                     type = "distribution"))
      data.frame(q = q_count, dis = dis_vals, year = yr)
    })
    pred_count_dis <- do.call(rbind, dis_list)
    
    list(
      hazard = nd_count_haz,
      transformation = pred_count_tra,
      density = pred_count_den,
      distribution = pred_count_dis,
      ref = nd_count_ref
    )
  })
  
  ### DVC plots ----
  # DVC hazard ratio plot
  output$hazard_plot_count <- renderPlot({
    df <- pred_data_count()$hazard
    
    # cloglog message
    if (input$method_count != "cloglog") {
      plot.new()
      title("Hazard Ratio is only available with cloglog link function")
      return(invisible())
    }
    
    # Plot
    ggplot(df, aes(x = day, y = exp(haz))) +
      geom_hline(yintercept = 1, color = "grey70", linetype = "dashed") +
      geom_line(color = "#d24e71", linewidth = 1.2) +
      labs(
        x = "Day of year",
        y = "Hazard ratio"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  # DVC transformation plot
  output$transformation_plot_count <- renderPlot({
    df <- pred_data_count()$transformation
    ggplot(df, aes(x = DVC, y = tra)) +
      geom_line(color = "#36338e", linewidth = 1.2) +
      labs(x = "Number of deer-vehicle collisions",
           y = "Transformation") +
      theme_minimal()
  })
  
  # DVC density plot
  output$density_plot_count <- renderPlot({
    df <- pred_data_count()$density
    ggplot(df, aes(x = q, y = den, color = year)) +
      geom_line(linewidth = 0.9) +
      scale_colour_viridis_d(option = "C", alpha = 0.7, end = 0.9,
                             direction = -1) +
      labs(
        x = "Number of deer-vehicle collisions",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.position = c(0.85, 0.4),
        legend.background = element_blank()
      )
  })
  
  # DVC distribution plot
  output$distribution_plot_count <- renderPlot({
    df <- pred_data_count()$distribution
    ggplot(df, aes(x = q, y = dis, color = year)) +
      geom_line(linewidth = 0.9) +
      scale_colour_viridis_d(option = "C", alpha = 0.7, end = 0.9,
                             direction = -1) +
      labs(
        x = "Number of deer-vehicle collisions",
        y = "Distribution"
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.position = c(0.85, 0.4),
        legend.background = element_blank()
      )
  })
  
  ### DVC dataset ----
  # Data table output
  output$data_table_count <- DT::renderDataTable({
    DT::datatable(
      dvc,
      options = list(scrollX = TRUE, pageLength = 10)
    ) %>%
      DT::formatRound(
        columns = c("tvar1", "tvar2", "tvar3", "tvar4", "tvar5",
                    "tvar6", "tvar7", "tvar8", "tvar9", "tvar10",
                    "tvar11", "tvar12", "tvar13", "tvar14",
                    "tvar15", "tvar16", "tvar17", "tvar18",
                    "tvar19", "tvar20"),
        digits = 5
      ) %>%
      
      DT::formatStyle(
        columns = c("DVC", "year", "time"),
        'text-align' = 'center')
  })
  
  # Summary statistics output  
  output$summary_count <- renderText({
    capture.output({
      print(summary(dvc))
    }) %>% paste(collapse = "\n")
  })
  
  
  ## Model Builder -------------------------------------------------------------
  
  # Describe data
  output$link_function_ui <- renderUI({
    response_type <- input$response_type
    
    if (response_type %in% c("continuous")) {
      selectInput("link_function", "Link Function:",
                  choices = c("Linear" = "Lm",
                              "Box-Cox" = "BoxCox",
                              "Colr" = "Colr"),
                  selected = "Lm")
    } else if (response_type == "ordered") {
      selectInput("link_function", "Link Function:",
                  choices = c("Logistic (proportional odds)" = "Polr_logistic",
                              "Normal (probit)" = "Polr_probit"),
                  selected = "Polr_logistic")
    } else if (response_type == "count") {
      selectInput("link_function", "Link function:",
                  choices = c("Log-log" = "loglog",
                              "Complementary log-log" = "cloglog",
                              "Logit" = "logit",
                              "Probit" = "probit"),
                  selected = "cloglog")
    }
  })
  
  # Initialize reactive values to store covariates
  covariates <- reactiveVal(list(
    list(
      id = 1,
      name = "var1"
    )
  ))
  
  # Add covariate
  observeEvent(input$add_covariate, {
    current_covs <- covariates()
    new_id <- length(current_covs) + 1
    
    # Preserve existing covariate names before adding new one
    if (length(current_covs) > 0) {
      for (i in seq_along(current_covs)) {
        current_name <- input[[paste0("cov_name_", i)]]
        if (!is.null(current_name)) {
          current_covs[[i]]$name <- current_name
        }
      }
    }
    
    # Add new covariate
    current_covs[[new_id]] <- list(
      id = new_id,
      name = paste0("var", new_id)
    )
    covariates(current_covs)
  })
  
  # Remove covariate function
  observeEvent(input$remove_covariate, {
    cov_id <- as.numeric(input$remove_covariate)
    current_covs <- covariates()
    current_covs[[cov_id]] <- NULL
    covariates(current_covs)
  })
  
  # Render covariate inputs
  output$covariate_inputs <- renderUI({
    covs <- covariates()
    
    lapply(seq_along(covs), function(i) {
      cov <- covs[[i]]
      # Use stored name or current input value
      current_name <- isolate(input[[paste0("cov_name_", i)]]) %||% cov$name
      
      div(
        style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
        fluidRow(
          column(10,
                 textInput(paste0("cov_name_", i), "Name:", 
                           value = current_name)
          ),
          column(2,
                 actionButton(paste0("remove_", i), "",
                              icon = icon("trash"),
                              class = "btn-danger btn-sm",
                              style = "padding: 2px 6px; float: right;",
                              onclick = paste0("Shiny.setInputValue('remove_covariate',",
                                               i, ", {priority: 'event'})"))
          )
        )
      )
    })
  })
  
  # Render stratification variable selector
  output$stratification_var_ui <- renderUI({
    covs <- covariates()
    
    if (length(covs) < 2) {
      return(p("Add at least 2 covariates to use stratification (one for strata, one for shifting).", 
               style = "color: #888; font-style: italic;"))
    }
    
    choices <- sapply(seq_along(covs), function(i) {
      input[[paste0("cov_name_", i)]] %||% paste0("var", i)
    })
    names(choices) <- choices
    
    selectInput("strat_var_select", "Stratification variable:",
                choices = choices)
  })
  
  # Generate code
  generated_code_text <- eventReactive(input$generate_code, {
    req(input$response_type)
    
    # Header
    code <- "# Load required packages\n"
    if (input$response_type == "count") {
      code <- paste0(code, "library(cotram)\n\n")
    } else {
      code <- paste0(code, "library(tram)\n\n")
    }
    
    # Model-specific code
    if (input$response_type == "count") {
      code <- paste0(code, generate_cotram_code())
    } else if (input$response_type == "continuous") {
      code <- paste0(code, generate_continuous_code())
    } else if (input$response_type == "ordered") {
      code <- paste0(code, generate_ordered_code())
    }
    
    code
  })
  
  output$generated_code <- renderText({
    generated_code_text()
  })
  
  # Helper function for continuous models
  generate_continuous_code <- function() {
    covs <- covariates()
    
    if (length(covs) == 0) {
      return("# Please add at least one covariate before generating code.")
    }
    
    req(input$model_structure)
    response_name <- input$response_name %||% "y"
    
    # Build formula
    formula_str <- NULL  # Initialize
    
    cov_terms <- sapply(seq_along(covs), function(i) {
      cov_name <- input[[paste0("cov_name_", i)]] %||% paste0("var", i)
      return(cov_name)
    })
    
    if (input$model_structure == "shifting") {
      formula_str <- paste(response_name, "~", paste(cov_terms,
                                                     collapse = " + "))
    } else if (input$model_structure == "stratified") {
      strat_var <- input$strat_var_select %||% "strat_var"
      # Remove stratification variable from shifting terms
      shifting_terms <- cov_terms[cov_terms != strat_var]
      if (length(shifting_terms) > 0) {
        formula_str <- paste(response_name, "| 0 +", strat_var, "~",
                             paste(shifting_terms, collapse = " + "))
      } else {
        formula_str <- paste(response_name, "| 0 +", strat_var, "~ 1")
      }
    } else if (input$model_structure == "conditional") {
      formula_str <- paste(response_name, "|", paste(cov_terms,
                                                     collapse = " + "), "~ 0")
    }
    
    # Determine bounds values
    lower_bound <- if (input$bound_min_type == "neg_inf") {
      "-Inf"
    } else {
      input$bound_min_builder
    }
    
    upper_bound <- if (input$bound_max_type == "pos_inf") {
      "Inf"
    } else {
      input$bound_max_builder
    }
    
    # Build the model code
    code <- paste0(
      "# Fit continuous transformation model\n",
      "model <- ", input$link_function, "(\n",
      "  ", formula_str, ",\n",
      "  data = my_data,\n",
      "  support = c(", input$support_min_builder, ", ",
      input$support_max_builder, "),\n",
      "  bounds = c(", lower_bound, ", ", upper_bound, "),\n",
      "  order = ", input$builder_order, "\n",
      ")\n"
    )
    
    return(code)
  }
  
  # Helper function for ordered models
  generate_ordered_code <- function() {
    covs <- covariates()
    
    if (length(covs) == 0) {
      return("# ERROR: Please add at least one covariate before generating code.")
    }
    
    response_name <- input$response_name %||% "y"
    model_structure <- input$model_structure_ordered %||% "proportional"
    
    # Build formula
    cov_terms <- sapply(seq_along(covs), function(i) {
      cov_name <- input[[paste0("cov_name_", i)]] %||% paste0("var", i)
      return(cov_name)
    })
    
    if (model_structure == "proportional") {
      # Proportional odds: y ~ x1 + x2 + x3
      formula_str <- paste(response_name, "~", paste(cov_terms, collapse = " + "))
    } else if (model_structure == "stratified") {
      # Stratified: y | 1 + strat_var ~ other_vars
      strat_var <- input$strat_var_select %||% "strat_var"
      shifting_terms <- cov_terms[cov_terms != strat_var]
      
      if (length(shifting_terms) > 0) {
        formula_str <- paste(response_name, "| 1 +", strat_var, "~",
                             paste(shifting_terms, collapse = " + "))
      } else {
        formula_str <- paste(response_name, "| 1 +", strat_var, "~ 1")
      }
    }
    
    method_arg <- ""
    if (input$link_function == "Polr_probit") {
      method_arg <- ',\n  method = "probit"'
    } else if (input$link_function == "Polr_logistic") {
      method_arg <- ',\n  method = "logistic"'
    }
    
    code <- paste0(
      "# Fit ordered transformation model\n",
      "model <- Polr(\n",
      "  ", formula_str, ",\n",
      "  data = my_data",
      method_arg, "\n",
      ")\n"
    )
    
    return(code)
  }
  
  # Helper function for count models
  generate_cotram_code <- function() {
    covs <- covariates()
    
    if (length(covs) == 0) {
      return("# Please add at least one covariate before generating code.")
    }
    
    response_name <- input$response_name %||% "y"
    
    cov_terms <- sapply(seq_along(covs), function(i) {
      cov_name <- input[[paste0("cov_name_", i)]] %||% paste0("var", i)
    })
    formula_str <- paste(response_name, "~", paste(cov_terms, collapse = " + "))
    
    code <- paste0(
      "# Fit count transformation model\n",
      "model <- cotram(\n",
      "  ", formula_str, ",\n",
      "  data = my_data,\n",
      "  method = '", input$link_function, "',\n",
      "  prob = c(0, 0.9),\n",
      "  order = ", input$builder_order, "\n",
      ")\n"
    )
    
    code
  }
}

# Run the application ----------------------------------------------------------

shinyApp(ui = ui, server = server)