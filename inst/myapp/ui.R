library(shiny)

navbarPage(title = "FitMacropar",
  tabPanel(title = "Data resume",
    column(width = 12, tableOutput("table"), style = "overflow-y: scroll"),
    verbatimTextOutput("stats")
  ),
  tabPanel(title = "Fits",
   "Negative binomial",
   numericInput(
     inputId = "pars.nb.size", label = "Size",
     value = 0.5, min = 0, step = 0.01
   ),
   numericInput(
     inputId = "pars.nb.mu", label = "Mu",
     value = 0.5, min = 0, step = 0.01
   ),
   actionButton("fit.nb", "Fit to data"),
   "Poisson",
   numericInput(
     inputId = "pars.pois.lambda", label = "Lambda",
     value = 0.5, min = 0, step = 0.01
   ),
   actionButton("fit.pois", "Fit to data"),
   "Model 1",
   numericInput(
     inputId = "pars.model1.M0", label = "M0",
     value = 0.5, min = 0, max = 1, step = 0.01
   ),
   actionButton("fit.model1", "Fit to data")
  ),
  navbarMenu(title = "Results",
    tabPanel(title = "Histogram",
     uiOutput("maxK.out"),
     checkboxGroupInput(
       "hist.models", "Models to show on histrogram", 
       choices = list("Negative binomial" = 1, "Poisson" = 2, "Model 1" = 3, "Observed" = 4),
       selected = c(1, 2, 3, 4),
       inline = TRUE
     ),
     plotOutput(outputId = "hist")
    ),
    tabPanel(title = "Khi2 test"),
    tabPanel(title = "Models comparison",
      tableOutput(outputId = "comparison")
    )
  )
)