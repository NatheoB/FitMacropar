library(shiny)
library(fitdistrplus)

function(input, output, session){
  # Functions
  calc.AIC <- function(distrib, pars.number) {
    return(2*pars.number - 2*sum(log(distrib)))
  }

  calc.BIC <- function(distrib, pars.number, sample.size) {
    return(pars.number*log(sample.size) - 2*sum(log(distrib)))
  }

  rank.models <- function(BICs) {
    index <- sort(BICs, index.return = TRUE)$ix
    rank <- rep(0, length(BICs))
    for (i in 1:length(BICs)) {
      rank[index[i]] <- i
    }
    return(rank)
  }

  Mk.model1 <- function(k, M0) {
    return((1 - M0)^k * M0)
  }

  # (Log) Density function with given M0 and input data
  dMk.model1 <- function(x, M0, log = FALSE) {
    if (log) {
      return(sapply(x, function(k){log(Mk.model1(k, M0))}))
    }
    return(sapply(x, function(k){Mk.model1(k, M0)}))
  }

  # Calculate Negative Log-Likelihood
  NLL.model1 <- function(M0, data) {
    -sum(dMk.model1(x = data, M0, log = TRUE))
  }

  # Create a vector with all sampling from data
  data.sampling <- rep(data.original$paraPerHost, data.original$freq)
  # Create a vector of frequency
  data.frequency <- data.original$freq
  data.sampleSize <- sum(data.frequency)

  # Table output
  matrix.output <- t(as.matrix(data.original))
  rownames(matrix.output) <- c("Frequency", "No of parasites / host")
  output$table <- renderTable(matrix.output, rownames = TRUE, colnames = FALSE)

  # Stats output
  output$stats <- renderPrint({
    summary(data.sampling)
  })

  # Fit data button
  observeEvent(input$fit.nb, {
    fit.mle.nb <- fitdist(data.sampling, "nbinom", method = "mle")
    updateNumericInput(session, "pars.nb.size", value = fit.mle.nb$estimate[1][[1]])
    updateNumericInput(session, "pars.nb.mu", value = fit.mle.nb$estimate[2][[1]])
  })

  observeEvent(input$fit.pois, {
    fit.mle.pois <- fitdist(data.sampling, "pois", method = "mle")
    updateNumericInput(session, "pars.pois.lambda", value = fit.mle.pois$estimate[1][[1]])
  })

  observeEvent(input$fit.model1, {
    fit.mle.model1 <- optimize(f = NLL.model1, interval = c(0,1), data = data.sampling)
    updateNumericInput(session, "pars.model1.M0", value = fit.mle.model1$minimum)
  })


  # Max K of histogram
  output$maxK.out <- renderUI({
    sliderInput("maxK.in", "maxK.out", min = 0, max = length(data.frequency)-1, value = length(data.frequency)-1)
  })

  # Reactive discrete probability distribution and reactive frequencies
    # Create NBD data
  dpd.nb <- reactive({
    dnbinom(
      0:(length(data.frequency)-1),
      size = input$pars.nb.size, mu = input$pars.nb.mu
    )
  })
  freq.nb <- reactive(dpd.nb() * data.sampleSize)

  # Create Poisson data
  dpd.pois <- reactive({
    dpois(
      0:(length(data.frequency)-1),
      lambda = input$pars.pois.lambda
    )
  })
  freq.pois <- reactive(dpd.pois() * data.sampleSize)

  # Create Model 1 data
  dpd.model1 <- reactive({
    dMk.model1(
      0:(length(data.frequency)-1),
      M0 = input$pars.model1.M0
    )
  })
  freq.model1 <- reactive(dpd.model1() * data.sampleSize)

  # Initialize histogram
  plot.y <- reactive(rbind(freq.nb()[1:(input$maxK.in+1)],
                  freq.pois()[1:(input$maxK.in+1)],
                  freq.model1()[1:(input$maxK.in+1)],
                  data.frequency[1:(input$maxK.in+1)]))
  plot.grades <- c("Negative binomial", "Poisson", "Model1", "Observed")
  plot.cols <- c("red", "blue", "yellow", "green")
  plot.x <- reactive(0:input$maxK.in)


   # Plot histrogram
  output$hist <- renderPlot({
    barplot(plot.y()[as.numeric(input$hist.models),], beside = TRUE,
            col = plot.cols[as.numeric(input$hist.models)],
            xlab = "No of parasites / host", ylab = "Frequency",
            legend.text = plot.grades[as.numeric(input$hist.models)],
            main = "Macroparasite multiplicity")
  })

  # Models comparison
    # AIC
  AIC <- reactive(c(
    calc.AIC(dpd.nb()[data.sampling+1], 2),
    calc.AIC(dpd.pois()[data.sampling+1], 1),
    calc.AIC(dpd.model1()[data.sampling+1], 1)
  ))

    # BIC
  BIC <- reactive(c(
    calc.BIC(dpd.nb()[data.sampling+1], 2, data.sampleSize),
    calc.BIC(dpd.pois()[data.sampling+1], 1, data.sampleSize),
    calc.BIC(dpd.model1()[data.sampling+1], 1, data.sampleSize)
  ))

    # Results
  comparison <- matrix(
    data = rep(0, 4 * (length(plot.grades)-1)),
    nrow = length(plot.grades)-1,
    ncol = 4,
    byrow = TRUE
  )
  dimnames(comparison) <- list(
    plot.grades[1:(length(plot.grades)-1)],
    c("AIC", "AIC.Rank", "BIC", "BIC.Rank"))

  # Render models comparison
  output$comparison <- renderTable({
    comparison
  }, rownames = TRUE)
}
