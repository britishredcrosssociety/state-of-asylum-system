# State of the asylum system
For Policy, Research and Advocacy's flagship publication on the state of the UK's asylum system. This repo contains analysis code and a Shiny web app for prototyping the interactive website.

Prototype web app with interactive graphs: https://britishredcross.shinyapps.io/state-of-asylum-system/

## How plots work in this app
This Shiny app uses [modules](https://shiny.rstudio.com/articles/modules.html) for each of the plots. This makes the code easier to read and to extend.

Each plot (i.e. Shiny module) has two components:

1. A user interface (UI)
2. A server

Both of these components are stored in a single .R script, and there would be one .R file for each plot/module.

For example, the plot showing numbers of asylum applications is in [`R/applicationsPlot.R`](https://github.com/britishredcrosssociety/state-of-asylum-system/blob/main/R/applicationsPlot.R). It's UI is defined by the following function:

```
applicationsPlotUI <- function(id) {
  ...
}
```

And its server is:

```
applicationsPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ...
  })
}
```

In this example, the UI contains three parts:

1. A title: `tags$h4(...)`
2. A select box letting the user filter by nation/nationality: `selectizeInput(...)`
3. The plot itself: `plotlyOutput(...)`

The server function contains code to filter and summarise the asylum applications data (depending on whether the user has selected any nations or not, and which nations they have selected), and then plot the interactive graph. This code is in `output$plot <- renderPlotly({...})`.

## How to add new plots
To add a new plot, you can use the code in [`R/applicationsPlot.R`](https://github.com/britishredcrosssociety/state-of-asylum-system/blob/main/R/applicationsPlot.R) as a template. Remember to change the ...UI and ...Server function names.

Not all plots will need a select box - and might use different UI components altogether. The [Mastering Shiny book](https://mastering-shiny.org/) has [a chapter on UI components](https://mastering-shiny.org/basic-ui.html).

The main thing you will need to change is the code within `output$plot <- renderPlotly({...})`.

Once you've created a new `...Plot.R` file, you need to add your plot to the main user interface. In `R/appUI.R`, add a new `fluidRow` in the relevant section and then call your new ...PlotUI function in there:

```
fluidRow(
  newPlotUI("newPlot")
)
```

Finally, you need to call the ...PlotServer function in `R/appServer.R`:

```
newPlotServer("newPlot")
```
