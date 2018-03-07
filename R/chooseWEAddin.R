chooseWEAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Choose Worked Example File"),
    miniUI::miniContentPanel(

      shiny::selectInput("type", "Choose a File Format:",
                  list("HTML in web browser (html)"=1,
                       "R markdown in RStudio (.Rmd)"=2,
                       "Plain R code in RStudio(.R)"=3),
                  selected = 1
      ),
      shiny::selectInput("example", "Choose a Worked Example:",
                   list("Week 0: Intro to R Programming"=1,
                        "Week 0: R Graphics"=2,
                        "Week 1: Importing Genetic Data"=3,
                        "Week 2: Spatial Data"=4,
                        "Week 2: Bonus Material"=5,
                        "Week 3: Genetic Diversity"=6,
                        "Week 4: Metapopulation Genetics"=7,
                        "Week 5: Spatial Statistics"=8,
                        "Week 6: Quantitative Genetics"=9,
                        "Week 7: Spatial Linear Models"=10,
                        "Week 8: Simulation"=11),
                  selected = 1
      )
    )
  )

  server <- function(input, output, session) {

    # Listen for 'done' events.
    shiny::observeEvent(input$done, {

      selectedFile <- paste0(c("Week0_BasicR",
                               "Week0_Graphics",
                               "Week1_vignette",
                               "Week2_vignette",
                               "Week2_bonus_vignette",
                               "Week3_vignette",
                               "Week4_vignette",
                               "Week5_vignette",
                               "Week6_vignette",
                               "Week7_vignette",
                               "Week8_vignette")[as.numeric(input$example)],
                             c(".html", ".Rmd", ".R")[as.numeric(input$type)])
      selectedPath <- (paste0(system.file("doc", selectedFile,
                       package = "LandGenCourse")))


      if (file.exists(file.path("downloads", selectedFile))) {
        cat("File already downloaded, skipping download. Opening file.", "\n");
      } else {
        file.copy(from=selectedPath, to=file.path("downloads", selectedFile))
        cat("Opening file.", "\n");
      }

      switch(input$type,
             "1" = utils::browseURL(file.path("downloads", selectedFile)),
             "2" = rstudioapi::navigateToFile(file.path("downloads",
                                                        selectedFile)),
             "3" = rstudioapi::navigateToFile(file.path("downloads",
                                                        selectedFile)))
#      switch(input$type,
#             "1" = utils::browseURL(paste0('file://', selectedPath)),
#             "2" = rstudioapi::navigateToFile(selectedPath),
#             "3" = rstudioapi::navigateToFile(selectedPath))

      cat(paste("Opening",selectedFile))
      shiny::stopApp()
    })

  }

  viewer <- shiny::dialogViewer("Choose Worked Example",
                                width = 400, height = 600)
  shiny::runGadget(ui, server, viewer = viewer)

}
