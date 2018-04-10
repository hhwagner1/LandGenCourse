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
                        "Week 8: Simulation Experiments"=11,
                        "Week 9: Population Structure"=12,
                        "Week 10: Landscape Resistance"=13,
                        "Week 11: Detecting Adaptation"=14,
                        "Week 12: Model Selection"=15,
                        "Week 13: Gravity Models"=16),
                  selected = 1
      )
    )
  )

  server <- function(input, output, session) {

    # Listen for 'done' events.
    shiny::observeEvent(input$done, {

    # Input and output file names:
      baseName <- c("Week0_BasicR",
                    "Week0_Graphics",
                    "Week1_vignette",
                    "Week2_vignette",
                    "Week2_bonus_vignette",
                    "Week3_vignette",
                    "Week4_vignette",
                    "Week5_vignette",
                    "Week6_vignette",
                    "Week7_vignette",
                    "Week8_vignette",
                    "Week9_vignette",
                    "Week10_vignette",
                    "Week11_vignette",
                    "Week12_vignette",
                    "Week13_vignette")[as.numeric(input$example)]

      suffix <- c(".html", ".Rmd", ".R")[as.numeric(input$type)]
      selectedFile <- paste0(baseName, suffix)

      selectedPath <- (paste0(system.file("doc", selectedFile,
                                          package = "LandGenCourse")))
      now<-format(Sys.time(), "%b%d%H%M%S")
      outFile <- paste0(baseName, "_", now, suffix)

      # Create folders as necessary:
      if(!dir.exists(file.path("downloads")))
        dir.create(file.path("downloads"), recursive=TRUE)
      if(!dir.exists(file.path("output")))
        dir.create(file.path("output"), recursive=TRUE)

      if(as.numeric(input$example)==11 && as.numeric(input$type)==2)
      {
        if(!dir.exists(file.path("output","simout")))
          dir.create(file.path("output","simout"), recursive=TRUE)
      }

      if(as.numeric(input$example)==12)
      {
        file.copy(from=system.file("extdata", "WE9_Fig1.png", package = "LandGenCourse"),
                  to=file.path("downloads", "WE9_Fig1.png"))
        file.copy(from=system.file("extdata", "WE9_Fig2.png", package = "LandGenCourse"),
                  to=file.path("downloads", "WE9_Fig2.png"))
      }
      if(as.numeric(input$example)==15)
      {
        file.copy(from=system.file("extdata", "WE12_Fig1.png", package = "LandGenCourse"),
                  to=file.path("downloads", "WE12_Fig1.png"))
      }
      if(as.numeric(input$example)==16)
      {
        file.copy(from=system.file("extdata", "WE13_Fig1.png", package = "LandGenCourse"),
                  to=file.path("downloads", "WE13_Fig1.png"))
      }

      # Copy file to 'downloads' folder:
      file.copy(from=selectedPath, to=file.path("downloads", outFile))
      cat(paste("File saved as: ",
                file.path(here::here(),"downloads", outFile), "\n"))

      # Open file:
      switch(input$type,
             "1" = utils::browseURL(file.path("downloads", outFile)),
             "2" = rstudioapi::navigateToFile(file.path("downloads",
                                                        outFile)),
             "3" = rstudioapi::navigateToFile(file.path("downloads",
                                                        outFile)))

      cat(paste("Opening",outFile))
      shiny::stopApp()
    })

  }

  viewer <- shiny::dialogViewer("Choose Worked Example",
                                width = 400, height = 600)
  shiny::runGadget(ui, server, viewer = viewer)

}
