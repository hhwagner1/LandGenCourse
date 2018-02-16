openCheatsheetAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Open Cheat Sheet"),
    miniUI::miniContentPanel(

      shiny::selectInput("sheet", "Select a Cheat Sheet:",
                   list("List of R Functions by Tutorial (doc)"=1,
                        "Base R"=2,
                        "R Markdown Language"=3,
                        "Data Import"=4,
                        "Data Transformation with dplyr"=5,
                        "Data Visualization with ggplot2"=6),
                  selected = 1
      )
    )
  )

  server <- function(input, output, session) {

    # Listen for 'done' events.
    shiny::observeEvent(input$done, {

    if(!dir.exists(file.path(getwd(), "downloads")))
    {
      dir.create(file.path(getwd(), "downloads"), FALSE)
    }

    if(input$sheet == "1")
    {
      now<-format(Sys.time(), "%b%d%H%M%S")
      file.copy(system.file("extdata", "RCommands.docx", package = "LandGenCourse"),
                file.path(getwd(),"downloads", paste0("RCommands_", now, ".docx")))
#      utils::download.file(paste0("file://", system.file("extdata", "RCommands.docx",
#                                                         package = "LandGenCourse")),
#             destfile=file.path("downloads", paste0("RCommands_", now, ".docx")),
#                           mode="wb")
      utils::browseURL(paste0("file://", file.path(getwd(), "downloads",
                                                   paste0("RCommands_", now, ".docx"))))
    }

    if(input$sheet != "1")
    {
      selectedFile <- switch(input$sheet,
        "1" = "",
        "2" = "https://github.com/rstudio/cheatsheets/raw/master/base-r.pdf",
        "3" = "https://github.com/rstudio/cheatsheets/raw/master/rmarkdown-2.0.pdf",
        "4" = "https://github.com/rstudio/cheatsheets/raw/master/data-import.pdf",
        "5" = "https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf",
        "6" = "https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf")

      utils::download.file(selectedFile,
                         destfile=file.path("downloads", basename(selectedFile)),
                         mode="wb")
      utils::browseURL(file.path("downloads", basename(selectedFile)))
    }


    cat(paste0("Hints:
- 'List of R Functions by Tutorial' will open as Word file (docx).
- File will be saved to folder 'downloads' in active working directory.
- A timestamp (date and time) is added to file name to avoid overwriting your notes.
- Add your notes and save under a new name and/or destination!
- All other cheat sheets will open in your default PDF viewer.
- They are also saved to folder 'downloads' in your active working directory."))


      shiny::stopApp()
    })

  }

  viewer <- shiny::dialogViewer("Open a Cheat Sheet",
                                width = 400, height = 400)
  shiny::runGadget(ui, server, viewer = viewer)

}
