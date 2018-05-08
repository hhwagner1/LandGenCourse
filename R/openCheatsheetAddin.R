openCheatsheetAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Open Cheat Sheet"),
    miniUI::miniContentPanel(

      shiny::selectInput("sheet", "Select a Cheat Sheet:",
                   list("List of R Functions by Tutorial (docx)"=1,
                        "List of R Functions by Worked Example (pdf)"=2,
                        "Base R"=3,
                        "R Markdown Language"=4,
                        "Data Import"=5,
                        "Data Transformation with dplyr"=6,
                        "Data Visualization with ggplot2"=7),
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
      utils::browseURL(paste0("file://", file.path(getwd(), "downloads",
                       paste0("RCommands_", now, ".docx"))))
    }

    if(input$sheet == "2")
    {
      file.copy(system.file("extdata", "Index_of_functions.pdf", package = "LandGenCourse"),
                  file.path(getwd(),"downloads", "Index_of_functions.pdf"))
      utils::browseURL(paste0("file://", file.path(getwd(), "downloads", "Index_of_functions.pdf")))
    }

    if(as.numeric(input$sheet) > 2)
    {
      selectedFile <- switch(input$sheet,
        "1" = "",
        "2" = "",
        "3" = "https://github.com/rstudio/cheatsheets/raw/master/base-r.pdf",
        "4" = "https://github.com/rstudio/cheatsheets/raw/master/rmarkdown-2.0.pdf",
        "5" = "https://github.com/rstudio/cheatsheets/raw/master/data-import.pdf",
        "6" = "https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf",
        "7" = "https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf")

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
