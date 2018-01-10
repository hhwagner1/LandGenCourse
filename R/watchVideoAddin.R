watchVideoAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Watch a Course Video"),
    miniUI::miniContentPanel(

      shiny::selectInput("type", "What do you want to open?",
                         list("Video only (in web browser)"=1,
                              "Slides only (as PDF)"=2,
                              "Video and slides"=3),
                         selected = 1
      ),
      shiny::selectInput("video", "Select a Topic:",
                   list("Week 0: Intro to RStudio"=1,
                        "Week 0: Intro to R course package"=2,
                        "Week 0: Intro to R Notebooks"=3,
                        "Week 0: Version Control 101"=4,
                        "Week 1: Getting LG Data into R"=5,
                        "Week 2: Land Cover Analysis in R"=6,
                        "Week 3: Genetic Diversity (Part 1)"=7,
                        "Week 3: Genetic Diversity (Part 2)"=8,
                        "Week 5: Spatial Statistics"=9,
                        "Week 6: Linear (Mixed) Models in R (Part 1)"=10,
                        "Week 6: Linear (Mixed) Models in R (Part 2)"=11),
                  selected = 5
      )
    )
  )





  server <- function(input, output, session) {

    # Listen for 'done' events.
    shiny::observeEvent(input$done, {

      selectedVideo <- c(
        "https://www.dropbox.com/s/4jogqfu3xpeuged/Intro_RStudio_small.mp4?dl=0",
        "https://www.dropbox.com/s/598kwim7x09m47t/Intro_LandGenCourse_small.mp4?dl=0",
        "https://www.dropbox.com/s/hxij2c18hckc1uv/Intro_RNotebooks.mp4?dl=0",
        "http://sho.co/19EFD",
        "http://sho.co/19DCV",
        "http://sho.co/19DA2",
        "http://sho.co/19UT5",
        "http://sho.co/19UT6",
        "http://sho.co/19UM3",
        "http://sho.co/19SXN",
        "http://sho.co/19SXK")[as.numeric(input$video)]

      selectedSlides <- c("", "", "",
                          "Week0_Slides.pdf",
                          "Week1_Slides.pdf",
                          "Week2_Slides.pdf",
                          "Week3_Slides.pdf",
                          "Week3_Slides.pdf",
                          "Week5_Slides.pdf",
                          "Week6_Slides.pdf",
                          "Week6_Slides.pdf")[as.numeric(input$video)]

      if(!dir.exists(file.path("./downloads")))
      {
        dir.create(file.path("./downloads"), FALSE)
      }

      if(as.numeric(input$type) != 2) utils::browseURL(selectedVideo)
      if(as.numeric(input$type) != 1 && selectedSlides!="")
      {

        if (file.exists(file.path("downloads", selectedSlides))) {
            cat("Slides already downloaded, skipping download. Slides will open in default PDF viewer.", "\n");
        } else {
          utils::download.file(paste0("https://github.com/hhwagner1/DGS_LG_Labs/raw/master/docs/Video_slides/", selectedSlides),
                               destfile=file.path("downloads", selectedSlides), mode="wb")
          cat("Slides will open in default PDF viewer.", "\n");
           }
        utils::browseURL(file.path("downloads", selectedSlides))
      }


      cat(paste0("Video location: ",selectedVideo, "\n\n"))
      if(selectedSlides=="")cat(paste0("No slides available for this video.", "\n\n"))
      cat("Hints:
- Videos open in your default web browser.
- Make video fullscreen by clicking on screen symbol (bottom right).
- Slides are saved to folder 'download' in your active working directory.")

      shiny::stopApp()
    })

  }

  viewer <- shiny::dialogViewer("Watch a Course Video",
                                width = 500, height = 600)
  shiny::runGadget(ui, server, viewer = viewer)

}
