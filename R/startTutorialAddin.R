startTutorialAddin <- function() {

  course_name <- "Landscape_Genetics_R_Course"
  path <- file.path(swirl:::swirl_courses_dir(),
                    swirl:::make_pathname(course_name))
  if (file.exists(path)) unlink(path, recursive = TRUE, force = TRUE)

  swirl::install_course_github("hhwagner1", course_name)

  cat("\n","To start a tutorial:","\n",
      "- type: require(swirl)", "\n",
      "- type: swirl()", "\n",
      "- follow prompts", "\n",
      "- select course and tutorial","\n\n")

  cat("To stop and resume a tutorial:","\n",
      "- to stop and exit swirl, type: bye()","\n",
      "- to resume where you stopped, type: swirl()","\n\n")

  cat("To restart tutorial from beginning:","\n",
      "- type: swirl()","\n",
      "- use a different name", "\n",
      "  (simply add a number, like this: 'MyName2')")
}


