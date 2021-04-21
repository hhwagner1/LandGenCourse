---
description: >-
  This site provides access to the teaching materials included in the R package
  'LandGenCourse', which accompanies the distributed graduate course DGS
  Landscape Genetics.
---

# README

### Important links:

* [DGS Landscape Genetics](https://sites.google.com/site/landscapegeneticscourse/): public site of the distributed graduate course
* [R package 'LandGenCourse'](https://github.com/hhwagner1/LandGenCourse) on GitHub

### How to use this site

Use the navigation at the top to access key materials without installing the R package:

The teaching materials are structured in three parts with weekly modules. 

## Package website: LandGenCourse

[https://hhwagner1.github.io/LandGenCourse/](https://hhwagner1.github.io/LandGenCourse/)

* [**Articles**](https://hhwagner1.github.io/LandGenCourse/articles/index.html): Links to the weekly Worked Examples and Bonus Materials.
* [**Tutorials**](https://hhwagner1.github.io/LandGenCourse/tutorials/index.html): Links to the Course Videos that introduce statistical concepts, R code and datasets used in the Worked Examples.
* [**Reference**](https://hhwagner1.github.io/LandGenCourse/reference/index.html): List of R functions and R datasets included in LandGenCourse. The functions can be accessed via RStudio Add-is \(see below\). 
* [**Intro**](https://hhwagner1.github.io/LandGenCourse/intro/index.html): Links to the Course Videos that introduce statistical concepts, R code and datasets used in the Worked Examples.

## Description

This package provides an interface for accessing materials for the course 'Landscape Genetic Data Analysis with R'. It installs all R packages needed for the course and adds four RStudio Add-ins. The weekly labs are implemented as vignettes. Some weeks have additional worked examples as bonus materials.

### How to use the R package 'LandGenCourse'

**Note**: this package currently runs under R 3.6, it has not yet been migrated to R 4.0.

## How to install \(or update\)

```text
if (!require("devtools")) install.packages("devtools")
devtools::install_github("hhwagner1/LandGenCourse")
```

## How to use

The package installs four Add-ins in RStudio. Each will provide you with some dropdown menu choices.

* **Watch Course Video**: opens a video resource from the course "Landscape Genetic Data Analysis with R".
* **Start Tutorial**: installs swirl course "Landscape\_Genetics\_R\_Course" and prints instructions.
* **Choose Worked Example**: opens vignette file \(.html, .Rmd, or .R\) with a worked example from the course "Landscape Genetic Data Analysis with R".
* **Open Cheat Sheet**: opens selected R cheat sheet.

## Video instructions for beginners

This video walks through the process of installing, the course package, and using the RStudio Add-Ins. [Intro\_LandGenCourse\_small.mp4](https://www.dropbox.com/s/598kwim7x09m47t/Intro_LandGenCourse_small.mp4?dl=0)

