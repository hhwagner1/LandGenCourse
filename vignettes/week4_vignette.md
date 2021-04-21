---
title: 'Week 4: Metapopulation Genetics'
author: 'Nusha Keyghobadi, Thomas Lamy and Helene Wagner'
date: '`r Sys.Date()`'
show_toc: true
output:
  'knitr:::html_vignette':
    toc: 'yes'
    fig_width: 4
    fig_height: 3.5
vignette: >

  %\VignetteIndexEntry{Week 4: Metapopulation Genetics}
  %\VignetteEngine{knitr::rmarkdown} %\VignetteEncoding{UTF-8}
---

# Week4\_vignette

### 1. Overview of Worked Example

#### a\) Goals

This worked example shows how to:

* Assess the spatial distribution of genetic structure in a metapopulation using hierarchical AMOVA
* Relate site-specific Fst to patch connectivity and population size
* Relate site-specific genetic diversity to explanatory variables \(node-level analysis\)
* Assess temporal changes \(between years for same site\) and evidence for extinction events 
* Perform power analysis and sample size calculation for the temporal study

#### b\) Data set

Lamy et al. \(2012\) sampled the freshwater snail _Drepanotrema depressissimum_ in a fragmented landscape of tropical ponds on the island of Guadeloupe in the French West Indies. They used a spatially and temporally stratified sampling design with a total of 25 sites, where 12 sites formed four well-separated clusters of three neighbouring sites each, to study spatial variability, and 12 sites spread across the island were sampled in multiple years to study temporal variability. For each site and year, 22 - 34 individuals were genotyped at ten microsatellite loci. The species is diploid, hermaphroditic, and outcrossed.

A key characteristic of this system is the presence of a dry and a rainy season. In the dry season, many ponds can dry out, possibly causing extinction of the local snail populations. During the rainy season, ponds refill and can even overflow, thereby becoming connected through the hydrological network. During this rainy season, dispersal between ponds may occur.

* **dd.ecogen**: The dataset 'dd.ecogen' with genetic data for 1270 snails from 42 populations is included in package 'LandGenCourse'. To load it, type: data\(dd.ecogen\).
* **dd.site**: Population-level data from Tables 2 - 5 of Lamy et al. \(2012\) are available in dataset 'dd.site' \(with 25 variables\) in package 'LandGenCourse'. To load it, type: data\(dd.site\).

**Reference:**

Lamy, T., Pointier, J. P., Jarne, P. and David, P. \(2012\), Testing metapopulation dynamics using genetic, demographic and ecological data. Molecular Ecology, 21: 1394–1410. doi:10.1111/j.1365-294X.2012.05478.x

#### c\) Required R libraries

All required packages should have been installed already when you installed 'LandGenCourse'.

\`\`\`{r message=FALSE, warning=TRUE} library\(LandGenCourse\) library\(EcoGenetics\) library\(methods\) library\(ggplot2\)

## require\(tibble\)

## require\(poppr\)

## require\(ade4\)

## require\(pwr\)

## require\(effsize\)

## require\(sp\)

## require\(ggmap\)

## require\(car\)

```text
### d) Import data

Let's import the genetic data (spatial and temporal data sets combined, 42 combinations of site and year). Use '?dd.ecogen' to check helpfile with data set desription.

The 'ecogen' object 'dd.ecogen' contains individual-level data in the following slots:

- **XY**: Spatial coordinates (lat-long format)
- **G**: Microsatellite loci (columns = loci, rows = individuals)
- **A**: Table of allele frequencies (columns = alleles, rows = individuals)
- **S**: Structure variables (SiteID, SITE, YEAR, Cluster)

```{r}
data(dd.ecogen, package = "LandGenCourse")
dd.ecogen
?dd.ecogen
```

We also import site-level data from Tables 2 - 5 in Lamy et al. \(2012\). Use '?dd.site' to check helpfile with data set desription of the variables.

```text
data(dd.site, package = "LandGenCourse")
tibble::as_tibble(dd.site)
?dd.site
```

**Questions**: with the help file for 'dd.site', check the meaning of the following explanatory variables:

* What does 'APE' refer to, and how is it different from 'APA'?
* What does 'NLT' represent, and is it calculated independently from 'Size'?
* What does 'Type' mean, and what about 'V' and 'D'?
* To understand how connectivity 'C' and stability 'Stab' were calculated, you'll need to consult Lamy et al. \(2012\).

**Your hypothesis**: which explanatory variables would you expect to affect:

* Genetic diversity within local populations?
* Genetic differentiation among local populations?
* Both?

In the following, we'll perform three types of analyses:

* Compare 25 populations in space, across the island of Guadeloupe.
* Compare 12 populations in 4 clusters: differentiation within vs. among clusters?
* Compare 12 sites over time, some of which experienced a local extinction event.

### 2. Spatial distribution of genetic structure

How similar are populations from nearby habitat patches compared to populations across the island? To answer this question, we perform a hiearchical AMOVA \(analysis of molecular variance\) with individuals from 12 populations that form 4 clusters with 3 populations each.

#### a\) Creating a 'genind' object with the hierarchical data set

First, we need to extract the samples that belong to the hierarchical data set. There are four clusters: "North", "East", "Center" and "South". We are looking for the observations where the variable "Cluster" has one of these four values, all other observations will have a missing value for "Cluster". We can use '!is.na' to extract all rows with non-missing values. Then we convert to a 'genind' object.

```text
dd.ecogen.Cluster <- dd.ecogen[!is.na(dd.ecogen[["S"]]$Cluster),]
dd.genind.Cluster <- EcoGenetics::ecogen2genind(dd.ecogen.Cluster)
```

#### b\) Hierarchical AMOVA

There are several implementations of AMOVA in R, e.g. in pacakges 'ade4', 'pegas' and 'vegan'. The 'ade4' implementation is closest to the original implementation in Arlequin. Package 'poppr' has a wrapper function 'poppr.amova' that makes it easy to perform AMOVA with the 'ade4' or with the 'pegas' implementation \(see '?poppr.amova' for a discussion of their pros and cons\). Here we'll use 'ade4'.

* The first argument is the 'genind' object. 
* The argument 'hier' defines the hierarchy, with the top level first  \(i.e., here SITE is nested within Cluster\). The variables are expected to be found in the @strata slot of the 'genind' object. 
* The argument 'within=FALSE' specifies that within-individual variance \(i.e., observed heterozygosity\) should not be tested. Setting this to 'TRUE' can lead to problems with missing values. 

First we run the AMOVA and estimate the percent of molecular variance at each hierarchical level.

```text
amova.result <- poppr::poppr.amova(dd.genind.Cluster, hier = ~ Cluster/SITE, 
                                   within=FALSE, method = "ade4")
amova.result
```

Then we test whether each variance component is statistically significant \(i.e., significantly larger than zero\).

```text
amova.test <- ade4::randtest(amova.result, nrepet = 999)
amova.test
```

**Questions**:

* At what level is there stronger differentiation, within or among clusters? 
* What does this mean biologically?
* Are both levels statistically significant?

### 3. What determines genetic differentiation among sites?

What factors explain site-specific Fst? Let's consider the key micro-evolutionary processes:

* **Genetic drift**: the smaller the population, the higher the rate of drift, hence we expect higher differention for small populations. Predictor: long-term population size 'NLT'.
* **Gene flow**: gene flow homogenizes allele frequencies, hence we expect less differentiation for well connected patches. Predictors: connectivity 'C', density of favorable habitat 'D' \(within 2 km radius\).

First, we create a new SpatialPointsDataFrame with the subset of data for the spatial analysis \(25 ponds, one year each\).

```text
dd.spatial <- dd.site[dd.site@data$Spatial==TRUE, ]
```

#### a\) Correlation matrix

Let's start with a correlation matrix.

```text
cor(dd.spatial@data[ , c("FST.GESTE", "NLT", "C", "D")], 
    use="pairwise.complete")
```

**Questions**:

* Is there genetic evidence for higher drift in small populations?
* Is there genetic evidence for higher gene flow among well connected patches?
* Are the two factors confounded for this data set?
* Would you prefer 'C' or 'D' to quantify patch connectivity? Does it matter? What does this mean biologically?

#### b\) Scatterplots

Let's plot the response variable FST.GESTE against each of the two predictors NLT and C. Here, we use functions from the package `ggplot2` \(already loaded\) to define two ggplot objects `NLT.plot` and `C.plot`, then we plot them side-by-side with the function `cowplot::plot_grid`.

* For NLT.plot, we define the dataset as `dd.spatial@data`, the x-axis as variable NLT, the y-axis as variable FST.GESTE, and the labels as variable SITE. 
* We add points with geom\_point.
* We add a regression line \(`geom_smooth`\), make it linear \(`method = lm`\) and add a shaded area for plus/minus 1 SE of the mean for a given value of x \(`se = TRUE`\).
* We add backfilled labels \(`geom_label`\), define their size \(`size`\), and move them up a little along the y-axis \(`nudge_y`\)

\`\`\`{r fig.height=4, fig.width=8} NLT.plot &lt;- ggplot\(dd.spatial@data, aes\(x=NLT, y=FST.GESTE, label=SITE\)\) + geom\_point\(\) + geom\_smooth\(method = lm, se = TRUE\) + geom\_text\(size=2, nudge\_x=0, nudge\_y=0.01, check\_overlap=TRUE\)

C.plot &lt;- ggplot\(dd.spatial@data, aes\(x=C, y=FST.GESTE, label=SITE\)\) + geom\_point\(\) + geom\_smooth\(method = lm, se = TRUE\) + geom\_text\(size=2.5, nudge\_x=0, nudge\_y=0.01, check\_overlap=TRUE\)

cowplot::plot\_grid\(NLT.plot, C.plot\)

```text
### c) Regression model

The two predictors 'NLT' and 'C' are not strongly correlated. We'll fit a regression model with both predictors. Here we use function 'scale' to standardize each variable, so that we can interpret the regression slope coefficients as partial correlation coefficients (beta coefficients). 

```{r}
mod.diff <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.spatial)
summary(mod.diff)
```

Is the model valid? Let's check the residual plots:

\`\`\`{r fig.height=7, fig.width=8} par\(mfrow=c\(2,2\)\) plot\(mod.diff, labels.id = dd.spatial@data$SITE\) par\(mfrow=c\(1,1\)\)

```text
If we had more than two predictors, it would be a good idea to calculate variance inflation factors. The package 'car' has a function 'vif' that takes as argument a fitted model. Here, both predictors have VIF = 1.007, which indicates no collinearity. 

```{r}
car::vif(mod.diff)
```

#### d\) Which populations don't fit the general pattern?

Let's plot the residuals in space. The function 'bubble' from the package 'sp' evaluates the projection information of the SpatialPointsDataFrame 'dd.spatial'.

```text
dd.spatial@data$Residuals <- mod.diff$residuals
sp::bubble(dd.spatial, zcol = "Residuals", col = c("red", "blue"))
```

Or on a map from the internet, using 'qmplot' from the 'ggmap' package. It expects lat-lon coordinates that are stored in a data frame, not a SpatialPointsDataFrame. If we convert 'dd.spatial' with the function 'as.data.frame', R will return a data frame with the site variables and with the coordinates appended as additional columns. The code below does the following \(see Week 4 video, part 2\):

* Creates an index `a` that lists identifies the two potential outliers by their SITE names \(logical\),
* Creates a vectors `a2` that contains the row numbers of the potential outliers, 
* Grabs a grayscale map from the internet, plots all ponds with size and color according to their residuals, and stores the resulting map in the object `myMap`.
* Plots the map and adds labels for the two potential outliers.The argument `size` affects font size and `label.padding` determines the size of the textbox for the label. 

\`\`\`{r fig.height=4.5, fig.width=7, message=FALSE} a &lt;- is.element\(dd.spatial@data$SITE, c\("DESB", "PTC"\)\) a2 &lt;- c\(1:nrow\(dd.spatial@data\)\)\[a\] myMap &lt;- ggmap::qmplot\(Longitude, Latitude, data = as.data.frame\(dd.spatial\), source = "stamen", maptype = "toner-lite", force=TRUE,  
col = sign\(Residuals\), size = abs\(Residuals\)\) myMap + ggplot2::geom\_label\(data = as.data.frame\(dd.spatial\[a2,\]\), mapping = ggplot2::aes\(Longitude, Latitude, label = SITE\), size = 4, label.padding = unit\(0.2, "lines"\), col = "black", vjust = 0, nudge\_x = 0, nudge\_y = -0.015\)

```text
Exports the last plot as a 'png' (Portable Network Graphics) file, specifying canvas size and resolution. To run the code, uncomment by removing '#' at the beginning of each line.

```{r message=FALSE}
#require(here)
#if(!dir.exists(paste0(here(),"/output"))) dir.create(paste0(here(),"/output"))
#ggplot2::ggsave(paste0(here(),"/output/ResidualMap.png"), 
#               width = 7, height = 5.5, units = "in",  dpi = 300)
```

What might explain the large residuals for the two sites 'PTC' and 'DESB'?

* Site 'PTC' lies on the tip of a peninsula and thus is very isolated geographically.
* Site 'DESB' is a very instable site that can frequently dry out during the dry season, as it is shallow and lies in the comparatively dry northern part of the island. In addition, although 'DESB' is surrounded by many ponds, these ponds never get connected to 'DESB' hydrologically during the rainy season. Therefore, immigration can only occur via cattle or birds, which are much less important drivers of gene flow than immigration by hydrological connectivity during the rainy season.  

#### e\) Regression model without outliers

We can use the same index 'a' to exclude the potential outliers from the regression model:

```text
mod.diff.minus2 <- lm(scale(FST.GESTE) ~ scale(NLT) + scale(C), 
               data=dd.spatial[-a2,])
summary(mod.diff.minus2)
```

* Did omitting the two sites improve model fit?
* Did it change the nature of the results?
* Does this affect the biologial interpretation?

\`\`\`{r fig.height=7, fig.width=8} par\(mfrow=c\(2,2\)\) plot\(mod.diff.minus2, labels.id = dd.spatial@data$SITE\[-a2\]\) par\(mfrow=c\(1,1\)\)

```text
## 4. What determines genetic diversity?

- Can the same predictors (population size and connectivity) explain genetic diversity? 
- Is patch size ('Size') a good proxy for population size (as often used in ecological studies)?
- Which measure of genetic diversity shows the stronger response, allelic richness (rarefied) or expected heterozygosity?

### a) Correlation matrix

```{r}
cor(dd.spatial@data[, c("RA", "He", "Size", "NLT", "C", "D")],
    use="pairwise.complete")
```

**Questions**:

* How strongly are the two diversity measures 'RA' and 'He' correlated?
* Are 'NLT' and 'C' strongly correlated with the diversity measures 'RA' and 'He'? 
* Is the correlation with 'Size' similarly strong as the correlation with 'NLT'? 
* How strongly are 'Size' and 'NLT' correlated with each other? 
* Does 'D' show a stronger correlation with diversity than with differentiation?

#### b\) Regression models

For allelic richness:

```text
mod.RA <- lm(scale(RA) ~ scale(NLT) + scale(C), data = dd.spatial)
summary(mod.RA)
```

\`\`\`{r fig.height=7, fig.width=8} par\(mfrow=c\(2,2\)\) plot\(mod.RA, labels.id = dd.spatial@data$SITE\) par\(mfrow=c\(1,1\)\)

```text
For gene diversity (expected heterozygosity):

```{r}
mod.He <- lm(scale(He) ~ scale(NLT) + scale(C), data = dd.spatial)
summary(mod.He)
```

\`\`\`{r fig.height=7, fig.width=8} par\(mfrow=c\(2,2\)\) plot\(mod.He, labels.id = dd.spatial@data$SITE\) par\(mfrow=c\(1,1\)\)

```text
## 5. Are genetic differentiation and diversity related?

Would you expect a relationship between genetic diversity and genetic differentiation of individual patches?

Lets examine the correlation between gene diversity (He) and site-specific Fst:

```{r}
cor(dd.site$He, dd.site$FST.GESTE, use = "pairwise.complete")
```

There are a number of possible reasons for such a correlation. Can you put forward some hypotheses to explain this relationship? See Lamy et al. \(2012\) for their interpretation.

### 6. Effect of recent extinction events

Several patches fell dry between observation years, which is assumed to signify extinction of the local population. Does genetic evidence support this interpretation, i.e., is there genetic evidence of bottlenecks or founder effects in _D. depressissimum_?

#### a\) Effect of patch extinction event \(temporal data set\)

```text
dd.temporal <- dd.site[dd.site@data$MultiYear==TRUE,]
cor(dd.temporal@data[, c("Fst.temp", "APE", "NLT", "C")], 
    use="pairwise.complete")
```

We can compare a number of competing models using the Akaike Information Criterion \(AIC\). Models with lower AIC are better \(see Week 12\).

```text
mod.Fst.temp <- lm(scale(Fst.temp) ~ scale(APE), data=dd.temporal)
summary(mod.Fst.temp)

mod.Fst.temp.C <- lm(scale(Fst.temp) ~ scale(APE) + scale(C), 
                     data=dd.temporal)
mod.Fst.temp.NLT <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT), 
                       data=dd.temporal)
mod.Fst.temp.both <- lm(scale(Fst.temp) ~ scale(APE) + scale(NLT) + scale(C),  
            data=dd.temporal)
AIC(mod.Fst.temp, mod.Fst.temp.C, mod.Fst.temp.NLT, mod.Fst.temp.both)
```

The best model includes neither 'C' nor 'NLT'. Note that 'APE' is a binary variable, so in essence we're performing a t-test here.

```text
res.Fst.temp <- t.test(Fst.temp ~ APE, data=dd.temporal, alternative = "less")
res.Fst.temp
```

#### b\) Power analysis

The effect is not statistically significant. Does that mean that we found no effect of apparent population extinctions on temporal Fst? Let's check effect size. For means, Cohen's effect size is measured by d \(which is measured in units of standard deviations\):

* small effect: d &gt; 0.2 \(means at least 0.2 standard deviations apart\)
* medium effect: d &gt; 0.5
* large effect: d &gt; 0.8 

We can let R calculate effect size for us:

```text
effsize::cohen.d(Fst.temp ~ factor(APE), data=dd.temporal)
```

So, we actually found a 'medium' effect \(more than 0.5 standard deviations difference between group means\). Maybe sample size was too small to have sufficient power?

Let's check sample size:

```text
table(dd.temporal$APE[!is.na(dd.temporal$Fst.temp)])
```

Ah, that explains a lot. There were only 5 sites with apparent extinction, and 7 without.

Given that sample size, what was the statistical power of our test to detect at least a large effect \(d = - 0.8\), i.e., be able to reject the null hypothesis if such an effect is present in the population from which we sampled?

```text
pwr::pwr.t2n.test(n1=7, n2=5, d=-0.8, alternative = "less")
```

So the power to detect at least a large effect, if it exists in the population, was only 0.355, way below the 0.8 \(or even 0.95\) that we would want to see. For a medium effect, the power is even smaller.

#### c\) Sample size calculation

How large a sample would we have needed in each group to achieve a power of 0.8 to detect a large effect? And for a medium effect?

```text
pwr::pwr.t.test(power = 0.8, d = -0.8, alternative = "less")
pwr::pwr.t.test(power = 0.8, d = -0.5, alternative = "less")
```

More than 20 sites in each group would have been needed to detect a large effect, or more than 50 per group to detect a medium effect, with a power of 80%.

Hence, these particular results are inconclusive. There was a trend showing a large effect size but power was very low. This aspect of the study should ideally be repeated with a larger sample size before reaching any conclusions.

Note however that using additional evidence \(e.g., population assignment tests\), Lamy et al. \(2012\) concluded that extinctions were in fact less common in this system than previously assumed – in many cases of apparent extinction, individuals may still be present but just not detected.

### 7. R Exercise Week 4

**Task:** Build on your previous exercises and plot the sites on a map downloaded from the internet. Explore the relationships between Hexp, census population size and percent forest cover within 500 m of the site \(forest may act as a barrier for grassland plants\).

**Hints:**

a\) **Load packages**: You may want to load the packages `dplyr` and `ggplot2`. Alternatively, you can use `::` to call functions from packages. b\) **Import your datasets from Weeks 2 & 3 R Exercises**. Here's an example for your code, adapt it as needed to import the R objects "Pulsatilla.longlat.rds" \(SpatialPointsDataFrame, Week 2\) and "H.pop.rds" \(Week 3\) that you saved previously: `Pulsatilla.longlat <- readRDS(paste0(here::here(), "/output/Pulsatilla.longlat.rds"))` c\) **Plot sites on map from internet**: To get started, you may use this code \(note the names of the variables with the spatial coordinates\): `myMap <- ggmap::qmplot(x=meanX, y=meanY, data = as.data.frame(Pulsatilla.longlat), source = "stamen", maptype = "toner-lite")` Next, modify code from section 3.d to add labels for all sites and plot the map. d\) **Combine data**: Use the function `dplyr::left_join` to add the variables from the dataset `H.pop` to the data frame in the @data slot of `Pulsatilla.longlat`. Notes:

* This is important, as the order of populations may not be the same in the two datasets. 
* Remember to check the structure of the datasets \(variable names and types\) first so that you know which are the ID variables that you can use to match sites. 
* If the two ID variables are not of the same type \(e.g., one if a `factor`, the other is `character`\), it is best to change the format of one \(e.g., with `as.character`\) before doing the left-join.

e\) **Scatterplot with regression line**: Create a scatterplot of Hexp \(y axis\) plotted against nIndiv \(x axis\). Add a regression line and, if possible, label points. You may modify code from section 3.b using the `ggplot2` package, or use base R functions.  
f\) **Regression analysis**: Adapt code from section 3.c to perform a regression of `Hexp` \(response variable\) on the predictor `nIndiv`. Create residual plots and inspect them. What is the main issue here?

**Questions:** There is one influential point in the regression analysis:

* Which site is it?
* Where is it located \(north / east / south / west\)?
* What makes it an influential point \(large residual, leverage, or both\)?
* What would happen to the regression line and the R-squared if this point was omitted?

\`\`\`{r message=FALSE, warning=TRUE, include=FALSE} LandGenCourse::detachAllPackages\(\)

\`\`\`

