# **Project-2C**

## Grace Holliday and Damon D'Ambrosio

## Purpose

The purpose of this repo is to host the files necessary to complete Project 2. These include this very `README.md` file, along with the following:

- `project2.Rmd`, which serves as the backbone for all of the analyses, which are automated
- `render_w_automation.R`, which is used to knit `project2.Rmd` in an automated fashion and produce the requisite outputs
- Various `data_channel_is_X.html` files, which are created as a result of the automated analysis/knitting
- `OnlineNewsPopulatiry.csv`, which is the raw data used for analysis

## Required packages

The following packages are required:

- `rmarkdown`: Create and render the markdown files
- `tidyverse`: All manner of data manipulation/figure-making activities
- `caret`: Used to create/train the various models
- `gbm`: Necessary to create the boosted tree model
- `randomForest`: Necessary to create the random forest model

## Links to analyses

The links to the various automated analyses can be found below:

- [Business articles](data_channel_is_bus.html)
- [Entertainment articles](data_channel_is_entertainment.html)
- [Lifestyle articles](data_channel_is_lifestyle.html)
- [Social media articles](data_channel_is_socmed.html)
- [Tech articles](data_channel_is_tech.html)
- [World articles](data_channel_is_world.html)

## Code to create analyses

The following code, which exists as the `render_w_automation.R` separately in the repository, creates the linked analyses above via the `project2.Rmd` file:

```{r render_w_automation, eval = FALSE}
channels <- c("data_channel_is_lifestyle","data_channel_is_entertainment",
    "data_channel_is_bus",
    "data_channel_is_socmed",
    "data_channel_is_tech",
    "data_channel_is_world")

for(i in 1:6)
{rmarkdown::render("project2.rmd",
                  output_format="html_document",
                  params=list(datachannel=channels[i]),
                  output_file = channels[i])
}
```


