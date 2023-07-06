channels <- c("data_channel_is_lifestyle","data_channel_is_entertainment",
    "data_channel_is_bus",
    "data_channel_is_socmed",
    "data_channel_is_tech",
    "data_channel_is_world")

for(i in 1:6)
{rmarkdown::render("project2.Rmd",
                  output_format="html_document",
                  params=list(datachannel=channels[i]),
                  output_file = channels[i])
}

    