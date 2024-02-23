install.packages("tidyverse")
install.packages("rvest")

library("rvest")
library("tidyverse")
library(stringr)

temp = c("21", "ola", ".3aw7")
str_detect(temp, "\\d")

url = "https://www.imdb.com/chart/top/"
html = read_html(url)

# top movies
movieTitles = html |> html_elements("h3.ipc-title__text") |> html_text2() 
movieTitles = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_text2() |> str_replace("\\d+\\.\\s", "")

# cli-title-metadata
moviesYears = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_text2() |> str_extract("\\d{4}")
moviesYears = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_elements("div.cli-title-metadata") |> html_element("span.cli-title-metadata-item:nth-child(2)") |> html_text2()


# dataframe
dataframe = data.frame(movieTitles, moviesYears)

# todo usando a url "rvest.tidyverse.org/articles/starwars.html" raspagem do titulo, diretor e do ano do filme
