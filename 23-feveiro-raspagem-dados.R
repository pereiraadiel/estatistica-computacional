install.packages('rvest')
install.packages('tidyverse')
library(rvest)
library(tidyverse)
library(stringr)

# imdb
url  = "https://www.imdb.com/chart/top"

html = read_html(url)

movieTitles = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_text2() |> str_replace("\\d+\\.\\s", "")

moviesYears = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_elements("div.cli-title-metadata") |> html_element("span.cli-title-metadata-item:nth-child(1)") |> html_text2()
moviesClassification = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_elements("div.cli-title-metadata") |> html_element("span.cli-title-metadata-item:nth-child(3)") |> html_text2()

top250 = data.frame(movieTitles, moviesYears, moviesClassification)

getwd()
write.csv(top250, file= "top250.csv")
write.table(top250, file= "top250.csv", sep=',', row.names=FALSE)

# guns violence by state wisevoter.com
url = "https://wisevoter.com/state-rankings/gun-violence-by-state/"

html = read_html(url)

states = html |> html_elements("table.shdb-on-page-table") |> html_table() |> as.data.frame()

states = states[, -1]

taxa= as.numeric(str_replace(states[,4], "\\%", ""))
states$Gun.Ownership.Rate = taxa

mortes = states$Gun.Death.Rate = str_replace(states$Gun.Death.Rate, " per 100k", "")
states$Gun.Death.Rate = as.numeric(mortes)

states$Red.or.Blue.State = as.factor(states$Red.or.Blue.State)

ggplot(data = states, aes(x=Gun.Ownership.Rate, y=Gun.Death.Rate, color=Red.or.Blue.State))+geom_point()
