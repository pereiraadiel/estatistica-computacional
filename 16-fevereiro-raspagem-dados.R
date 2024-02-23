# WEB Scraping

library(rvest)
library(dplyr)

url = "https://www.bbc.com/news/world-us-canada-68175846"

html = read_html(url)


newsTitle = html |> html_elements("h1") |> html_text2()
newsLinks = html |> html_elements("a") |> html_text2()

# top movies
moviesUrl = "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html = read_html(moviesUrl)

movieTitles = html |> html_elements("h3.ipc-title__text") |> html_text2() |> head(10)
movieList = html |> html_elements("ul.ipc-metadata-list") |> html_elements("li") |> html_text2() |> head(10)

