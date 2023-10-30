source("data-raw/setup.R")

# interreg-iotable-japan --------------------------------------------------

dir <- "data-raw/interreg-iotable-japan"
dir_create(dir)

# 1970--1990
url <- "https://www.meti.go.jp/statistics/tyo/tiikiio/result"
tribble(
  ~year, ~sector,
  1970, 43,
  1975, 43,
  1980, 43,
  1985, 45,
  1990, 46
) |> 
  mutate(url = str_c(url,
                     read_html(str_glue("{url}/result_3.html")) |> 
                       html_element("ul.marginLeft2em") |> 
                       html_elements("li > a") |> 
                       html_attr("href"),
                     sep = "/"),
         list(year, sector, url) |> 
           pmap(\(year, sector, url) {
             file <- path(dir, 
                          str_c(year, sector, 
                                sep = "-"),
                          ext = path_ext(url))
             if (!file_exists(file)) {
               curl::curl_download(url, file)
             }
           },
           .progress = TRUE))

# 1995
file <- path(dir, 
             str_c(1995, 46, 
                   sep = "-"),
             ext = "xlsx")
if (!file_exists(file)) {
  curl::curl_download("https://www.meti.go.jp/statistics/tyo/tiikiio/result/result_1/xls/h2rio95c.xlsx", file)
}

# 2005
file <- path(dir, 
             str_c(2005, 53,
                   sep = "-"),
             ext = "xlsx")
if (!file.exists(file)) {
  curl::curl_download("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000020467392&fileKind=0", file) 
}
