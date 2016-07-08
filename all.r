source("https://raw.githubusercontent.com/dominiqueemmanuel/tempo_files1/master/check_dependencies.r",encoding="UTF-8")
x<-paste0(tempfile(),".rds")
url<-"https://github.com/dominiqueemmanuel/tempo_files1/blob/master/packages_versions?raw=true"
request <- httr::GET(url)
writeBin(httr::content(request, type = "raw"), x)
compare_and_install_packages_version(file=x)
