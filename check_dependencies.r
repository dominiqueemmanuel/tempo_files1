###################################################################
##### ATTENTION ###################################################
###################################################################
## 1. Pour exécuter ce script il faut que getwd() vale
##   [Mon disuqe]/Stat_Regie/data/application_data
## 2. Ne pas exécuter la fonction create_packages_versions sauf besoin explicite
###################################################################
###################################################################

# devtools::install_github("dominiqueemmanuel/graphpdd")
# devtools::install_github("dominiqueemmanuel/verbatim.utils")


### installation des packages de base pour ce script s'ils ne
### sont pas installé

if (!("BiocInstaller" %in% installed.packages()[, "Package"])) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("BiocInstaller", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
}
if (!("devtools" %in% installed.packages()[, "Package"])) {
  install.packages("devtools")
}
if (!("rsconnect" %in% installed.packages()[, "Package"])) {
  devtools::install_github("rstudio/rsconnect")
}
## Fonction  n'utiliser que de temps en temps, depuis la version de développement (idéalement toujours le même utilisateur, celui qui centralise les infos des version des packages)
create_packages_versions <- function(file = "packages_versions") {
  ## Cette fonction crée un fichier rds contenant les version
  ## des packages
  library(rsconnect)
  library(BiocInstaller)
  library(devtools)
  enc_before <- getOption("encoding")
  on.exit(options(encoding = enc_before))
  options(encoding = "UTF-8")
  pkg <- appDependencies()
  pkg$RemoteUsername <- NA
  pkg$RemoteRepo <- NA
  pkg$RemoteSha <- NA
  for (k in seq_along(pkg[, 1])) {
    p <- pkg[k, ]$package
    d <- packageDescription(p)
    
    if(!is.null(d$RemoteUsername)) pkg$RemoteUsername[k] <- d$RemoteUsername
    if(!is.null(d$RemoteRepo)) pkg$RemoteRepo[k] <-d$RemoteRepo
    if(!is.null(d$RemoteSha)) pkg$RemoteSha[k] <- d$RemoteSha
  }
  saveRDS(pkg, file)
}
# create_packages_versions()

compare_and_install_packages_version <- function(file = "packages_versions", sink_file = NULL){#"log_check_dependencies.txt") {

  install_version2 <- function (package, version = NULL, repos = getOption("repos"), 
                                type = getOption("pkgType"), ...) 
  {
    contriburl <- contrib.url(repos, type)
    available <- available.packages(contriburl)
    if (package %in% row.names(available)) {
      current.version <- available[package, "Version"]
      if (is.null(version) || version == current.version) {
        return(install.packages(package
                                # , repos = repos,
                               # , contriburl = contriburl
                                # , type = type
                                ,dependencies=FALSE))
      }
    }
    info <- devtools:::package_find_repo(package, repos)
    if (is.null(version)) {
      package.path <- info$path[NROW(info)]
    }
    else {
      package.path <- paste(package, "/", package, "_", version, 
                            ".tar.gz", sep = "")
      if (!(package.path %in% info$path)) {
        stop(sprintf("version '%s' is invalid for package '%s'", 
                     version, package))
      }
    }
    url <- paste(info$repo[1L], "/src/contrib/Archive/", package.path, 
                 sep = "")
    install_url(url, ...)
  }
  
  if(!is.null(sink_file))sink(file = sink_file)
  library(rsconnect)
  library(BiocInstaller)
  library(devtools)
  pkg <- readRDS(file)
  pkg <- pkg[order(pkg$source,decreasing = TRUE),]
  for(iter in 1:2){## au cas où les installation des dépendances à la première passe étaient différentes de celles souhaitées
    for (k in seq_along(pkg[, 1])) {
      # k<-21
      p <- pkg[k, ]$package
      v <- pkg[k, ]$version
      s <- pkg[k, ]$source
      print("########################################################################################")
      print(paste0(p," : ",tryCatch(packageVersion(p),error=function(e)as.character(e))))
      RemoteUsername <- pkg[k, ]$RemoteUsername
      RemoteRepo <- pkg[k, ]$RemoteRepo
      RemoteSha <- pkg[k, ]$RemoteSha
      d <- packageDescription(p)
      
      install_from_cran <- function(){
        print(paste0("Package : ", p))
        if (is.na(d)) {
          print("Currently not installed")
        } else {
          print(paste0("Currently installed but with version : ",
                       d$Version))
        }
        print(paste0("Trying to install version ", v,
                     " from ", s))
        err <- tryCatch({
          install_version2(package = p,
		  version = v,
		  repos = "http://cran.us.r-project.org",
                          type = "source",
						  dependencies=FALSE
						  ,upgrade_dependencies = FALSE
						  )
          FALSE
        }, error = function(e) list(TRUE, e))
        if (!err[[1]])
          print("Installation  : OK")
        if (err[[1]])
          print(paste0("ERREUR Installation  : KO : ",
                       as.character(err[[2]])))
      }
      
      if (s == "CRAN") {
        if (is.na(d) || d$Version != v) {
          #install_from_cran()
           if(p != 'rJava'){
               install_from_cran()
            } else{
               #install.packages('rJava', repos='http://www.rforge.net/', configure.args='--disable-Xrs')
               print("======================")
               print("Not install rJava")
               print("======================")
            }
        }
      } else if (s == "github") {
        
        if (is.na(d) || d$Version != v || !identical(d$RemoteSha,RemoteSha)) {
          if(is.na(RemoteSha)){expre_sha<-""
          } else {
            expre_sha<-paste0("@", RemoteSha)
          }
          if(is.na(RemoteUsername) | is.na(RemoteRepo)){
            pkg[k, ]$source<-"CRAN"
            s<-"CRAN"
            #install_from_cran()
             if(p != 'rJava'){
               install_from_cran()
            } else{
               #install.packages('rJava', repos='http://www.rforge.net/', configure.args='--disable-Xrs')
               print("======================")
               print("Not install rJava")
               print("======================")
            }
            
          } else {
            print(paste0("Package : ", p))
            if (is.na(d)) {
              print("Currently not installed")
            } else {
              print(paste0("Currently installed but with version : ",
                           d$Version))
            }
            print(paste0("Trying to install version ", v,
                         " from ", s, " : ", RemoteUsername, "/",
                         RemoteRepo,expre_sha))
            err <- tryCatch({
              install_github(paste0(RemoteUsername, "/",
                                    RemoteRepo, expre_sha),
                             args = "--no-test-load",
                             upgrade_dependencies = FALSE,
                             dependencies = FALSE
                             )
              FALSE
            }, error = function(e) list(TRUE, e))
            if (!err[[1]])
              print("Installation  : OK")
            if (err[[1]])
              print(paste0("ERREUR Installation  : KO : ",
                           as.character(err[[2]])))
          }
        }
        
      } else if (s == "Bioconductor") {
        
        if (is.na(d) || d$Version != v) {
          print(paste0("Package : ", p))
          if (is.na(d)) {
            print("Currently not installed")
          } else {
            print(paste0("Currently installed but with version associated to : ",
                         d$Version))
          }
          print(paste0("Trying to install version ", v,
                       " from ", s))
          err <- tryCatch({
            biocLite(p, suppressUpdates = TRUE, suppressAutoUpdate = TRUE,ask=FALSE,type="source")
            FALSE
          }, error = function(e) list(TRUE, e))
          if (!err[[1]])
            print("Installation  : OK")
          if (err[[1]])
            print(paste0("ERREUR Installation  : KO : ",
                         as.character(err[[2]])))
          d2 <- packageDescription(p)
          if (!is.na(d2) && d2$Version != v) {
            print(paste0("ERREUR Installation : la version installée (",
                         d2$Version, ") est toujours différente de la version cible (",
                         v, ")"))
            print("Vérifier la version de R (est-elle la même que celle sur laquelle a été produite le fichier RDS 'packages_versions' ?")
          }
        }
        
      } else {
        stop(paste0("Source non reconnue : ", s))
      }
    }
  }
  if(!is.null(sink_file))sink(file = NULL)
}
# compare_and_install_packages_version()
