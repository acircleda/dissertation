# code to cite all packages used across R files (that are not loaded)

library(tidyverse)

# get file paths
files<- c(
  # main directory
  list.files(getwd()),
  # main subdirectory
  paste0("/data/", list.files(paste0(getwd(),"/data"))))

# get R and RMD files
r_files <- files[str_detect(files, ".R$|.r$|.Rmd$")]

# get all files
code<-unlist(lapply(paste0(getwd(), "/", r_files), readLines))

#find only packages in code
packages <- code %>% as.data.frame() %>%
  rename(lines = 1) %>%
  filter(str_detect(lines, "library\\(")) %>%
  mutate(lines = str_remove(lines, "\\)"),
         lines = str_remove(lines, "library\\("),
         lines = str_remove(lines, "#"),
         lines = str_trim(lines)) %>%
  distinct(lines)


# function to cite each package
cite_package <- function(x){
  toBibtex(citation(x)) }

# get citations
citations <- apply(packages, 1, cite_package)

#generate bib file
lapply(citations, write, "packages.bib", append=TRUE)
