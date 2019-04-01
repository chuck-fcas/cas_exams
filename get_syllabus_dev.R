library(tidyverse)
library(xml2)

## TO DO | 
###[ ] Turn into a function.
###[x] Cleanup urls
###[ ] author names need first and last flipped
###[ ] automate syllabus pull down.
###[ ]Remove special characters from book.bib.  Certain characters ruin the encoding of bibtex.

#Set output/input path ----
bib_path <- "bibliography/"

#Web browser program path ----
chrome_path <- "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"

#Exam 7 syllabus url ----
syllabus_url <- "https://www.casact.org/admissions/syllabus/TextReferences/index.cfm?fa=exam7"

#This is a manual process to be removed later.
#Opens in chrome.
shell.exec(syllabus_url)
#Right click syllabus table, view page source, and save as 'e7syllabus.html'.

#Read html table and collect various nodes
syllabus_file <- paste0(bib_path, "e7syllabus.html")
syllabus_xml <- read_html(syllabus_file)
syllabus_header <- syllabus_xml %>% xml_find_all("//tbody//tr//th")
syllabus_data <- syllabus_xml %>% xml_find_all("//tbody//tr//td")

#Get dimensions of table
syllabus_cols <- length(syllabus_header)
syllabus_rows <- length(syllabus_data) / syllabus_cols

#Reorganize html data table into a tibble
bibdata <-  syllabus_data %>% xml2::xml_text() %>% unlist(F)
#If you're wondering why I set the dimensions and then transpose you won't find an answer here.  I'll let the reader try it themselves.
dim(bibdata) <- c(syllabus_cols, syllabus_rows)
bibdata <- t(bibdata) %>% as_tibble()
names(bibdata) <- syllabus_header %>% xml2::xml_text() %>% toupper() %>% gsub(" ", "_", .)

#Retrieve the xml 'href' attributes and dimensionalize the same way as before.
syllabus_href <-
  syllabus_data %>% 
  xml2::as_list() %>% 
    lapply(function(x)
      lapply(x, function(y) y %>% attr("href"))
    )
#Set the same dimensions as before to retrieve url information only
dim(syllabus_href) <- c(syllabus_cols, syllabus_rows)

#Organize data into a tibble.
#Remove special characters and organize citations.
#Extract text from citation.  #https://www.regular-expressions.info/quickstart.html

#This function removes special characters that do not belong in the key for a bibentry
syllabus_key_fix <- function(x){
  x <- x %>%  
    str_replace("ó","o") %>% 
    str_replace("é","e") %>% 
    str_replace("è","e") %>% 
    str_replace("ö","o") %>% 
    str_replace("ü","u") %>% 
    str_replace("ñ","n") %>% 
    str_replace("á","a") %>% 
    str_replace("‐","-") %>% 
    str_replace("‐","-") %>% 
    str_replace("′","$^{\\prime}$") %>% 
    str_replace("\\(","-") %>% 
    str_remove_all(" ") %>% 
    str_remove_all("\\)") 
  x
}

bibdata_ <-
  bibdata %>%
  separate(CITATION,
           into = c("author", "title", "note"),
           sep = "\"") %>%
  transmute(
    bibtype = "Article",
    key     = ABBREVIATION %>% syllabus_key_fix(),
    note    = trimws(note),
    title  = gsub(",", "", title) %>% trimws(),
    author  = author  %>%  map(function(x) {
      result <- str_replace(x, "and ", "; ") %>% str_replace("; ; ", "; ")
      result <- result %>% str_replace_all("\\., |\\. ","\\.")
      result <- str_split(result, "; ")
      result[[1]] %>% unlist()
    }),  # 
    journal = map(note, function(x) {
      result <- str_split(x, ",")
      result[[1]][[1]][1] 
    }) %>% unlist() %>% trimws(),
    note     = note %>% str_remove(journal),
    pages    = note %>% str_extract_all("pp\\. [0-9][0-9]{1,4}-[0-9][0-9]{1,4}") %>% str_remove_all("pp\\. "),
    note     = note %>% str_remove_all( "pp\\. [0-9][0-9]{1,4}-[0-9][0-9]{1,4}"),
    year     = note %>% str_extract(    "[0-9]{4}") %>% if_else(is.na(.), str_extract(key, "[0-9]{4}"), .) %>% if_else(is.na(.), "####", .), # sometimes year is in the key if it is not found elsewhere.
    note     = note %>% str_remove(     "[0-9]{4}"),
    volume   = note %>% str_extract(    "((Vol(ume)?(\\.)?) \\d)(, Issue \\d)?") %>% if_else(is.na(.), "", .),
    note     = note %>% str_remove(     "((Vol(ume)?(\\.)?) \\d)(, Issue \\d)?"),
    month    =(note %>% str_extract("\\w+") %in% c("Spring", "Summer", "Fall", "Winter" , month.name)) %>% 
               if_else (str_extract(note, "\\w+"),"|"),
    note     = note %>% str_remove(month) %>%
                        str_remove ("(,( ){1,2}){1,3}") %>% 
                        str_replace(", ,", ",") %>% 
                        str_replace(" ,", ",") %>% 
                        str_replace("  ", " "), #Final cleanup -- easy enough.
    author   = author %>% str_replace(",", "") %>% as.character() , #person(role = "aut"), #Needs refinement - seperate authors into different people.
    url      = lapply(syllabus_href[1,], unlist) %>% #First row holds all the relevant information we need.
               mapply(FUN = paste0, ., collapse = ">, <") %>%  #Adding url characters '<' and '>' between.
               paste0("<", ., ">") #Adding url characters '<' and '>' around
    
      )

# # Remove all characters that do not agree with bibentry/bibtex.
# removals <- c("\\", "#", "$", "%", "^", "&", "_", "~", "\\?") %>%
#   paste0(collapse = "") %>%
#   paste0("[", ., "]")
# 
# 
# bibdata_ <- bibdata_ %>% mutate_all(str_remove_all, pattern = removals)

#Shit - one more potentially annoying thing to do: download pdfs.

#Create bibentries
library(RefManageR)
bibentry_ <- bibdata_ %>% as.BibEntry()

#Finally, write bibentry down to file
WriteBib(bibentry_, file = "book.bib", biblatex = TRUE, append = FALSE, verbose = TRUE)

#Example of opening bibentry.
file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- suppressMessages(ReadBib(file.name))[[20:21]]
identical(as.BibEntry(unlist(bib)), bib)  ## see also RelistBibEntry

bibdata_$href_url %>% mapply(FUN = paste0, ., collapse = ">, <") %>% paste0("<", ., ">")
