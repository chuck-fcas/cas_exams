library(tidyverse)
library(xml2)

# The purpose of this script is to get historical exams administered by the CAS.  The important exams anyway... TEEHEE.----

# Returns the foillowing list-
# url        = URL string where all available past exams are stored on the CAS website.
# url_index  = An annoying string.  The only purpose of this constant is to be removed within.  Preserving for completeness.
# xml        = The output of xml2::read_html(url)
# nodeset    = xml as a nodeset filtered to relevant nodes.
# exams_meta = A tibble.  Contains meta-data for each such as the exam name, url, xml, etc.
# exams      = A tibble.  The location of all the past exams downloaded to hard drive from exams_meta.

# At first this function was going to be able to use download.file.  Of course, the files are encrypted because you have to be a CAS member.  
# Using RSelenium to get pdf files!

casact_past_exams <- 
  function(casact_url     = "https://www.casact.org/admissions/studytools/", #CAS Website where exams are held as of August 2018----
           download_pdfs  = TRUE,
           overwrite_pdfs = TRUE,
           dest_dir       = Sys.getenv("TEMP")){
  
  #This index string will get in the way later.  It gets removed from url strings in order to navigate the site properly.
  casact_url_index <- "index.cfm?fa=PastExams"
  
  # Start Selenium Driver  - should start session of default web browser when the client is called----
  # Good thing RSelenium is badass.  I was able to search the interwebs for a solution that takes in a list of options for chrome browser and is able to download.
  eCaps <- list(
    chromeOptions = 
      list(prefs = list(
        "profile.default_content_settings.popups" = 0L,
        "download.prompt_for_download" = FALSE,
        "download.default_directory" = dest_dir,
        "download.directory_upgrade" = TRUE,
        "plugins.always_open_pdf_externally" = TRUE
      )
      )
  )
  rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  remDr <- rD[["client"]]
  
  #Navigate the browser to the target url----
  paste0(casact_url, 
         casact_url_index) %>% 
    remDr$navigate()
  
  #Read page source from website as an xml_document----
  casact_xml <- 
    remDr$getPageSource()[[1]] %>% 
    read_html
  
  #The hardest part is figuring out the xpath, but it's not bad.  Use inspect element in the browser if you get lost----
  casact_nodeset <- 
    casact_xml %>% 
    xml_find_all("//article//a")
  
  #Subset the nodeset to the desired data.  This works well as of August 2018----
  set <- 
    casact_nodeset %>% 
    xml_attr("href") %>% 
    stringr::str_detect("exam.+/") %>% 
    if_else(is.na(.), FALSE, .)
  
  #Get the names of the exams and the url.  Scrape each url for links----
  casact_exams_meta <- 
    tribble(
      ~exam,
      ~href,
      casact_nodeset[set] %>% 
        xml_contents %>% 
        as_list %>% 
        as_vector,
      casact_nodeset[set] %>% 
        xml_attr("href")
    ) %>% 
    unnest %>% 
    mutate(
      html = 
        paste0(casact_url, 
               href) %>% 
        lapply(function(x){
          remDr$navigate(x)
          remDr$getPageSource()[[1]] %>% 
            read_html})
    )
  
  #Within each page we can get links for the past exams----
  casact_exams_meta <- 
    casact_exams_meta %>%
    mutate(
      nodeset = 
        list(html, "//article//a") %>% 
        pmap(xml_find_all),
      set = 
        list(nodeset, "(Examiner's Report)|(Examiners' Report)") %>% # [A]Need to enhance this regex to find links for lower level exams.
        pmap(str_detect),
      nodeset = #Filter out bad urls.
        map2(nodeset, set, function(x, y) x[y]),
      pdfhref = 
        map2(nodeset, "href", xml_attr) #Again... not every exam is going to matter so we have character(0) for now.
      )
  
  #Filter to upper level exams----
  #Need to enhance this function to include all exams.  Need better text mining above at [A]
  casact_exams <- 
    casact_exams_meta[casact_exams_meta$exam %in% 
             paste("Exam", c(5, "6- US", 7:9)),]
  
  #Prepping the exam pdf data----
  casact_exams <-
    casact_exams %>% 
      select(exam, href, pdfhref) %>% 
      unnest
  
  #Download each pdf to location----
  casact_exams <-
    casact_exams %>% 
    mutate(
      pdf_doc_url = 
        paste0(casact_url, 
               href, 
               pdfhref),
      pdf_doc_destfile = 
        paste(dest_dir, 
              pdfhref, 
              sep = "\\")
    )
  
  #Finally download pdf files into temporary location----
  if(download_pdfs){
    #This code would be acceptable if we didn't have to login.  Easier just to continue to use the browser and save the pdfs.
    # list(url      = casact_exams$pdf_doc_url,
    #      destfile = casact_exams$pdf_doc_destfile,
    #      mode     = casact_exams$exam %>%
    #        sapply(function(x) "wb")) %>%
    # pmap(download.file)

    list(casact_exams$pdf_doc_url,
         casact_exams$pdfhref) %>% 
      pmap(function(x, y) {
        target_path <- paste(dest_dir, y, sep = "\\")
        if(!file.exists(target_path)){
          remDr$navigate(x)
          } else {
            if(overwrite_pdfs){
              file.remove(target_path)
            } else (
              message(target_path %>% paste0(., " - file already exists.  Set 'overwrite_pdfs' to TRUE and try again."))
            )
          }
        }
      )
    
    #Some reason the '.crdownload' string is added.  This is a hot fix for now.
    str_crap <- ".crdownload"
    list.files(dest_dir) %>% 
      paste(dest_dir, ., sep="\\") %>% 
      tidyselect::vars_select(ends_with(str_crap)) %>% 
      lapply(function(x) file.rename(x, x %>% stringr::str_remove(str_crap)))
    } else {
      message("User chose not to download pdfs.")
    }

  #Close down the server----
    remDr$close()
    # stop the selenium server
    rD[["server"]]$stop() 
  
  #Return all the outputs----
  return(list(
    url        = casact_url,
    url_index  = casact_url_index,
    xml        = casact_xml,
    nodeset    = casact_nodeset,
    exams_meta = casact_exams_meta,
    exams      = casact_exams
  ))

}
