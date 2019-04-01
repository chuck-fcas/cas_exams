require(tidyverse)
#The purpose of this script is to collect all the tables and data from the CAS past exams.----

#Introduce tabulizer.----

## Before we get going we need to get tabulizer working.  The package depends on a Java installation.  Let's include a parameter so we can skip this nonsense.
## https://github.com/ropensci/tabulizer#installation ropensci is really cool.

yes_tabulizer_is_working_so_please_skip <- 
  function(skip = TRUE,
           environ = "C:\\Program Files (x86)\\Java\\jre1.8.0_181\\jre"){
  
    if(!skip){
      if (!require("remotes")) {
        install.packages(c("remotes", "tabulizer"))
      }
      
      #Install on 64-bit Windows
      remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
      #Elsewhere
      remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
      
      #Odds are Java will be annoying and not work.  Even damn time, am I right?  I have this issue on windows but debugging comes down to a few steps.
      #First, it's ok to say "I fucking hate Java".  Next Manually download both 32 and 64 bit for windows.  https://www.java.com/en/download/manual.jsp
      #Rest the environment variable to clear out old versions. Finally set the environment variable to the latest version you jusst manually installed.
      Sys.setenv(JAVA_HOME = "")
      Sys.setenv(JAVA_HOME = "C:\\Program Files (x86)\\Java\\jre1.8.0_181\\jre")
      rstudioapi::restartSession()
      install.packages("rJava", repos = "http://rforge.net/", type = "source")
      library(tabulizer)
      message("YAY - hopefully you didn't errors.  Everything should be fine if you see this message")
    } else {
      tryCatch(
        require(tabulizer),
        error = function(e) print("rJava is not working.  Try setting 'skip' to FALSE instead."),
        finally = print("Sweet.  It worked.")
      )
    }
}

yes_tabulizer_is_working_so_please_skip(skip = TRUE)

#First, we start by getting all the pdfs off of the CAS website.  To do this we wrote a function that will return some items.  Your browser will flicker around a bit - this is the selenium driver navigating in the browser.  Going straight to the html data on the website will block you - no bots allowed.
source("R/casact_past_exams.R")

#Set to TRUE if you need to download the pdfs.
#Sometimes it's better to run it multiple times with 'overwrite_pdfs' set to FALSE.  The browser doesn't always keep up and may skip downloads.  Need a fix for this.
casact_past_exam_list <- casact_past_exams(download_pdfs  = TRUE,
                                           overwrite_pdfs = FALSE,
                                           dest_dir       = "D:\\ple4ma\\e7\\pastexams")  ###DAMMIT.  Some of the exams (exam 9 I think) leave an extension of .crdownload.  I hot pixed it.

#Let's look for tables in exam 5.  If we take a look at the exams object you can see what is available.  
# Choose the exam you want to see and load the tables from the pdf into dataframes.  Let's give it a shot.
casact_past_exam_list$exams

#The first exam will do the trick.  Page 4 has a table on it, however, the following produces list().
casact_past_exam_list$exams$pdf_doc_destfile[1] %>% 
extract_tables(file = ., pages = 4)

#Let's scan the whole document then.
casact_past_exam_list$exams$pdf_doc_destfile[1] %>% 
  extract_tables(file = .)

#The error is thrown from java when tabulizer scans the entire document. We end up with nothing.  Why?  
# Turns out the entire pdf is a scan a print, or an image; in this case it does not preserve the tabular formats.  
# Let's try the area funationally and see if that indeed is the case.
#The shiny and miniUI packages are needed for this. 
# The viewer should allow the user to interact with the page and allow a selection of an area. 
library("shiny")
library("miniUI")
casact_past_exam_list$exams$pdf_doc_destfile[1] %>% 
  tabulizer::extract_areas(., pages = 4)

#After you select table and hit DONE we end up with an empty box, a 1x1 cahracter matrix with "" as the value.  
# If we want to read this exam automatically it will require image recognition, a transformation or conversion, 
# or some other algorithm to extract tabular formats.  Probably easier to get copies from the exam chairs instead;  
# worth looking into.

#Let's try another exam.  
casact_past_exam_list$exams$pdf_doc_destfile[9] %>% 
  tabulizer::extract_tables(file = ., pages = 4)

#Wow, what a difference that was.  The data needs a little bit of cleaning up but it definitely pulled the table out.  
# To confirm my suspicions about the "image theory" above I opened both exams with Adobe reader and a browser.  
# Sure enough I am not allowed to select text from the first document, but I can for the second.  Confirmed. 
#Might as well test out the area function - it works very well.  Certainly we won't be using the area function to pull
# x many exams with y many item tables, but this could help us view the document and understand the extraction.
casact_past_exam_list$exams$pdf_doc_destfile[9] %>% 
  tabulizer::extract_areas(., pages = 4)

#Before we get too crazy let's see if we can cleanup the table extraction.  Using the same function as before but specify the output as a dataframe.
casact_past_exam_list$exams$pdf_doc_destfile[9] %>% 
  extract_tables(file   = ., 
                 pages  = 4, 
                 output = "data.frame")

#That was actually pretty worthless to us in terms of "clean-up".  dataframes are always easier to work with anyway so it is a nice result.  
# We are going to have to find a way to clean up the headers of the tables.  Before we do that let's extract all the data we can.  
# Let's review some other tables in attempts to identify a pattern to the  - we don't get too specific on the table extraction.  

#Instead of using the pages argument let's put the whole exam through. Hell, let's put every exam through.  
# Those exams that return list(0) are images and again need to be dealt with separately.
casact_past_exam_list$exams$pdf_doc_tables <-
  casact_past_exam_list$exams$pdf_doc_destfile %>% 
  lapply(function(x) extract_tables(file   = x, 
                                    output = "data.frame"))

# What do you know we get the same java error as before.  I suppose that is expected.  We did not get an error when we specified the page argument. 
# Instead of a tryCatch let's pass every page as a parameter along with the pdf document.  This opens ups an opportunity to test out more tabulizer functionality.
# Getting the number of pages does not produce an error.
casact_past_exam_list$exams$n_pages <-
  casact_past_exam_list$exams$pdf_doc_destfile %>% 
  sapply(function(x) get_n_pages(file   = x))

casact_past_exam_list$exams$pdf_doc_tables <-
  casact_past_exam_list$exams$pdf_doc_destfile %>% 
  lapply(function(x) 
    
    
    
    extract_tables(file   = x, 
                                    output = "data.frame"))

casact_past_exam_list$exams$pdf_doc_destfile[9] %>% get_n_pages(.) %>% 4:. 


craycray <- 
  list(x = casact_past_exam_list$exams$n_pages, 
       y = casact_past_exam_list$exams$pdf_doc_destfile) %>% 
  pmap(function(x,y) {
    sapply(4:x, function(p){
      print(paste(p,y))
      tryCatch(expr = extract_tables(file   = y, 
                                     pages  = p), error = function(e) e)
      
    })
  })
tabulizer::extract_areas()

list(x = casact_past_exam_list$exams$n_pages, y = casact_past_exam_list$exams$pdf_doc_destfile)



# This poses a problem. Avoiding tryCatch is always a goal.  Relying on error catching outside of the core R packages, especially java, makes me nervous.
# I expect a similar error but let's test to see if the error persists with the documents we were looking at earlier.
casact_past_exam_list$exams$pdf_doc_destfile[1] %>% get_n_pages(.)




casact_past_exam_list$exams$pdf_doc_destfile[1] %>% tabulizer::locate_areas(., pages = 78)

#Alright, we are going to deal with those asshole documents. We do have a way to determine them without an error - 
# let's create a function to check the output of page 4 and based on that output extract the pages we need.
# When we do want to try some OCR tThis looks promising: https://ropensci.org/blog/2016/11/16/tesseract/.  Same guys who are doing tabulizer.

casact_past_exam_list$exams$pdf_doc_destfile
#Delete all the pdfs if you want to.
casact_past_exams$exams$pdf_doc_destfile %>% 
file.remove()