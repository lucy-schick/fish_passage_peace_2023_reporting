preview_chapter('0100-intro.Rmd')


preview_chapter('0800-appendix-125231-trib-table.Rmd')
preview_chapter('0800-appendix-198666-trib-to-mcleod.Rmd')

#################################################################################################
##go to the index.Rmd and change gitbook_on <- TRUE
#################################################################################################

# rmarkdown::render_site(output_format = 'bookdown::gitbook',
#                        encoding = 'UTF-8')

# to load from scratch we need the pipe
rmarkdown::render('scripts/02_reporting/photos_import.Rmd', output_dir = "scripts/02_reporting/docs")
source('scripts/02_reporting/0180-photos-extract-metadata.R')


# define if we want to update the bib
update_bib <- FALSE

{
  # update util file functions from staticimports
  staticimports::import()
  source('scripts/staticimports.R')
  my_news_to_appendix()
  # These files are included in the gitbook version already so we move them out of the build
  files_to_move <- list.files(pattern = ".Rmd$") |>
    # 2400 is the phae 1 data anad photos
    stringr::str_subset('2400', negate = F) #move the attachments out
  files_destination <- paste0('hold/', files_to_move)

  ##move the files
  mapply(file.rename, from = files_to_move, to = files_destination)


  bookdown::render_book("index.Rmd", params = list(update_bib = update_bib))

  ##move the files from the hold file back to the main file
  mapply(file.rename, from = files_destination, to = files_to_move)
}


#################################################################################################
##go to the index.Rmd and change gitbook_on <- FALSE
#################################################################################################
##move the phase 1 appendix out of the main directory to a backup file or else the file is too big


# define the _bookfile_name from _bookdown.yml
filename_html <- 'fish_passage_peace_2023_reporting'

{

  file.rename('0600-appendix.Rmd', 'hold/0600-appendix.Rmd')

  ##   then make our printable pdf
  bookdown::render_book("index.Rmd",
                        output_format = 'pagedown::html_paged',
                        params = list(update_bib = update_bib))

  #move the phase 1 appendix back to main directory
  file.rename('hold/0600-appendix.Rmd', '0600-appendix.Rmd')

  # print to pdf
  pagedown::chrome_print(
    paste0(getwd(), '/', filename_html, '.html'),
    output = paste0(getwd(),'/docs/', filename_html, '.pdf'),
    timeout = 180
  )

  # PDF is only 19.2mb so don't compact, this retains the good image quality.
  # # reduce the size
  # tools::compactPDF(paste0(getwd(), "/docs/", filename_html, ".pdf"),
  #                   gs_quality = 'screen',
  #                   ##this was on the windows machine
  #                   # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
  #                   gs_cmd = "opt/homebrew/bin/gs"
  # )

  # get rid of the html as its too big and not needed
  file.remove(paste0(getwd(), '/', filename_html, '.html'))

}


##########################################make Phase 1 appendix seperately only when updated
#################################################################################################
##we need a workflow to print the Phase 1 attachment


files_to_move <- list.files(pattern = ".Rmd$") %>%
  stringr::str_subset(., 'index|fish_passage_peace_2023_reporting|0600', negate = T)
files_destination <- paste0('hold/', files_to_move)

##move the files
mapply(file.rename, from = files_to_move, to = files_destination)


##   then make our printable pdf
bookdown::render_book("index.Rmd",
                      output_format = 'pagedown::html_paged',
                      params = list(update_bib = update_bib))

##  move it to the docs folder so that it can be in the same place as the report
# file.rename('Elk2021.html', 'docs/Attachment_3_Phase_1_Data_and_Photos.html')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)

#print the attachment to pdf with chrome print
# openHTML('docs/Attachment_3_Phase_1_Data_and_Photos_prep.html')

pagedown::chrome_print(
  paste0(getwd(),'/', filename_html,'.html'),
  output = paste0(getwd(),'/docs/Attachment_3_prep.pdf'),
  timeout = 120
)


##now get rid of the first pages
length <- pdftools::pdf_length(paste0(getwd(), "/docs/Attachment_3_prep.pdf"))

# this changes so let's define
crop_this_many_pages <- 6

# trim up the file.  We ditch the last page only when there are references.  In the case of the bulkley there are due to the yaml file
pdftools::pdf_subset(paste0(getwd(), "/docs/Attachment_3_prep.pdf"),
                     pages = (crop_this_many_pages + 1):(length - 1), output = paste0(getwd(), "/docs/Attachment_3.pdf"))

##clean out the old files
file.remove(paste0(getwd(), "/docs/Attachment_3_prep.pdf"))
file.remove(paste0(getwd(),'/', filename_html,'.html'))

##it is very important to shrink the size of the pdf so we donèt blow up our github repo.
# We will set up ghostscript on the mac but for now we do manually.

tools::compactPDF(paste0(getwd(), "/docs/Attachment_3.pdf"),
                  gs_quality = 'ebook',
                  # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                  gs_cmd = "opt/homebrew/bin/gs"
)

