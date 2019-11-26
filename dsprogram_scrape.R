### installing packages ###
install.packages("tidyverse")
install.packages("robotstxt")
install.packages("readxl")
install.packages("here")
install.packages("Rcrawler")
install.packages("htmltidy")
install.packages("rvest")
install.packages("XML")
install.packages("purrr")

### loading libraries ###
library(tidyverse)
library(robotstxt)
library(readxl)
library(here)
library(Rcrawler)
library(htmltidy)
library(rvest)
library(XML)
library(purrr)

#################### FIRST THINGS FIRST #####################
### show and set working directory location ###
here()
setwd(here())

### importing .xls file to rstudio ###
data <- read_excel("./Data/ds_dataservices_enviroscan.xlsx")
#Isolating the urls
#websites <- data$URL
#Isolating the names
organization <- data$Organization

### Checking to see if it's allowed to scrape ###
paths_allowed(paths = websites)
#Everything checks out to be true

#############################################################

######################### FUNCTIONS #########################
### Function which checks to see whether a webpage can be accessed. Purrr's version of try error.
read_html_possibly = possibly(read_html, otherwise = "Web page could not be accessed!")

### Function to create a list of links ### 
link_list<- function(nodes){
  tibble(link_name = nodes %>%
           html_text() %>%
           str_to_lower(),
         link = nodes %>%
           html_attr("href")
  )
}

### Function to create a list of links if the first one doesn't work ###
link_list2 <- function(nodes, site = ""){
  tibble(link_name = nodes %>%
           html_text() %>%
           str_to_lower(),
         link = nodes %>%
           html_attr("href") %>%
           xml2::url_absolute(site)) #this attaches the url to it.
}

### Function to get html and clean it ### UNTESTED
get_clean_info <- function(htmlcode, nodes = "") {
  htmlcode %>%
    html_nodes(nodes) %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
  
}


#############################################################

############### ORGANIZATION WEBSCRAPERS ####################
### University of Colorado ###
colorado_scrape <- read_html("https://www.colorado.edu/libraries/research-assistance/data-services") %>%
  html_node("#content") %>%
  html_text() %>%
  str_replace_all("\\n", " ")


### University of Miami ###
miami_scrape <- read_html("https://sp.library.miami.edu/subjects/data-services") %>%
  html_node("#tab-body") %>%
  html_text() %>%
  str_replace_all("\\n|\\t", " ")

### University of Virginia ###
## extract the pertinent link names from the side bar
virginia <- read_html("https://data.library.virginia.edu/") 
va_nodes <- virginia %>%
  html_nodes(".menu-research-data-services-container a")
# get list of site links
va_links <- link_list(va_nodes)

#va_links<- tibble(link_name = va_nodes %>%
#                       html_text() %>%
#                       str_to_lower(),
#                     link = va_nodes %>%
#                       html_attr("href") %>%
#                     xml2::url_absolute("https://data.library.virginia.edu")) #this attaches the url to it.
                          
## map it using the purrr package. This runs a function across multiple links.
  virginia_scrape <- va_links$link %>%
    map(read_html_possibly) %>%
    map(html_node("#main"))

## the links for items 6 and 8 doesn't exist, so deleting those.
## will also create a function to clean up the code. for some reason you had to do this before
## mapping.
  
  get_info <- function(htmlcode) {
    htmlcode %>%
      html_node("#main") %>%
      html_text() %>%
      str_replace_all("\\n|\\t", " ")
  }

#the links for items 6 and 8 doesn't exist, so deleting those.
virginia_scrape <- virginia_scrape[-c(6,8)] %>%
  map(get_info)

## University of Illinois Urbana-Champaign  
# extract links
uiuc <- read_html("https://www.library.illinois.edu/rds/")
uiuc_nodes <- uiuc %>%
  html_nodes(".col-sm-3 a")

# get list of site links
uiuc_links <- link_list(uiuc_nodes)


#getting all the scraped data
uiuc_scrape <- uiuc_links$link %>%
  map(read_html_possibly)

#created function in order to map rreading in the html and cleaning it up.       
getinfo_uiuc <- function(htmlcode) {
  htmlcode %>%
    html_nodes("#primary, #s-lg-box-18508865, .col-md-8") %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
  
}
#removing the 11th and 12th entries
uiuc_scrape <- uiuc_scrape[-c(11, 12)] %>%
  map(getinfo_uiuc) 


## University of Michigan
# reading the html for the main page and getting all the links
umich <- read_html("https://www.lib.umich.edu/research-data-services")
umich_nodes <- umich %>%
  html_nodes("#mlibrary_unit_main_content a") 

# get list of site links
umich_links <- link_list(umich_nodes)

#scraping the data for all those links
umich_scrape <- umich_links$link %>%
  map(read_html_possibly)
  
#function to obtain relevant data and to clean it up.
getinfo_umich <- function(htmlcode){
  htmlcode %>%
    html_node("#main-content") %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
}

#remove the second entry in umich_scrape since it's just e-mail and clean up data
umich_scrape <- umich_scrape[-c(2)] %>%
  map(getinfo_umich)

## University of Iowa
# reading the html for the main page and getting all the links 
uiowa <- read_html("https://www.lib.uiowa.edu/data/")

#getting the link names
uiowa_nodes <- uiowa %>%
  html_nodes("#menu-data-menu a")

# get list of site links
uiowa_links <- link_list(uiowa_nodes)

uiowa_scrape <- uiowa_links$link %>%
  map(read_html_possibly)

getinfo_uiowa <- function(htmlcode) {
  htmlcode %>%
    html_nodes(".content-region") %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
}

uiowa_scrape <- uiowa_scrape %>%
  map(getinfo_uiowa)

### University of California - San Diego
ucsd <- read_html("https://ucsd.libguides.com/data-services")

#get nodes
ucsd_nodes <- ucsd %>%
  html_nodes("#s-lg-box-7925573 a")

#get list of site links
ucsd_links <- link_list(ucsd_nodes)
ucsd_scrape <- ucsd_links$link %>%
  map(read_html_possibly)




getinfo_ucsd <- function(htmlcode) {
  htmlcode %>%
    html_nodes("#s-lg-box-collapse-7923959 .s-lib-box-content, 
               .pull-left, #s-lg-box-collapse-174905 .s-lib-box-content, 
               #s-lg-box-collapse-19410331 .s-lib-box-content, .panel-body") %>%
    html_text() %>%
    str_replace_all("\\n|\\t", " ")
}

ucsd_scrape <- ucsd_scrape %>%
  map(getinfo_ucsd)
#############################################################  

### UT - Austin ###
utaustin_scrape <- read_html("https://www.lib.utexas.edu/research-help-support/research-data-services") %>%
  html_nodes(".main-content, .utexas_promo_unit_no_image") %>%
  html_text() %>%
  str_replace_all("\\n|\\t|\\r", " ")

### University of Maryland ###
umd <- read_html("https://www.lib.umd.edu/data")

umd_nodes <- umd %>%
  html_nodes(".subsite-left, #sidebar-nav-menu a")

umd_links <- tibble(link_name = umd_nodes %>%
                        html_text() %>%
                        str_to_lower(),
                    link = umd_nodes %>%
                       html_attr("href") %>%
                     xml2::url_absolute("https://www.lib.umd.edu")) #this attaches the url to it.

#getting rid of unncessary rows
umd_links <- umd_links[-c(1, 3, 7, 10, 13),]

umd_scrape <- umd_links$link %>%
  map(read_html_possibly)

getinfo_umd <- function(htmlcode) {
  htmlcode %>%
    html_node("#main-content") %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
}

umd_scrape <- umd_scrape %>%
  map(getinfo_umd)
  
### Princeton University ###
princeton <- read_html("https://library.princeton.edu/research-data")

princeton_nodes <- princeton %>%
  html_nodes("#main-content .menu a")

princeton_links <- link_list2(princeton_nodes, site = "https://library.princeton.edu/")

princeton_scrape <- princeton_links$link %>%
  map(read_html_possibly) 


getinfo_princeton <- function(htmlcode) {
  htmlcode %>%
    html_node(".even") %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
}

princeton_scrape <- princeton_scrape %>%
  map(getinfo_princeton)

### University of Vermont ###
vt_scrape <- read_html("https://lib.vt.edu/research-learning/research-data-management-curation.html") %>%
  html_node("#vt_body_col :nth-child(1)") %>%
  html_text() %>%
  str_replace_all("\\n|\\t|\\r", " ")

### Oregon State ###
oregonstate <- read_html("https://guides.library.oregonstate.edu/research-data-services")
oregonstate_nodes <- oregonstate %>%
  html_nodes("#s-lg-guide-tabs a")

oregonstate_links <- link_list(oregonstate_nodes)

#cleaning some things up
oregonstate_links <- oregonstate_links[-c(2,3,4,5,6,7,26,27),]

oregonstate_scrape <- oregonstate_links$link %>%
  map(read_html_possibly)

getinfo_oregonstate <- function(htmlcode) {
  htmlcode %>%
    html_nodes("#s-lg-col-1 .s-lib-box-content") %>%
    html_text() %>%
    str_replace_all("\\n|\\t|\\r", " ")
}

oregonstate_scrape <- oregonstate_scrape %>%
  map(getinfo_oregonstate)



### Rutgers University ###
rutgers <- read_html("https://libguides.rutgers.edu/data")
rutgers_nodes <- rutgers %>%
  html_nodes(".s-lib-side-borders div a")

rutgers_links <- link_list(rutgers_nodes)
#need to clean up a lot of these links
rutgers_links_clean <- rutgers_links[-c(7, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44),]

rutgers_scrape <- rutgers_links_clean$link %>%
  map(read_html_possibly)

#this map2 function takes multiple arguements and applies them throughout the list.  The two parameters
# is rutgers_scrape and the actual node. Then you add a ~ with a function and pass the two parameters
# with the function(.x and .y)
rutgers_scrape <- map2(rutgers_scrape, ".s-lib-box-content", ~get_clean_info(.x, .y))

rutgers_save <- bind_rows(rutgers_scrape)

write.csv(rutgers_scrape, file = "ga_site_data\rutgers.csv", sep = "\t")


### Penn State ###




#University of Oregon
#site <- "https://library.uoregon.edu/data-services"
#o <- html_session(site) %>%
#  follow_link(css = "#sidenav a")
#oregon_nodes <- read_html("https://library.uoregon.edu/data-services") %>%
#  html_nodes(" #sidenav a")
# oregon_links <- tibble(link_name = oregon_nodes %>%
#                         html_text() %>%
#                         str_to_lower(),
#                       link = oregon_nodes %>%
#                         html_attr("href"))




