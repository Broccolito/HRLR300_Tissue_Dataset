####################################################
#Google Crawler
#Author Wanjun Gu
#July 6 2018
#This algorithm will acquire open-source pictures
#From the internet 
####################################################

library(rvest)
library(stringr)
library(XML)
library(RCurl)
get_picture = function(search_item = "github",
                       downlaod = FALSE){
  URL = paste("https://www.google.com/search?biw=1242&bih=588&tbm=isch&sa=1&ei=f7XfWo_dKZKAjwPf342YCA&q=",
              search_item,
              sep = "")
  #getstr() will get the characters in between two characters
  getstr = function(mystring,
                    initial.character,
                    final.character){
    snippet = rep(0, length(mystring))
    for (i in 1:length(mystring)){
      initial.position = gregexpr(initial.character,
                                  mystring[i])[[1]][1] + 1
      final.position = gregexpr(final.character,
                                mystring[i])[[1]][1] - 1
      snippet[i] = substr(mystring[i], initial.position, final.position)
    }
    return(snippet)
  }
  page = read_html(URL)
  node = html_nodes(page,xpath = '//img')
  urls = vector(length = length(node))
  for(i in 1:length(node)){
    urls[i] = as.character(node[[i]])
  }
  urls = str_sub(substring(getstr(urls,"src="," width="),5),
                 1, str_length(substring(getstr(urls,"src="," width="),5))-1)
  
  if(downlaod){
    for(i in 1:length(urls)){
      download.file(urls[i], paste(as.character(i), as.character(rnorm(1)), ".jpg", sep = ""), mode = "wb")
    }
  }else if(!downlaod){
    #Open the pictures in browser
    for(i in 1:length(urls)){
      shell.exec(urls[i])
    }
  }else if(downlaod == "both"){
    for(i in 1:length(urls)){
      download.file(urls[i], paste(as.character(i),
                                   as.character(rnorm(1)), ".jpg", sep = ""), mode = "wb")
    }
    for(i in 1:length(urls)){
      shell.exec(urls[i])
    }
  }
}
get_url = function(term){
  term = as.character(term)
  getGoogleURL = function(search.term,
                          domain = '.co.uk', 
                          quotes=TRUE){
    search.term = gsub(' ', '%20', search.term)
    if(quotes){
      search.term = paste('%22', search.term, '%22', sep='') 
    }
    getGoogleURL = paste('http://www.google', domain, '/search?q=',
                          search.term, sep='')
  }
  getGoogleLinks = function(google.url){
    doc = getURL(google.url, httpheader = c("User-Agent" = "R
                                             (2.10.0)"))
    html = htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                          (...){})
    nodes = getNodeSet(html, "//h3[@class='r']//a")
    return(sapply(nodes, function(x){x = xmlAttrs(x)[["href"]]}))
  }
  search.term = term
  quotes = "FALSE"
  search.url = getGoogleURL(search.term=search.term,
                            quotes=quotes)
  links = substring(getGoogleLinks(search.url), 8)
  return(as.vector(links))
}
gen_search = function(item){
  item = as.character(item)
  get_picture(search_item = item, downlaod = FALSE)
  urlvector = get_url(term = item)
  return(urlvector)
}