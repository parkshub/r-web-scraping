library ("knitr")
library ("rvest")
library ("xml2")
library("stringr")
library ("httr")
library ("iptools")
library ("tidyr")
library ("RCurl")


##########################################
##GENERAL CODE THAT APPLIES TO ALL LOOPS##
##########################################

########## FIRST PROXY LIST ##########  

proxy_url = "https://free-proxy-list.net/"
proxy_url = read_html (proxy_url)

proxy_list_raw = proxy_url %>%
  html_nodes ("div.table-responsive") %>%
  html_nodes ("tbody") %>%
  html_nodes ("td") %>%
  html_text()

proxy_list1= list()


# Separating proxy from port 
for (i in seq(from=1, to=2400, by = 8)){
  proxy_list1 = rbind (proxy_list1, proxy_list_raw [i])
}

for (i in seq(from=2, to=2400, by = 8)){
  proxy_list1 = rbind (proxy_list1, proxy_list_raw [i])
}

proxy_list1 = unlist(proxy_list1)


# Gathering IP from first list
proxy_ip1 = proxy_list1 [1:300]


# Gathering ports from first list
proxy_port1 = proxy_list1 [301:600]

proxy_port1 = as.numeric (proxy_port1)
# When using proxy function, ports are only recognized when numeric




########## SECOND PROXY LIST ##########  

proxy_url2 = "https://api.proxyscrape.com/?request=getproxies&proxytype=http&timeout=10000&country=all&ssl=all&anonymity=all"

read_proxy_url2 = getURL("https://api.proxyscrape.com/?request=getproxies&proxytype=http&timeout=10000&country=all&ssl=all&anonymity=all")

# Separating proxy and port

proxy_list2 = unlist(str_split(read_proxy_url2, "\\r\\n"))

proxy_list2 = unlist(str_split(proxy_list2, ":"))


# Gathering IP from second list

proxy_ip2 = list()

for (i in seq(from=1, to=14949, by=2)){
  proxy_ip2 = rbind(proxy_ip2, proxy_list2[i])
}

proxy_ip2 = unlist(proxy_ip2)

proxy_ip2 = proxy_ip2[1:7474]


# Gathering port from second list

proxy_port2=list()

for (i in seq(from=2, to=14949, by=2)){
  proxy_port2 = rbind (proxy_port2, proxy_list2[i])
}

proxy_port2 = as.numeric(unlist(proxy_port2))


# Compiling all proxies and port

proxy_ip = c(proxy_ip1, proxy_ip2)

proxy_port = c(proxy_port1, proxy_port2)


#### SETTING UP VARIABLES #### 

z=1
y=0
x=0




university_df=read.csv("C:\\Program Files\\RStudio\\My Work\\list_uni.csv")
# You would need to change the path and file name to what you want



for (i in 1:nrow(university_df)) {
  
  # extracting the university's url from csv file
  url_main_raw=university_df[1,i]
  url_main=as.character(url_main_raw)
  url_main_read = read_html(url_main)
  
  # extracting university's name from csv file
  
  name_raw=university_df[1,1]#school name raw
  name=as.character(name_raw) # school name
  
  
  
  
  page_total_raw = url_main_read %>%
    html_nodes("li.page-item") %>%
    html_text ()
  
  page_total = page_total_raw[12]
  page_total = strtoi(page_total, base = 0L)
  
  
  
  complete_df = data.frame()
  


################################################
########PER PAGE LOOP WITH DYNAMIC PROXY########
################################################
  
  for (page_number in 1:page_total) {
    sess_df= data.frame()
    start_time <- Sys.time()
    
    # Reseting the while loop variable
    looper_test = TRUE
    
    # Modifying the url to move to the next page
    url_second_test = paste0 (url_main, page_number)
    
    
    
    ########## Beginnig of "while" loop debugging ########## 
    
    # "While" loop that loops until a stable proxy is found 
    while (looper_test == TRUE) {
      
      print ("Looking for stable proxy")
      cat ('Currently on proxy number', z, "and website page number", page_number)
      # It was tiring to wait 60+ minutes to see whether the code worked so I made a little notification!
      
      # Defining the variable you want to debug
      proxy_check = tryCatch (
        # Defining the variable
        expr = {read_html(html_session(url_second_test, use_proxy(proxy_ip[z], proxy_port[z])))
        },
        # Defining what happens in case of an error
        error = function(e) {
          print ("error")
        },
        # Defining what happens in case of a warning
        warning = function (w) {
          print ("warning")
        }
        # My understanding is that, when there is an error or warning, the variable being debugged is defined as either an error or warning
      )
      # Moving to next proxy if error or warning
      if ((proxy_check[1]=="error")|(proxy_check[1]=="warning")){
        z = z + 1
        y=0
      }
      # Ending the loop if connection to proxy or webpage is successful
      if ((proxy_check[1]!="error") & (proxy_check[1]!="warning")){
        looper_test = FALSE
        print ("Proxy is stable")
        y=y+1
      }
    }
    
    if ((y+1)%%200==0){
      print ("Switching after 200 use.")
      z=z+1
      y=0
    }
    ########## End of "while" loop debugging ########## 
    # Error means that the connection timedout
    # Warning can mean two things
    # 1. If you've been using the same proxy for awhile, it probably means the IP was blocked
    # If you ran the code separately, this is what it should say Error in open.connection(x, "rb") : HTTP error 429.
    # 2. If not, access to the proxy was denied 
    
    
    
    ########## Beginning of steps for extracting name and date ##########
    
    # Creating a list for name and dates since name aren't separated from the dates 
    namedate_raw = proxy_check %>% 
      html_nodes ("div.sessaoConteudo") %>%
      html_nodes ("li") %>%
      html_text ()
    
    # Separating name and date
    namedate = gsub ("\\n","", namedate_raw) # change this part
    
    # Creating an empty list to fill

    date = list()
    

    
    # Sequence for gathering just the dates and making them into a =list
    for (i in seq(from=2, to=length(namedate), by=2)) {
      date = rbind (date, namedate[i])
    }
    ########## End of steps for extracting name and date ##########
    
    
    
    # Creating a list of the links from the names
    name_link = proxy_check %>% 
      html_nodes ("div.sessaoConteudo") %>%
      html_nodes ("a") %>%
      html_attr("href")
    
    # Creating a list of the page number repeated by the number of entries on the current page
    url_page = rep(page_number, nrow(date))
    
    # Combining all the lists from current page into a data frame
    sess_df = cbind (url_page, unlist(name), unlist(date), name_link) #change this part
    
    # If it's the first page, the complete data frame is the current data frame. Else, combine current frame to the complete frame
    if (page_number == 1) {
      complete_df = sess_df
    } else {
      complete_df = rbind (complete_df, sess_df)
    }
    
    end_time <- Sys.time()
    
    # Setting a sleep of x (i.e. 3) seconds if the whole loop took less than 1 second.
    if ((end_time - start_time)<1){
      Sys.sleep (3)
    }
    
    x = x + as.numeric(end_time - start_time)
    
    
    ########## Reseting the proxy list if current list is expended ##########
    
    if ((x>20000)|(z==length(proxy_ip))){
      # First list of proxies
      
      proxy_url = read_html (proxy_url)
      proxy_list_raw = proxy_url %>%
        html_nodes ("div.table-responsive") %>%
        html_nodes ("tbody") %>%
        html_nodes ("td") %>%
        html_text()
      proxy_list1= list()
      for (i in seq(from=1, to=2400, by = 8)){
        proxy_list1 = rbind (proxy_list1, proxy_list_raw [i])
      }
      for (i in seq(from=2, to=2400, by = 8)){
        proxy_list1 = rbind (proxy_list1, proxy_list_raw [i])
      }
      proxy_list1 = unlist(proxy_list1)
      proxy_ip1 = proxy_list1 [1:300]
      proxy_port1 = proxy_list1 [301:600]
      proxy_port1 = as.numeric (proxy_port1)
      
      # Second list of proxies
      read_proxy_url2 = getURL(proxy_url2)
      proxy_list2 = unlist(str_split(read_proxy_url2, "\\r\\n"))
      proxy_list2 = unlist(str_split(proxy_list2, ":"))
      proxy_ip2 = list()
      for (i in seq(from=1, to=14949, by=2)){
        proxy_ip2 = rbind(proxy_ip2, proxy_list2[i])
      }
      proxy_ip2 = unlist(proxy_ip2)
      proxy_ip2 = proxy_ip2[1:7474]
      proxy_port2=list()
      for (i in seq(from=2, to=14949, by=2)){
        proxy_port2 = rbind (proxy_port2, proxy_list2[i])
      }
      proxy_port2 = as.numeric(unlist(proxy_port2))
      
      # Compiling all proxies and port
      proxy_ip = c(proxy_ip1, proxy_ip2)
      proxy_port = c(proxy_port1, proxy_port2)
      
      # Reseting the z value ()  and y value
      z=1
      y=0
      x=0
    }
    
    if (page_number == page_total) {

      name_path_raw = paste0("C:\\Program Files\\RStudio\\My Work\\", name)
      
      name_path = paste0 (name_path_raw, ".csv")
    
      write.csv(complete_df, name_path)
    }
  }
}


 








