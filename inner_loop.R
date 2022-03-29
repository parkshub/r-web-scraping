library ("knitr")
library ("rvest")
library ("xml2")
library("stringr")
library ("httr")
library ("iptools")
library ("tidyr")
library ("RCurl")
library ("dplyr")

proxy_url = "https://free-proxy-list.net/"
proxy_url = read_html (proxy_url)
proxy_list_raw = proxy_url %>%
  html_nodes ("div.table-responsive") %>%
  html_nodes ("tbody") %>%
  html_nodes ("td") %>%
  html_text()
proxy_list1= list()
# Separating proxy from port 
for (i1_ip in seq(from=1, to=2400, by = 8)){
  proxy_list1 = rbind (proxy_list1, proxy_list_raw [i1_ip])
}
for (i2_ip in seq(from=2, to=2400, by = 8)){
  proxy_list1 = rbind (proxy_list1, proxy_list_raw [i2_ip])
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
for (i3_ip in seq(from=1, to=14949, by=2)){
  proxy_ip2 = rbind(proxy_ip2, proxy_list2[i3_ip])
}
proxy_ip2 = unlist(proxy_ip2)
proxy_ip2 = proxy_ip2[1:7474]
# Gathering port from second list
proxy_port2=list()
for (i4_ip in seq(from=2, to=14949, by=2)){
  proxy_port2 = rbind (proxy_port2, proxy_list2[i4_ip])
}
proxy_port2 = as.numeric(unlist(proxy_port2))
# Compiling all proxies and port
proxy_ip = c(proxy_ip1, proxy_ip2)
proxy_port = c(proxy_port1, proxy_port2)
#### SETTING UP VARIABLES #### 
z=1
y=0
x=0
looper_test=TRUE




# df = read.csv("C:/Program Files/RStudio/My Work/Week 5/Escavador_UFAL_Selected_names.csv")
df = read.csv("C:/Users/tomgu/Dropbox/PhD/Projects/Network/Summer_2019_Andrew_Park/working/Week_5/Escavador_UFAL_Selected_names.csv")

fullname=as.character(df$name)
fulllink=as.character(df$name_link)



# Creating the total data frame for a specific school
school_col_name=c("name", "link", "school_1",	"major_1",	"dates_school_1",	"advisor_s1",	"school_2",	"major_2",	"dates_school_2",	"advisor_s2",	"school_3",	"major_3",	"dates_school_3",	"advisor_s3",	"school_4",	"major_4",	"dates_school_4",	"advisor_s4",	"school_5",	"major_5", "dates_school_5",	"advisor_s5","school_6",	"major_6", "dates_school_6",	"advisor_s6", "work_1",	"dates_work_1",	"work_2",	"dates_work_2",	"work_3",	"dates_work_3",	"work_4",	"dates_work_4",	"work_5",	"dates_work_5",	"work_6",	"dates_work_6")

school_df = data.frame(matrix(ncol=38, nrow=nrow(df)))

colnames(school_df)=school_col_name


for (a in 1:nrow(df)) {
  
  start_time <- Sys.time()
  
  looper_test = TRUE
  
  myname=fullname[a]
  mylink=fulllink[a]
  
  
  
  while (looper_test == TRUE) {
    print ("Looking for stable proxy")
    cat ("Proxy number:", z, " Loop number:", a," Proxy repitition:", y)
    proxy_check = tryCatch (
      expr = {read_html(html_session(mylink, use_proxy(proxy_ip[z], proxy_port[z])))
      },
      error = function(e) {
        print ("error")
      },
      warning = function (w) {
        print ("warning")
      }
    )
    if ((proxy_check[1]=="error")|(proxy_check[1]=="warning")){
      z = z + 1
      y=0
    }
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
  
  major_and_date = proxy_check %>%
    html_nodes ("body") %>%
    html_nodes("main#escavador-app.main.-contrast") %>%
    html_nodes ("section#academico.box.-flush.-anchor") %>%
    html_nodes ("div.list-inline.inline-edit-content.row") %>%
    html_nodes ("div.col-sm-6.inline-edit-item-box.clearfix-box") %>%
    html_nodes ("p.heading.-likeH5.inline-edit-item") %>%
    html_text ()
  
  major_raw=list()
  for (i1_major in seq(from=1, to=12, by=2)) {
    major_raw=rbind(major_raw, major_and_date[i1_major])
  }
  
  major_date_raw=list()
  for (i2_major in seq(from=2, to=12, by=2)) {
    major_date_raw=rbind(major_date_raw, major_and_date[i2_major])
  }
  
  school_raw = proxy_check %>%
    html_node ("body") %>%
    html_nodes("main#escavador-app.main.-contrast") %>%
    html_nodes ("div.container.-rel") %>%
    html_nodes ("section#academico.box.-flush.-anchor") %>%
    html_nodes("div#formacao.box.-flush.inline-edit-main-box")%>%
    html_nodes ("div.list-inline.inline-edit-content.row") %>%
    html_nodes ("div.col-sm-6.inline-edit-item-box.clearfix-box") %>%
    html_nodes (xpath="//*[@id='formacao']/div/div/div/p[3]/a/text()")%>%
    html_text()
  
  advisor_raw1= proxy_check %>%
    html_nodes ("body") %>%
    html_nodes("main#escavador-app.main.-contrast") %>%
    html_nodes ("section#academico.box.-flush.-anchor") %>%
    html_nodes ("div.list-inline.inline-edit-content.row") %>%
    html_nodes ("div.col-sm-6.inline-edit-item-box.clearfix-box") %>%
    html_nodes (xpath="//*[@id='formacao']/div/div[2]/div/p[3]") %>%
    html_text ()
  
  advisor_raw=list()
  
  
  
  for (blah in 1:6){
    advisor_test=as.logical(str_detect(advisor_raw1[blah],"Orientador", negate = FALSE))
    if (is.na(advisor_raw1[blah])==TRUE) {
      advisor_raw[blah]=NA
      
    } else if (advisor_test == FALSE) {
      advisor_raw[blah]=NA
    } else {
      advisor_split_raw=unlist(strsplit(advisor_raw1[blah],"(?<=[O][r][i][e][n][t][a][d][o][r][:])", perl=TRUE))
      advisor_split=advisor_split_raw[2]
      pattern="[[A-Z][a-z][.][ ][//]]+"
      advisor_raw2=str_match(advisor_split,pattern)
      advisor_raw2=str_remove(advisor_raw2,"Bolsista do")
      advisor_raw2=str_remove(advisor_raw2,"Título")
      advisor_raw2=str_trim(advisor_raw2)
      advisor_raw=unlist(append(advisor_raw, advisor_raw2))
    }
  }
  
  exp_place_raw = proxy_check %>%
    html_nodes ("body") %>%
    html_nodes("main#escavador-app.main.-contrast") %>%
    html_nodes ("section#profissional.box.-flush.-anchor") %>%
    html_nodes ("ul.list-block.-small.-flushHorizontal.inline-edit-content.row") %>%
    html_nodes ("div.col-sm-6.inline-edit-item-box.clearfix-box") %>%
    html_nodes (xpath="//*[@id='atuacao-profissional']/div/ul/div/li/input[@type='hidden' and @class='inline-edit-item-instituicao-nome']/@value") %>%
    html_text ()
  
  
  
  exp_date_raw = proxy_check %>%
    html_nodes ("body") %>%
    html_nodes("main#escavador-app.main.-contrast") %>%
    html_nodes ("section#profissional.box.-flush.-anchor") %>%
    html_nodes ("ul.list-block.-small.-flushHorizontal.inline-edit-content.row") %>%
    html_nodes ("div.col-sm-6.inline-edit-item-box.clearfix-box") %>%
    html_nodes ("li.item") %>%
    html_nodes ("div.inline-edit-item-descricao") %>%
    html_nodes (xpath="//*[@id='atuacao-profissional']/div/ul/div/li/p") %>%
    html_text ()
  
  # Edited in section: Compiling information on one person in a vector and adding it to the data frame
  
  sess_vector=c()
  
  sess_vector=c(sess_vector, myname)
  
  sess_vector=c(sess_vector, mylink)
  
  
  for (acad in 1:6){
    
    sess_vector= c(sess_vector, school_raw[acad])
    
    sess_vector= c(sess_vector, advisor_raw[acad])
    
    sess_vector= c(sess_vector, major_raw[acad])#! this has to be split 
    
    sess_vector= c(sess_vector, major_date_raw[acad])#! this has to be split
    
  }
  
  for (exp in 1:6) {
    
    sess_vector=c(sess_vector, exp_date_raw[exp])
    
    sess_vector=c(sess_vector, exp_place_raw[exp])
  }
  
  
  school_df[a, 1:38]=sess_vector# check if a is the most outer loop [V]
  
  end_time <- Sys.time()
  
  if ((end_time - start_time)<1){
    Sys.sleep (3)
  }
  
  x2=as.numeric(end_time - start_time)
  
  x = x + as.numeric(end_time - start_time)
  
  cat ("Loop", a, "took", x2, "seconds")
  cat ("Total seconds passed:", x)
  
  
  
  
  if ((x>21600)|(z==length(proxy_ip))){
    print("Importing new proxy list after 6 hours.")
    
    proxy_url = read_html (proxy_url)
    proxy_list_raw = proxy_url %>%
      html_nodes ("div.table-responsive") %>%
      html_nodes ("tbody") %>%
      html_nodes ("td") %>%
      html_text()
    proxy_list1= list()
    for (f in seq(from=1, to=2400, by = 8)){
      proxy_list1 = rbind (proxy_list1, proxy_list_raw [f])
    }
    for (g in seq(from=2, to=2400, by = 8)){
      proxy_list1 = rbind (proxy_list1, proxy_list_raw [g])
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
    for (h in seq(from=1, to=14949, by=2)){
      proxy_ip2 = rbind(proxy_ip2, proxy_list2[h])
    }
    proxy_ip2 = unlist(proxy_ip2)
    proxy_ip2 = proxy_ip2[1:7474]
    proxy_port2=list()
    for (j in seq(from=2, to=14949, by=2)){
      proxy_port2 = rbind (proxy_port2, proxy_list2[j])
    }
    proxy_port2 = as.numeric(unlist(proxy_port2))
    
    # Compiling all proxies and port
    proxy_ip = c(proxy_ip1, proxy_ip2)
    proxy_port = c(proxy_port1, proxy_port2)
    
    # Reseting the z and y value
    z=1
    y=0
    
    #write.csv(total_df,"Week5.csv")
  }
}




# Leaving this here for future reference. Interesting find.
# By inputing "//input[@type='hidden']/@value" at to the last part of xpath you can extract hidden values not shown on webpage
# You may have to specify @name="SOMETHING" if it has one. Example below.
# //input[@type="hidden" and @name="token"]/@value






