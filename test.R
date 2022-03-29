library ("knitr")
library ("rvest")
library ("xml2")
library("stringr")
complete_df = data.frame ()
table_df = data.frame()

#Extracting the handles from the main page
handles <- read_html("http://bdm.unb.br/handle/10483/1")

handles = handles %>% html_nodes ("div.list-group") %>% html_nodes ("a") %>% html_attr("href")

#Main outer loop

#Changing the handles for each loop
for (i in 1:length(handles)) {
  ggg = paste0("http://bdm.unb.br", handles[i])
  hhh = paste0 (ggg, "?offset=")
  
  url_raw <- hhh
  url_main = read_html(url_raw)
  
  # Splitting the course name from the number of people
  course_raw = url_main %>%
    html_nodes("div.row") %>%
    html_nodes("h2") %>%
    html_text ()
  course_split = (strsplit(course_raw, ":"))
  course_split =unlist (course_split)
  
  
  
  course_name=course_split[1]
  course_accent=gsub("\\n* *","", course_name)
  course_accent
  course_clear = course_accent
  
  
  course_split_number = course_split[2]
  course_number_str <- regmatches(course_split_number, regexpr("[0-9]+", course_split_number))
  course_number = strtoi(course_number_str, base = 0L)
  
  
  
  # Condition for if there are no people in the class
  if (course_number == 0) {
    url_html = read_html(url)
    table_raw = url_html %>%
      html_nodes("div.container") %>%
      html_nodes("table.table") %>%
      html_table ()
    
    table_list = table_raw 
    
  # Condition for courses with people
  } else {
    
    # Condition for courses with amount of people divisible by 20
    if ((course_number %% 20)==0){
      
      course_twenty = course_number-20
      
      for (page in seq(from=0, to=course_twenty, by=20)) {
        Sys.sleep(3)
        
        url = paste0(hhh, page)
        
        
        
        url_html = read_html(url)
        table_raw = url_html %>%
          html_nodes("div.container") %>%
          html_nodes("table.table") %>%
          html_table ()
        
        # Condition for if it's the first page
        if (page == 0){
          table_df = table_raw[[1]]
        } else {
          table_list = table_raw [[1]]
          table_df = rbind(table_df, table_list)
        }
          # Condition for the last page
          if (page == course_twenty) {
          Course = rep(course_clear, nrow(table_df))
          table_df = cbind(table_df, Course)
          complete_df = rbind (complete_df, table_df)
        }
      } 
    } else {
      
      # Condition for all other courses
      for (page in seq(from=0, to=course_number, by=20)) {
        
        Sys.sleep(3)
        
        url = paste0(hhh, page)
        
        
        url_html = read_html(url)
        table_raw = url_html %>%
          html_nodes("div.container") %>%
          html_nodes("table.table") %>%
          html_table ()
        
        if (page == course_number) {
          table_df = rbind(table_df, table_raw[[1]])
        } else {
          table_list = table_raw [[1]]
        }
        
        
        if (page == 0){
          table_df = table_list
          
        } else {
          table_df = rbind(table_df, table_list)
          
        }
        if (page == (floor(course_number/20))*20) {
          Course = rep(course_clear, nrow(table_df))
          table_df = cbind(table_df, Course)
        }
      }
      if (handles[i] == "/handle/10483/6") {
        complete_df = table_df
      } else {
        complete_df = rbind(complete_df, table_df)
      }
    } 
  }
}


write.csv(complete_df, "C:\\Program Files\\RStudio\\My Work\\Week3DataFrame.csv")