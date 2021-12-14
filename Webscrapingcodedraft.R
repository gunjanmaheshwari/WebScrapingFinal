

#https://www.pbs.gov.pk/node/3391/?name=001

# Created folders to extract data separately for analysis
pathway <- ("/Users/gunjanmaheshwari/Documents/Semester3/Intro to Data Science/Final Project/Provincial data/")
folder_names <- mixedsort(list.files("/Users/gunjanmaheshwari/Documents/Semester3/Intro to Data Science/Final Project/Provincial data/"))

# Creating a loop for pattern
val <- 1:135

for (x in val) {
  if (val<10){
    e_r <- paste0(pathway,folder_names[val],val,"09.pdf")
    e_mt <- paste0(pathway,folder_names[val],val,"11.pdf")    
    
  } else {
    e_r <- paste0(pathway,folder_names[val],"/0",val,"09.pdf")
    e_mt <- paste0(pathway,folder_names[val],"/0",val,"11.pdf")  
  } 
  
  if (val>=100){
    e_r <- paste0(pathway,folder_names[val],"/",val,"09.pdf")
    e_mt <- paste0(pathway,folder_names[val],"/", val,"11.pdf")
  }

  # Using function above to create pathway to paste all pdfs
  path_extract <- paste0(pathway,folder_names[val],"/Extracted/")
  
  
  # Converting text to character vector of equal length
  PDF <- pdf_text(e_r) %>% 
    readr::read_lines() 
  
  # Replacing ALL SEXES with ALL-SEXES 
  PDF <- str_replace(PDF, "ALL SEXES", "ALL-SEXES")
  
  # Calculating Index of overall term
  
  check <- 0
  index <- which(PDF == "OVERALL")
  
  if (length(index) == 0){
    index <- which(str_detect(PDF, "ALL-SEXES")) 
    check <- 1
  }
  
  # Creating Empty List
  table_name<- rep(NA, length(index))
  
  # Name of Variables in the pdf
  variables <- c("GENDER","TOTAL","MUSLIM","CHRISTIAN","HINDU","QADIANI/SCHEDULED","CASTES","OTHERS")
  
  # Extracting Table Names
  for (x in 1:length(index)){
    table_name[x] = str_squish(PDF[index[x] - 1])
  }
  
  # Creating an Empty DataFrame
  mat = matrix(ncol = 0, nrow = 0)
  district_religion = data.frame(mat)
  
  for (x in 1:length(index)){
    
    # Initializing initial and final rows of each section
    
    if (check == 0){
      start_end_overall <- c(index[x]+1)
    } else {
      start_end_overall <- c(index[x])
    }
    
    
    # Selecting the rows for Overall, Rural and Urban
    PDF.overall <- PDF[c(start_end_overall[1])] %>% str_squish() %>%
      strsplit(split = " ") %>% plyr::ldply(.) %>% setNames(variables)
    
    # Creating New Variables
    PDF.overall$DISTRICT <- c(list.files("/Users/gunjanmaheshwari/Documents/Semester3/Intro to Data Science/Final Project/")[val])
    PDF.overall$TEHSIL <- c(table_name[x])
    
    district_religion <- rbind(district_religion,PDF.overall)
    
  }
  
  # Writing the data into CSV
  write.csv(district_religion,paste0(path_extract,folder_names[val],"-overall_religion",".csv"),row.names = F)
  

  # mother tongue
  
  PDF <- pdf_text(e_mt) %>% readr::read_lines() 
  
  # Replacing ALL SEXES WITH ALL-SEXES 
  PDF <- str_replace(PDF, "ALL SEXES", "ALL-SEXES")
  
  # Calculating index
  
  check <- 0
  index <- which(PDF == "OVERALL")
  
  if (length(index) == 0){
    index <- which(str_detect(PDF, "ALL-SEXES")) 
    check <- 1
  }
  
  # Creating Empty List
  table_name<- rep(NA, length(index))
  
  # Name of Variables
  variables <- c("GENDER","TOTAL","URDU","PUNJABI","SINDHI","PUSHTO","BALOCHI","KASHMIRI","SARAIKI","HINDKO","BRAHVI","OTHERS")
  
  # Extracting Table Names
  for (y in 1:length(index)){
    table_name[y] = str_squish(PDF[index[y] - 1])
  }
  
  # Creating an Empty DataFrame
  district_mother_tongue = data.frame(mat)
  
  for (y in 1:length(index)){
    
    # Initializing initial and final rows of each section
    
    if (check == 0){
      start_end_overall <- c(index[y]+1)
    } else {
      start_end_overall <- c(index[y])
    }
    
    
    # Selecting the rows for Overall, Rural and Urban
    PDF.overall <- PDF[c(start_end_overall[1])] %>% str_squish() %>%
      strsplit(split = " ") %>% plyr::ldply(.) %>% setNames(variables)
    
    # Creating New Variables
    PDF.overall$DISTRICT <- c(list.files("/Users/gunjanmaheshwari/Documents/Semester3/Intro to Data Science/Final Project/")[val])
    PDF.overall$TEHSIL <- c(table_name[y])
    
    # Appending data of all districts
    district_mother_tongue <- rbind(district_mother_tongue,PDF.overall)
    
    
    # Writing the data into CSV
    write.csv(district_mother_tongue,paste0(path_extract,folder_names[val],"-overall_mother_tongue",".csv"),row.names = F)
    
    
  }
  
  