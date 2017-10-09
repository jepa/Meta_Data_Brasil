#### This function inports dataframes from a list of names and merge them by row


Import_row_merge <- function(Path,Data_Guide){
  library(data.table)
  library(dplyr)

for(i in 1:length(Data_Guide)){
  # Name <- paste(Data_Guide[i])
  FilesLiga <- paste(Path,
                Data_Guide[i],
                sep="")
  
  x <- fread(FilesLiga)
  x <- x %>% 
    mutate(File = paste(Data_Guide[i]))
  
  x <- data.table(x)
  
  # print(Liga)
  # }
  
  if(i == 1){
    Data <- copy(x) # <- copies the previouse data.table
  }else{ 
    setkey(Data, # <- sets the data.table as "reference" ?setkey
           Species); setkey(x,
                            Species); 
    list <- list(Data, # <- creates a list to merge the tables
                 x)
    Data <- rbindlist(list, #<- Merges all the data in one single file.
                      use.names = TRUE, # <- This will merge columns by Name
                      fill = FALSE, # <- This allows for the NA's
                      idcol =  NULL)  # Columns id
  }
}
  return(Data)
}
