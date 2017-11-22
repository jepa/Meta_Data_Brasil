### For Datamares ####
# This script generates the dadtaset for DataMates # And back

# Libraries
library(data.table)
library(dplyr)

Template <- fread("~/Documents/Github/Meta_Data_Mexico/App_Eng/Template.csv",
               colClasses = c(Location = 'character',
                              Notes = 'character',
                              Data_Uncertanty ='character'))


DataMares_MD <- Template %>% 
  filter(Author != "DataMares") %>% 
  select(MMID,
         Research_Field,
         Short_Title,
         Subject_name,
         Area,
         Region,
         Keywords)
  
write.csv(DataMares_MD,
          "DataMares_MD.csv",
          row.names = FALSE
          )


# Para adicionar los datos de dataMares

Pre_Template <- ec_cp_endemism_metadata %>% 
  slice(
    c(
      2,3,5,6,7,11,17,45)
  ) %>% 
  select(1:2) %>% 
  tidyr::spread(
    X0,X1
  )



  
