# This function deals with spatial analysis of the metadata. All you need to have is the State name that It will automatically fill in for Area and Region. Simillarity, if you only have Region, it will add the Area (and NA to Location)

#dplyr::mutate()
# You can also use mutate() to remove variables and
# modify existing variables

#### NOTE: SECOND PART NOT WORKING (REGION) ####

Politic_Distribution <- function(Data,
                                 Level="Location",
                                 Save = FALSE){
  
  library(dplyr)

Mexico_Politico <- read_csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Data/Mexico_Politico.csv")
#View(Mexico_Politico)

if(Level =="Location"){
  
  x <- Data %>% 
    #joins both datasets base on Location
    left_join(Mexico_Politico,
              by=Level) %>% 
    #Substitutes Region and Area from Template with Region and Area from the Mexico_Politico dataset
    filter(Location %in% Mexico_Politico$Location) %>% 
    mutate(Region = Region_M) %>%  
    mutate(Area = Area_M) %>% 
    # Removes Mexico_Politico
    select(-Region_M,
           -Area_M)
  #Now we join the new dataset with the one non-subsetted one (a.k.a those that did had Location info.)
  
  Fixed <- Data %>%
    filter(!Location %in% Mexico_Politico$Location) %>% 
    bind_rows(x) %>% 
    #Arrange everything by MMID
    arrange(MMID)

# if(Level =="Region"){
#   Fixed <- Data %>%
#     left_join(Mexico_Politico,
#               by=Level) %>%
#     mutate(Location = "NA") %>%
#     mutate(Area = Area_M) %>%
#     select(-Region_M,
#            -Area_M)
# }
  
if(Save == TRUE){
write.csv(Fixed, "New_Template.csv",
          row.names = FALSE)
}
return(Fixed)
}
}


# x <- Template %>% 
#   #joins both datasets base on Location
#   left_join(Mexico_Politico,
#             by=D) %>% 
#   #Substitutes Region and Area from Template with Region and Area from the Mexico_Politico dataset
#   filter(D %in% Mexico_Politico$Location) %>% 
#   mutate(Region = Region_M) %>%  
#   mutate(Area = Area_M) %>% 
#   # Removes Mexico_Politico
#   select(-Region_M,
#          -Area_M)
# #Now we join the new dataset with the one non-subsetted one (a.k.a those that did had Location info.)
# 
# Fixed <- Template %>%
#   filter(!Location %in% Mexico_Politico$Location) %>% 
#   bind_rows(x) %>% 
#   #Arrange everything by MMID
#   arrange(MMID)
