
#This function converts the reference category of the metadataset template into actual "clickable" links.

#Variables: 
#Link, the actual urll "www.somewhere.com"
#Reference, the name that will be displayed in the Dataset

Ref_Links <- function(Link,Reference){
 
  Result = paste("<a href=",Link,">",Reference,"</a>") 
  Result = data.frame(Result)  
  
  return(Result)
  }
  