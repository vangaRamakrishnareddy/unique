
library(dplyr)
library(tidyr)
library(splitstackshape)
df<- read.csv("data.csv")

# removing if any two rows are same
df<- unique(df)
df$ln<- as.character(df$ln)
df$fn<- as.character(df$fn)


# spliting the ln column into words
df4 <- df %>%
  mutate( parts = strsplit(ln, " ", fixed=FALSE) ) %>% 
  group_by( ln ) %>%
  do(  data.frame( 
    { 
      idx <- 1:length(.$parts[[1]])
      lst <- lapply(idx,
                    function(x) .$parts[[1]][x])
      names(lst) <- lapply(idx,
                           function(x) paste("ln",x,sep="") )
      
      (lst)
    } , stringsAsFactors=FALSE)
  ) %>% 
  inner_join(df,by="ln")

df4<- arrange(df4, dob,ln,fn,gn)


#spliting the fn column into words
df5 <- df %>%
  mutate( parts = strsplit(fn, " ", fixed=FALSE) ) %>% 
  group_by( fn ) %>%
  do(  data.frame( 
    { 
      idx <- 1:length(.$parts[[1]])
      lst <- lapply(idx,
                    function(x) .$parts[[1]][x])
      names(lst) <- lapply(idx,
                           function(x) paste("fn",x,sep="") )
      
      (lst)
    } , stringsAsFactors=FALSE)
  ) %>% 
  inner_join(df,by="fn")
df5<- arrange(df5, dob, ln,fn, gn)
df<- arrange(df, dob, ln,fn, gn)


data<- cbind(df4,df5)

data<- data[,-c(7,10,11,12)]

uniquepersons<- data[,-c(1,3,6,8)]

# final dataset which contains only unique persons
uniquepersons<- unique(uniquepersons)



write.csv(uniquepersons,"output.csv")



