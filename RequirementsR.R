pkgs=c("readr",
       "dplyr","stats","base","ggplot2","knitr","tidyr","randomForest","lubridate","corrplot","Boruta")

for(p in pkgs){
  if (!require(p,character.only=TRUE)) install.packages(p)
}
