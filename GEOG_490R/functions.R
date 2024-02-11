#functions

new_fun <- function(x){
  y <- x*3
  return(y)
}

new_fun(33)



read_csv_better <- function(x){
  read_csv(x) %>% 
    clean_names()
}


read_csv_better("./snotel820_timp.csv")
