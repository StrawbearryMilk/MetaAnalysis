getN_Age <- function(df,col1,index){#df is data, col1 and col2 are the column vectors for N and mean age

  size = 0
  age = NA
  result = c(index,size,age)
  digits = 2
  initPosition = digits+1
  
  numlist = c("0","1","2","3","4","5","6","7","8","9")
  string = df[index,col1]
  if (grepl("\n",string) | grepl("[[:alpha:]]",string)){
    return (c(index,NA,NA))
  }
  
  else if (substr(string, 1, 1) %in% numlist){
    #if the 3rd digit is an integer, set digit to 3, else 2 (no other cases exist)
    digits = ifelse(substr(string, 3, 3) %in% numlist, 3, 2)
    num = as.numeric(substr(string,1,digits))
    initPosition = digits+1

    charAt = substr(string,initPosition,initPosition)
    if (charAt == " "){
      initPosition = initPosition + 1
      charAt = substr(string,initPosition,initPosition)
      
    }
    
    if (charAt == "("){
      initPosition = initPosition + 1
      charAt = substr(string,initPosition,initPosition)
    }

        if (charAt %in% numlist){
      numChars = 0
      ageString <- substr(string, initPosition, nchar(string))
      i = 1
      while (charAt %in% numlist | charAt == "."){
        i <- i+1
        charAt <- substr(ageString,i,i)
      }
      age = substr(ageString,1,i-1)
      age = as.numeric(age)
      if (substr(ageString,i,i) == ")"){
        return(c(index, num, age))
      }
      else{
        return (c(index,num,NA))
      }
    }
    
    else{
      return (c(index,num,NA))
    }
  }
  else{
    #browser()
    return (c(index,NA,NA)) #row to be removed
  }
}

#cleaning data frame

# saveAsN <- function(digits, string){
#   result <- substr(string,1,digits)
#   return (as.numeric(result))
# }
# 
# saveAsAge <- function(digits, string){
#   
# }
# del.row <- function(df,index){
#   data <- data[-index,]
# }