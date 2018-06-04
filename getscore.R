getscore <- function(filename="FinalExam.Rmd"){
  
  files <- dir()
  if(filename %in% files){
    tmp <- readLines(filename)
    tmp1 <- grep("## Question", tmp, value=TRUE)
    tmp2 <- sapply(tmp1, function(x) strsplit(x, "[{]|[}]")[[1]][2])
    names(tmp2) <- 1:25
    if(length(grep("[a-d]", tmp2))<25){
      message <- "Not all questions Answered"
    } else{
      tmp3 <-              c(Q1="b",
                             Q2="c",
                             Q3="c",
                             Q4="a",
                             Q5="b",
                             Q6="a",
                             Q7="d",
                             Q8="b",
                             Q9="d",
                             Q10="a",
                             Q11="d",
                             Q12="c",
                             Q13="b",
                             Q14="a",
                             Q15="c",
                             Q16="a",
                             Q17="b",
                             Q18="b",
                             Q19="d",
                             Q20="b",
                             Q21="a",
                             Q22="a",
                             Q23="d",
                             Q24="a",
                             Q25="a")
      
      message <- paste(sum(tmp3 == tmp2), "correct")
    }
  } else{
    message <- "Your Exam not found. Please type getscore(\"FilePathToYourFile\")."
  }
  message
}