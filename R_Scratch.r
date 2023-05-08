decompose <- function (n) { 
  if (n<=2) {return(NULL)}
  target<-n^2
  possible_answers<-(1:(n-1))^2
  answers<-list()
  df<-merge(data.frame(a=possible_answers), data.frame(b=possible_answers), all=T)
  match_check<-df[,1]==df[, 2]
  match_check<-ifelse(match_check>0, FALSE, TRUE)
  df<-df[match_check, ]
  answer_check<-rowSums(df)==target
  if (sum(answer_check)>0) {
    answers<-append(answers, df[answer_check, ], (length(answers)+1))
  }
  for (itter in 3:n) {
    df<-merge(df, possible_answers, all=T)
    colnames(df)<-c(letters[1:itter])
    df<-df[rowSums(df)<=target, ]
    match_check<-rowSums(df[,itter]==df[, 1:itter-1])
    match_check<-ifelse(match_check>0, FALSE, TRUE)
    df<-df[match_check, ]
    answer_check<-rowSums(df)==target
    if (sum(answer_check)>0) {
      answers<-append(answers, df[answer_check, ], (length(answers)+1))
    }
  }
  return(answers)
}

print(Sys.time())
print(decompose(11))
print(Sys.time())

