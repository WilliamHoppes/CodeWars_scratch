

decompose <- function (n) { 
  possible_answers<-(1:(n-1))^2
  first<-(rep(possible_answers, times=4))
  second<-(rep(possible_answers, each=4))
  df<-data.frame(first=first, second=second)
  new_df<-data.frame(dummy=rep(0, times=nrow(df)*4))
  for (i in 1:ncol(df)) {
    new_df[, c(colnames(df)[i])]<-rep(df[, c(colnames(df)[i])], times=4)
    new_df$third<-rep(possible_answers, each=4^2)
  }
  new_df<-subset(new_df, select=-c(dummy))
  new_df<-new_df[, c("first", "second", "third")]
  print(df)
  print(new_df)
}

decompose <- function (n) { 
  target<-n^2
  possible_answers<-(1:(n-1))^2
  df<-data.frame(a=possible_answers)
  df<-merge(df, possible_answers, all=T)
  colnames(df)<-c(letters[1:2])
  df<-df[rowSums(df)<=target, ]
  df<-merge(df, possible_answers, all=T)
  colnames(df)<-c(letters[1:3])
  df<-df[rowSums(df)<=target, ]
  match_check=rowSums(df[,3]==df[, c(1,2)])
  match_check<-ifelse(match_check>0, FALSE, TRUE)
  print(match_check)
  df<-df[match_check, ]
  print(nrow(df))
  return(df)
}

print(decompose(5))

