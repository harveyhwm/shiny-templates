#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LIBRARY OF HELPER FUNCTIONS & MODULES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

getSqlData <- function(s,p=NULL,d=NULL,f=NULL){
  sql_filters <- applyFiltersSQL(f)
  setDT(sqlConnectionHandler(s(p,d,sql_filters),connectType,'query'))
}

getSqlDataNew <- function(s,p,d,c,m,a,pr,t,i){
  setDT(sqlConnectionHandler(s(p,d,c,m,a,pr,download=t,filename=i),connectType,'query'))
}

titlec <- function(x){
  s <- strsplit(x, ' ')[[1]]
  paste(toupper(substring(s,1,1)),tolower(substring(s, 2)),sep='',collapse=' ')
}

default_rows <- function(x){
  if(is.null(x)){x <- 15}
  x
}

calcDiff <- function(x,y){
  if(is.infinite(x)||is.infinite(y)||is.nan(x)||is.nan(y)||is.na(x)||is.na(y)||y==0){y=0}
  ifelse(y==0,na_val,paste0(format(round(100*(x-y)/y,2),nsmall=2),"%"))
}

getDateRange <- function(r,p='None'){
  if(grepl('^(m|q|y)',r)){
    m <- ifelse(grepl('^m',r),month(Sys.Date()),ifelse(grepl('^q',r),(3*quarter(Sys.Date()))-2,'01'))
    o <- ifelse(grepl('^m',r),1,ifelse(grepl('^q',r),3,12))
    date1 <- as.Date(paste(year(Sys.Date()),m,'01',sep='-')) %m-% months(ifelse(grepl('p$',r),o,0))
    date2 <- date1 %m+% months(o)-1
    date3 <- date1 %m-% months(ifelse(grepl('^(N|P)',p),o,12))
    date4 <- date3 %m+% months(o)-1
  }
  else if(grepl('^w',r)){
    date1 <- as.Date(Sys.Date()+1-as.POSIXlt(Sys.Date())$wday-ifelse(grepl('p$',r),7,0))
    date2 <- date1+6
    date3 <- (date1-ifelse(grepl('^(N|P)',p),7,0)) %m-% years(ifelse(grepl('^(N|P)',p),0,1))
    date4 <- date3+6
  }
  else if(grepl('[0-9]{1}',r)){
    date1 <- Sys.Date() - as.numeric(r)
    date2 <- Sys.Date()-1
    date3 <- (date1 - ifelse(grepl('^(N|P)',p),as.numeric(r),0)) %m-% years(ifelse(grepl('^(N|P)',p),0,1))
    date4 <- date3+as.numeric(r)-1
  }
  c(date1,date2,date3,date4)
}

getMyQuarters <- function(d){
  gsub('^01','Q1',gsub('^04','Q2',gsub('^07','Q3',gsub('^10','Q4',d))))
}

roundval <- function(x,n){
  if(x==na_val||is.na(x)||is.infinite(x)||is.nan(x)){x=0}
  round(x,n)
}

addSortColumns <- function(s,n){
  cols <- c()
  cols_new <- c()
  for (i in 1:ncol(s)){
    if(i <= n){
      eval(parse(text=paste0('s$`',names(s)[i],'_rank` <- s$`',names(s)[i],'`')))
    }
    else{
      tryCatch({
        eval(parse(text=paste0('s$`',names(s)[i],'_rank` <- s$`',names(s)[i],'`/sum(s$`',names(s)[i],'`)')))
      },error = function(err){
        eval(parse(text=paste0('s$`',names(s)[i],'_rank` <- 0')))
      })
      eval(parse(text=paste0('s$`',names(s)[i],'_rank` <- ifelse(s$`',names(s)[i],'_rank` < 0,-1-s$`',names(s)[i],'_rank`,s$`',names(s)[i],'_rank`)')))
    }
    cols <- c(cols,names(s)[i],paste0(names(s)[i],'_rank'))
    cols_new <- c(cols_new,paste0(names(s)[i],'_rank'),names(s)[i])
  }
  s <- data.table(s)[,cols,with=F]
  setnames(s,old=cols,new=cols_new)
  s
}

applyFilters <- function(d,f){
  for(i in 1:(len(f)/2)){
    if(!grepl('^All',f[2*i])){
      d <- eval(parse(text=paste0('d[d$`',f[(2*i)-1],'` == "',f[2*i],'",]')))
    }
  }
  d
}

applyFiltersSQL <- function(f){
  q <- ''
  if(!is.null(f) && f!=''){
    for(i in 1:(len(f)/2)){
      if(!grepl('^All',f[2*i])){
        if(f[(2*i)-1] != 'max_level'){
          f[2*i] <- paste0("'",f[2*i],"'")
        }
        q <- paste0(q," AND ",f[(2*i)-1]," IN (",f[2*i],")")
      }
    }
  }
  q
}

makePercent <- function(n,d){
  ifelse(is.na(n),'--',paste0(format(round(as.numeric(n),d),nsmall=d),"%"))
}

renameCols <- function(s,o,n){
  o2 <- c()
  n2 <- c()
  for(i in 1:length(o)){
    if(o[i] %in% names(s)){
      o2 <- c(o2,o[i])
      n2 <- c(n2,n[i])
    }
  }
  if(length(n2) > 0){setnames(s,old=o2,new=n2)}
}

rowCompleter <- function(s,r){
  for(i in 2:ncol(s)){eval(parse(text=paste0('s$`',names(s)[i],'` <- as.numeric(as.character(s$`',names(s)[i],'`))')))}
  k <- data.table(r[!r %in% s[,1]],replicate(ncol(s)-1,numeric(len(r[!r %in% s[,1]]))))
  names(k) <- names(s)
  s <- rbind(k,s)
  eval(parse(text=paste0('s[,names(s),with=F][,lapply(.SD,sum),by=eval(names(s)[1])][order(`',names(s)[1],'`)]')))
}

divideZero <- function(x,y,val=1){
  if(is.na(y) || y==0){s <- val}
  else{s <- round(as.numeric(x/y),2)} #format(round(as.numeric(x/y),2),nsmall=2)}
  s
}

zeroToBlank <- function(x){
  if(x == 0){x <- '--'}
  x
}

getNums <- function(q,p,d,f=NULL,a=NULL){
  sql_filters <- applyFiltersSQL(f)
  s <- setDT(sqlConnectionHandler(q(p,d,sql_filters),connectType,'query'))
  if(!is.null(a)){s <- s[s$award_short_description %in% a,]}
  s
}

makeDT <- function(s,r,total=NULL,ord=T,print=F){
  if(!is.null(total)){s[,1] <- 'Total'}
  if(print==F){
    datatable(s,options=list(pageLength=default_rows(r),ordering=ord),escape=F,rownames=F)
  }
  else{
    setDT(s)
  }
}

makeTable <- function(s,total=NULL){
  if(!is.null(total)){s[,1] <- 'Total'}
  as.data.frame(s)
}

deltaMaker <- function(x,y,val=0){
  ifelse(as.character(y)=='0','--',as.character(round((as.numeric(x)-as.numeric(y))/as.numeric(y),4)))
}

formx_numeric <- function(x){
  ifelse(x=='--',x,format(as.numeric(x),big.mark=','))
}

formx_percent <- function(x){
  ifelse(x=='--',x,makePercent(100*as.numeric(x),1))
}

myCatch <- function(try=NULL,warn=NULL,err=NULL,fin=NULL){
  z <- tryCatch({
    try
  }, warning = function(w) {
    try
  }, error = function(e) {
    err
  }, finally = {
    try
  })
  z
}

stripNaCols <- function(dt){ #remove NA or '--'
  dt[,which(unlist(lapply(dt,function(x)!all((x=='--')||is.na(x))))),with=F]
}

abcxyz <- 'xlsx_magic <- function(x, path = tempfile(fileext = ".xlsx"), col_names = TRUE, format_headers = TRUE){
  if (is.data.frame(x)) 
    x <- list(x)
  if (!is.list(x) || !all(vapply(x, is.data.frame, logical(1)))) 
    stop("Argument x must be a data frame or list of data frames")
  x <- lapply(x, normalize_df)
  stopifnot(is.character(path) && length(path))
  path <- normalizePath(path, mustWork = FALSE)
  ret <- .Call(C_write_data_frame_list, x, path, col_names, 
               format_headers)
  invisible(ret)
}

normalize_df <- function(df){
  # Types to coerce to strings
  for(i in which(vapply(df, inherits, logical(1), c("factor", "hms")))){
    df[[i]] <- as.character(df[[i]])
  }
  for(i in which(vapply(df, inherits, logical(1), "POSIXlt"))){
    df[[i]] <- as.POSIXct(df[[i]])
  }
  for(i in which(vapply(df, inherits, logical(1), "integer64"))){
    warning(sprintf("Coercing columnn %s from int64 to double", names(df)[i]), call. = FALSE)
    df[[i]] <- bit64::as.double.integer64(df[[i]])
  }
  df
}'