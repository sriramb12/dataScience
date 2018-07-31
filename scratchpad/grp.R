d=c(rep(letters[1:10],2))
d.f <- factor(d)
print(as.numeric(d.f))

df <- data.frame(a=runif(12),b=c(runif(11),NA), 
                 z=rep(letters[13:15],4),w=rep(letters[20:23],3))

print (df)
ex
group_by(df=df, by='w', clmns=c(rep(c('a','b'),3),'z','w'), 
 aggregation=c('sum','mean','var','sd','min','max','paste','length'), 
 full.names=TRUE, na.rm=TRUE)
# or using SQLite
groupBy(df=df, by='w', clmns=c(rep(c('a','b'),2),'z','w'), 
        aggregation=c('sum','mean','min','max','paste','length'), 
        full.names=TRUE, sql=TRUE)


## passing a custom function
meantop <- function(x,n=2, ...)
  mean(x[order(x, decreasing=TRUE)][1:n], ...)
  
groupBy(df, by='w', aggregation=rep(c('mean','max','meantop'),2), 
                    clmns=rep(c('a','b'),3), na.rm=TRUE)
