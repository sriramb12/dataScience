setwd('/Users/bsriram/datasci/june/wk2/stats')
experience = read.table("group.csv",sep = ",",header = T, na.strings=c('NA','?'))
class(experience)
print(mean(as.matrix(experience)))
print(experience)
for (i in experience)
{
  print(experience$i)
}
dd
print(mean(as.matrix(experience)))
print(summary(experience))

deliverySvcs = read.table("deliveryserivces.csv",sep = ",",header = T, na.strings=c('NA','?'))
print(deliverySvcs)

### Function to calculate the given stats
```{r}
stats = function(x)
{
   res = c("mean" = mean(x),
   "med" = median(x,
   "q1" = quantile(x)[2],
   "q2" = quantile(x)[3],
   "q3" = quantile(x)[4],
   "min" = range(x)[1],
   "max" = range(x)[2],
   "iqr" = IQR(x),
   "var" = var(x),
   "sd" = sd(x))
   return(res)
}

apply(experience, stats)
```



