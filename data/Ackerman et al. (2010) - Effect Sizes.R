##Calculating r and d effect sizes
library(compute.es)
options(digits = 5)


#For the first round data
tes(t=2.277, n.1=129, n.2=139, level=95, dig=2)

#For the whole data
tes(t=1.553, n.1=305, n.2=296, level=95, dig=2)
  