
if (length(dev.list())>0) dev.off() #closes all graphical devices
oldpar =par(no.readonly = TRUE)
visstat(InsectSprays,"count","spray")
on.exit(par(oldpar))
par("new")
test_norm(trees$Girth)
par("new")

#Check t-Test-----
InsectSpraysAB <- InsectSprays[ which(InsectSprays$spray == 'A'
| InsectSprays$spray == 'B'), ]
InsectSpraysAB$spray = factor(InsectSpraysAB$spray)

#welcht-t-Test
visstat(InsectSpraysAB,"count","spray")
test_norm_vis(InsectSpraysAB$count)
par("new")

#Wilcoxon
visstat(ToothGrowth,"len", "supp")
par("new")

