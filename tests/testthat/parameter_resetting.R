


oldpar =par(no.readonly = TRUE)
visstat(InsectSprays,"count","spray",graphicsoutput = "png") 
on.exit(oldpar)
par("new")
