
HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]));
visstat(HairEyeColorMale,"Hair","Eye")


visstat(ToothGrowth,"len", "supp")#wilcoxon
#error: old plots are not erased but overwritten
visstat(iris,"Petal.Width", "Species") #kruskal wallis testing normality assumptions generating two plots just overlayed first plot



#2x2 contingency tables---
HairEyeColorMaleFisher = HairEyeColor[,,1]
#slicing out a 2 x2 contingency table
blackBrownHazelGreen = HairEyeColorMaleFisher[1:2,3:4]
fishertest = blackBrownHazelGreen
blackBrownHazelGreen = counts_to_cases(as.data.frame(blackBrownHazelGreen));
visstat(blackBrownHazelGreen,"Hair","Eye")

