visstat(ToothGrowth,"len", "supp")
visstat(iris,"Petal.Width", "Species")

HairEyeColorMale = counts_to_cases(as.data.frame(HairEyeColor[,,1]));
visstat(HairEyeColorMale,"Hair","Eye")