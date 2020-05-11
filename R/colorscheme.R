#Define color scheme-----
#MIT License----
#Copyright (c) 2020 Sabine Schilling
#Feedback highly welcome: sabineschilling@gmx.ch

# Header colorscheme -----

#'\code{colorscheme(x)} selects color scheme of graphical output. Function parameter NULL lists all available color schemes, 1 a color tuple of green and blue
#'2 a color tuple of dark green and turquoi, 3 a colorplaette as defined by RcolorBrewer

#'
#' @param colorcode selects color scheme. parameters NULL: list of all available color schemes, 1: colortuple, 2, colortuple2, 3, ColorPalette
#' @return selected color scheme, colors are given with their Hex Code #RRGGBB names


colorscheme = function(colorcode=NULL)
{
  browserLightGreen="#B8E0B8" #matched part group0
  browserLightBlue="#B3D1EF"#matched part group1
  browserLightTurquois="#B3E1EF"#light turquois
  browserDarkGreen="#5CB85C" #dark green
  colortuple=c(browserLightGreen,browserLightBlue)
  colortuple2=c(browserDarkGreen,browserLightTurquois)
  #from package RColorBrewer Set 3
  ColorPalette= c("#8DD3C7" ,"#FFFFB3" ,"#BEBADA" ,"#FB8072", "#80B1D3",
                  "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD" ,"#CCEBC5" ,"#FFED6F")
  
  
  mylist = list(
    "colortuple" = colortuple,
    "colortuple2" = colortuple2,
    "ColorPalette" = ColorPalette
  )
  
  if (is.null(colorcode) )
  {return(mylist)}
  else if (colorcode==1){
    return(colortuple)
  }else if (colorcode==2){
    return(colortuple2)
  }else if   (colorcode==3){
    return(ColorPalette)
    }else{
      message("Choose valid parameter: NULL, 1,2 or 3")
  }
  
  
  
  
  
}