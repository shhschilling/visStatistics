#Helper function in visstat----
#' Generate name of dependent variable(varsample) and name of dependent variable from dataframe
#'
#'Selects columns defined by characters \code{samples} and \code{fact} from \code{dataframe}, return selected columsn with their names
#' @param dataframe \code{data.frame} or \code{list} containing at least two columns with column headings of data type \code{character}. Data must be column wise ordered.
#' @param samples column of selected dependent variable in dataframe
#' @param fact column of selected independent variable in dataframe 

#'
#' @return selected columns, \code{samples}, \code{fact}, \code{name_of_sample}  \code{name_of_factor} (character string equaling varsample)
#' @examples
#' get_varsamples_varfact_inputfile(trees,trees$Girth",trees$Height")
 

get_varsamples_varfact_inputfile = function(dataframe, samples,fact)
{
  # json input------
  
  if (is.null(dim(dataframe)))
    #FALSE for csv
    
    #Code only relevant for ANEUX
  {
    fulldata = dataframe
    data = dataframe$data
    data = as.data.frame(data)
    
    if ("matching" %in% names(dataframe) & fact == "match")
    {
      matched_selected_group0 = which(data$group0 == 1 & data$match == 1)
      matched_selected_group1 = which(data$group1 == 1 & data$match == 1)
      fact = c(rep(fulldata$group0name, length(matched_selected_group0)),
               rep(fulldata$group1name, length(matched_selected_group1)))
      fact = as.factor(fact)
      fullsample = data[, varsample]
      
      samples = fullsample[c(matched_selected_group0, matched_selected_group1)]
      name_of_factor = paste(fulldata$group0name, "and", fulldata$group1name)
      #name_of_factor="groups"
      name_of_sample = varsample
      # does not work on multiple matching criterias:
      # matchingCriteria=paste(tolower(paste(as.character(dataframe$matching),collapse =" ")),sep="")
      matchingCriteria = tolower(paste(
        apply(dataframe$matching, 1, function(x)
          paste(x, collapse = " ")),
        collapse = ', '
      ))
      name_of_sample = paste(name_of_sample, "with match:", matchingCriteria)
      # json file with no matching criterion
    } else if ("matching" %in% names(dataframe) & varfactor != "match")
    {
      
      name_of_sample =bla
      name_of_factor = bla2
      matchingCriteria = ""
    } else{
      stop("code runs only on json files generated from AneuX")
    }
    # csv input------
  } else{
    #Select samples and fact from data.frame dataframe-----
    name_of_sample = ...
    name_of_factor = ...
    matchingCriteria = ""
  }
  
  
  # samples are the two groups selected by the user in groups at the web surface
  mylist = list(
    "samples" = samples,
    "fact" = fact,
    "name_of_sample" = name_of_sample,
    "name_of_factor" = name_of_factor,
    "matchingCriteria" = matchingCriteria
  )
  return(mylist)
}



