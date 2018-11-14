#Helper function in visstat----
#' #' Reads in csv  or json files and  converts it into a R  data.frame
#' #'
#' #'For file of json-type function fromJSON from the jsonlite package is called, for files of type csv  functions read.csv resp. read.cs2 from the utils package  are called.
#' #' @param input_file data file of type csv or json with explicit file ending ".cvs" or ".json"
#' #'
#' #' @return data.frame or list generated from input_file
#' #' @examples
#' #' # Stringify some data
#' #' jsoncars <- toJSON(mtcars, pretty=TRUE)
#' #' 
#' #' 
#' #' @import jsonlite utils
#' #' @export read_input_file
#' read_input_file = function(input_file)
#' {
#'   #reads in files of type csv or json
#'   #automatically distinguises type of csv file
#'   filetye = sub('.*\\.', '', input_file)#sub(pattern, replacement,file), replaces everything before . with empty string
#'   if (filetype == "csv")
#'   {
#'     L <- readLines(input_file, n = 1)
#'     if (grepl(";", L))
#'     {
#'       dataframe = read.csv2(input_file,
#'                             dec = ".",
#'                             sep = ";",
#'                             header = TRUE) #sep=";" is the defualt of read.csv2
#'     } else {
#'       dataframe = read.csv(input_file,
#'                            dec = ".",
#'                            sep = ",",
#'                            header = TRUE)
#'       return(dataframe)
#'     }
#'   } else if (filetype == "json") {
#'     dataframe <- fromJSON(file(input_file)) #this is a list
#'     return(dataframe)
#'     
#'   } else
#'   {
#'     stop("code can only input files of type 'csv' or 'json'.")
#'   }
#'   
#' }

# get_groups_inputfile of type json (server) or data.frame ---------------------

#' Selects columns defined by characters varsample and varfactor from dataframe
#'
#'Selects columns defined by characters \code{varsample} and \code{varfactor} from \code{dataframe}, return selected columsn with their names
#' @param dataframe \code{data.frame} or \code{list} containing at least two columns with column headings of data type \code{character}. Data must be column wise ordered.
#' @param varsample column name of dependent variable in dataframe, dataype \code{character}
#' @param varfactor column name of independent variable in dataframe, dataype \code{character}

#'
#' @return selected columns, \code{varsample}, \code{varfactor}
#' @examples
#'
#' @export
get_samples_fact_inputfile = function(dataframe, varsample, varfactor)
{
  # json input------
  
  if (is.null(dim(dataframe)))
    #FALSE for csv
  {
    fulldata = dataframe
    data = dataframe$data
    data = as.data.frame(data)
    
    if ("matching" %in% names(dataframe) & varfactor == "match")
    {
      matched_selected_group0 = which(data$group0 == 1 & data$match == 1)
      matched_selected_group1 = which(data$group1 == 1 &
                                        data$match == 1)
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
      samples = data[, varsample]
      fact = data[, varfactor]
      name_of_sample = varsample
      name_of_factor = varfactor
      matchingCriteria = ""
    } else{
      stop("code runs only on json files generated from AneuX")
    }
    # csv input------
  } else{
    #Select samples and fact from data.frame dataframe-----
    samples = dataframe[, varsample]
    fact = dataframe[, varfactor]
    name_of_sample = varsample
    name_of_factor = varfactor
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

findmatches = function(samples, matchedsamples)
{
  if (is.null(dim(samples))) {
    samplestest = samples[which(matchedsamples == TRUE)]
  } else{
    samplestest = samples[which(matchedsamples == TRUE),]
  }
  return(samplestest)
}

