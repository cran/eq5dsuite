#' @title toEQ5DIndex
#' @description Generate EQ-5D state vector from data.frame/matrix or named vector with dimensions.
#' @param x A data.frame, matrix, or vector containing dimension levels. Should have column names corresponding to the dim.names argument.
#' @param dim.names A vector of dimension names in data.frame/matrix/vector x
#' @return A vector of 5-digit EQ-5D health state indexes.
#' @examples 
#' toEQ5Dindex(c(1,2,3,4,5))
#' 
#' example_data <- as.data.frame(matrix(data = c(1, 2, 3, 4, 5, 
#'                                               5, 4, 3, 2, 1, 
#'                                               3, 2, 1, 2, 3), 
#'                                      ncol = 5, 
#'                                      byrow = TRUE, 
#'                                      dimnames = list(NULL, c("mo", "sc", "ua", "pd", "ad"))))
#' example_data$irrelevant <- c(6,5,3)
#' toEQ5Dindex(example_data)
#' @export
toEQ5Dindex <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  # Matrix or data.frame
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    # colnames(x) <- tolower(colnames(x))
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    if(!all(dim.names %in% colnames(x))) stop("Provided dimension names not available in matrix/data.frame.")
    return(as.integer(as.matrix(x[, dim.names]) %*% (10^(4:0))))
  }
  if(length(x)<5) stop("Too short vector provided. Stopping")
  if(is.null(names(x))) {
    message("No names provided in vector.")
    if(length(x) == 5) {
      message("Vector of 5 provided, assuming dimensions in conventional order: MO, SC, UA, PD, AD.")
      names(x) <- dim.names
    }
  } else {
    names(x) <- tolower(names(x))
  }
  if(!all(dim.names %in% names(x))) stop("Provided dimension names not in vector names. Stopping.")
  as.integer(x[dim.names] %*% (10^(4:0)))
}

#' @title toEQ5Ddims
#' @description Generate dimension vectors based on state index
#' @param x A vector of 5-digit EQ-5D state indexes.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A data.frame with 5 columns, one for each EQ-5D dimension, with names from dim.names argument.
#' @examples 
#' toEQ5Ddims(c(12345, 54321, 12321))
#' @export
toEQ5Ddims <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  # coerce to integer
  x <- as.integer(x)
  # Remove items outside of bounds
  x[!regexpr("^[1-5]{5}$", x)==1] <- NA
  if(sum(is.na(x))) warning(paste0("Provided vector contained ", sum(is.na(x)), " items not conforming to 5-digit numbers with exclusively digits in the 1-5 range."))
  as.data.frame(outer(X = x, Y = structure(.Data = 10^(4:0), .Names = dim.names), FUN = "%/%") %% 10)
}

#' @title make_all_EQ_states
#' @description Make a data.frame with all health states defined by dimensions
#' @param version Either "3L" or "5L", to signify whether 243 or 3125 states should be generated
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param append_index Boolean to indicate whether a column of 5-digit EQ-5D health state indexes should be added to output.
#' @return A data.frame with 5 columns and 243 (-3L) or 3125 (-5L) health states
#' @examples 
#' make_all_EQ_states('3L')
#' @export
make_all_EQ_states <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), append_index = FALSE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!is.vector(version)) stop("version argument is not a vector of length 1")
  if(length(version)>1) {
      message("version argument provided length of more than 1, first element is used.")
      version <- version[[1]]
    }
  if(!toupper(version) %in% c("5L", "3L")) stop('version argument should be either "3L" or "5L"')
  xout <- do.call(expand.grid, structure(rep(list(1:ifelse(version == "5L", 5, 3)), 5),.Names = dim.names[5:1]))[,5:1]
  if(append_index) xout$state <- toEQ5Dindex(xout, dim.names)
  xout
}

#' @title make_all_EQ_indexes
#' @description Make a vector containing all 5-digit EQ-5D indexes for -3L or -5L version.
#' @param version Either "3L" or "5L", to signify whether 243 or 3125 states should be generated
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A vector with 5-digit state indexes for all 243 (-3L) or 3125 (-5L) EQ-5D health states
#' @examples 
#' make_all_EQ_indexes('3L')
#' @export
make_all_EQ_indexes <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  toEQ5Dindex(do.call(make_all_EQ_states, as.list(match.call()[-1])))
}

#' @title EQ_dummies
#' @description Make a data.frame of all EQ-5D dummies relevant for e.g. regression modeling. 
#' @param df data.frame containing EQ-5D health states.
#' @param version Either "3L" or "5L", to signify EQ-5D instrument version
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param drop_level_1 If set to FALSE, dummies for level 1 will be included. Defaults to TRUE.
#' @param add_intercept If set to TRUE, a column containing 1s will be appended. Defaults to FALSE.
#' @param incremental If set to TRUE, incremental dummies will be produced (e.g. MO = 3 will give mo2 = 1, mo3 = 1). Defaults to FALSE.
#' @param append Optional string to be appended to column names.
#' @param prepend Optional string to be prepended to column names.
#' @param return_df If set to TRUE, data.frame is returned, otherwise matrix. Defaults to TRUE.
#' @return A data.frame of dummy variables 
#' @examples 
#' make_dummies(make_all_EQ_states('3L'), '3L')
#' 
#' make_dummies(df = make_all_EQ_states('3L'), 
#'              version =  '3L', 
#'              incremental = TRUE, 
#'              add_intercept = TRUE, 
#'              prepend = "d_")
#' @export
make_dummies <- function(df, version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), drop_level_1 = TRUE, add_intercept = FALSE, incremental = FALSE, prepend = NULL, append = NULL, return_df = TRUE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!length(dim(df) == 2)) stop("Need to provide matrix or data.frame of 2 dimensions.")

  colnames(df) <- tolower(colnames(df))
  if(is.null(colnames(df))) {
    message("No column names")
    if(NCOL(df) == 5) {
      message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
      colnames(df) <- dim.names
    } 
  }
  if(!all(dim.names %in% colnames(df))) stop("Provided dimension names not available in matrix/data.frame.")
  # intercept_col <- NULL
  # if(add_intercept) intercept_col <- data.frame(intercept = rep(1, NROW(df)))
  
  vers <- ifelse(version == '5L', 5, 3)
  
  startlevel <- ifelse(drop_level_1, 2, 1)
  if(incremental) {
    tmp <- 1*lower.tri(diag(vers), TRUE)[,startlevel:vers]  
  } else {
    tmp <- diag(vers)[,startlevel:vers]
  }
  
  tmpout <- do.call(cbind, lapply(df[,dim.names], function(x) tmp[x,      ]))
  colnames(tmpout) <- as.vector(t(outer(paste0(prepend, dim.names), paste0(startlevel:vers, append), FUN = paste0)))
  if(add_intercept) tmpout <- cbind(intercept = 1, tmpout)
  if(return_df) tmpout <- as.data.frame(tmpout)
  tmpout
}


#' @title eqvs_add
#' @description Add user-defined EQ-5D value set and corresponding crosswalk option.
#' @param df A data.frame or file name pointing to csv file. The contents of the data.frame or csv file should be exactly two columns: state, containing a list of all 3125 (for 5L) or 243 (for 3L) EQ-5D health state vectors, and a column of corresponding utility values, with a suitable name.
#' @param version Version of the EQ-5D instrument. Can take values 5L (default) or 3L.
#' @param country Optional string. If not NULL, will be used as a country description for the user-defined value set.
#' @param saveOption Integer indicating how the cache data should be saved. 1: Do not save (default), 2: Save in package folder, 3: Save in another path.
#' @param savePath A path where the cache data should be saved when `saveOption` is 3. Please use `eqvs_load` to load it in your next session.
#' @param description Optional string. If not NULL, will be used as a descriptive text for the user-defined value set. 
#' @param description Optional string. If not NULL, will be used as a descriptive text for the user-defined value set. 
#' @param code2L Optional string. If not NULL, will be used as the two-digit code for the value set. Must be different from any existing national value set code.
#' @param code3L Optional string. If not NULL, will be used as the three-digit code for the value set. Must be different from any existing national value set code.
#' @return True/False, indicating success or error.
#' @examples 
#' # make nonsense value set
#' new_df <- data.frame(state = make_all_EQ_indexes(), TEST = runif(3125))
#' # Add as value set for Fantasia
#' eqvs_add(new_df, version = "5L", country = 'Fantasia', saveOption = 1)
#' @importFrom utils read.csv
#' @export

eqvs_add <- function(df, version = "5L", country = NULL, saveOption = 1, savePath = NULL, description = NULL, code2L = NULL, code3L = NULL) {
  # Ensure saveOption is either 1, 2, or 3
  if (!saveOption %in% c(1, 2, 3)) {
    stop("Invalid 'saveOption'. It must be 1, 2, or 3.")
  }
  
  pkgenv <- getOption("eq.env")
  if(inherits(df, 'character')) {
    if(!file.exists(df)) stop('File named ', df, ' does not appear to exist. Exiting.')
    df <- utils::read.csv(file = df, stringsAsFactors = F)
  } 
  if(!NCOL(df) == 2) stop('df should have exactly two columns.')
  class(df[, 1]) <- 'integer'
  
  # read off version-dependent parameters
  j <- if (version == "5L") 5 else 3
  states_str <- paste0("states_", version)
  eq5d_str <- paste0("EQ-5D-", version)
  uservsets_str <- paste0("uservsets", version)
  user_defined_str <- paste0("user_defined_", version)
  
  # check correct number of rows
  n <- j^5
  if(!NROW(df) == n) stop(paste0("df should have exactly ", n, " rows."))
  # check all states present
  if(!all(df[,1] %in% pkgenv[[states_str]]$state)) stop(paste0("First column of df should contain all ", eq5d_str, " health state indexes exactly once."))
  df <- df[match(pkgenv[[states_str]]$state, df[,1]),]
  # check country
  if(!is.null(country)) {
    if(length(country)>1) {
      warning('Length of country argument > 1, first item used.')
      country <- country[1]
    }
  }
  if(!is.null(description)) {
    if(length(description)>1) {
      warning('Length of description argument > 1, first item used.')
      description <- description[1]
    }
  }
  thisName <- colnames(df)[2]
  if(thisName %in% colnames(pkgenv[[uservsets_str]])) {
    warning(paste0("New country name already in user-defined ", eq5d_str, " value set list."))
    return(0)
  }
  
  if(any(is.na(df[,2]*-1.1))) stop("Non-numeric values in second column of df.")
  
  # No problems
  tmp <- pkgenv[[uservsets_str]]
  tmp[, thisName] <- df[, 2]
  assign(x = uservsets_str, value = tmp, envir = pkgenv)
  
  tmp <- data.frame(Name = ifelse(is.null(description), NA, description), 
                    Name_short = ifelse(is.null(country), NA, country), 
                    ISO3166Alpha2 = colnames(df)[2], 
                    ISO3166Alpha3 = NA)
  
  if(user_defined_str %in% names(pkgenv)) tmp <- rbind(pkgenv[[user_defined_str]], tmp)
  
  assign(x = user_defined_str, value = tmp, envir = pkgenv)
  
  # Handle save options
  if (saveOption == 2) {
    path <- pkgenv$cache_path
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  } else if (saveOption == 3) {
    if (is.null(savePath) || !nzchar(savePath)) {
      stop("Option 3 requires a valid 'savePath'.")
    } else {
      if (!dir.exists(savePath)) {
        stop("The specified 'savePath' does not exist.")
      } else {
        path <- savePath
      }
    }
  }
  
  if(saveOption == 1){
    .fixPkgEnv(saveCache = FALSE)
  }
  if (saveOption == 2 || saveOption == 3) {
    filePath <- file.path(path, 'cache.Rdta.')
    .fixPkgEnv(saveCache = TRUE, filePath = filePath)
    message(paste0('Cache data saved to ', file.path(path, 'cache.Rdta.')))
  }

  return(TRUE)
}

#' @title eqvs_load
#' @description Load cache data from a specified path.
#' @param loadPath The path from which to load the cache data.
#' @return TRUE if loading is successful, FALSE otherwise.
#' @export
eqvs_load <- function(loadPath) {
  pkgenv <- getOption("eq.env")
  if (is.null(loadPath) || !nzchar(loadPath)) {
    stop("A valid 'loadPath' is required.")
  }
  cacheFile <- file.path(loadPath, 'cache.Rdta')
  if (!file.exists(cacheFile)) {
    stop("Cache file not found at specified 'loadPath'.")
  }
  load(cacheFile, envir = pkgenv)
  message(paste0('Cache data loaded from ', cacheFile, '.'))
  return(TRUE)
}


#' @title eqvs_drop
#' @description Drop user-defined EQ-5D value set to reverse crosswalk options.
#' @param version Version of the EQ-5D instrument. Can take values 5L (default) or 3L.
#' @param country Optional string. If NULL, a list of current user-defined value sets will be provided for selection. If set, and matching an existing user-defined value set, a prompt will be given as to whether the value set should be deleted. 
#' @param saveOption Integer indicating how the cache data should be saved. 1: Do not save (default), 2: Save in package folder, 3: Save in another path.
#' @param savePath A path where the cache data should be saved when `saveOption` is 3. Please use `eqvs_load` to load it in your next session.
#' @return True/False, indicating success or error.
#' @examples 
#' \donttest{
#'   # make nonsense value set
#'   new_df <- data.frame(state = make_all_EQ_indexes(), TEST = runif(3125))
#'   # Add as value set for Fantasia
#'   eqvs_add(new_df, version = "5L", country = 'Fantasia', saveOption = 1)
#'   # Drop value set for Fantasia
#'   eqvs_drop('Fantasia', saveOption = 1)
#' }
#' @export
eqvs_drop <- function(country = NULL, version = "5L", saveOption = 1, savePath = NULL) {
  
  # Ensure saveOption is either 1, 2, or 3
  if (!saveOption %in% c(1, 2, 3)) {
    stop("Invalid 'saveOption'. It must be 1, 2, or 3.")
  }
  
  pkgenv <- getOption("eq.env")
  if(length(country)>1) {
    message('Length of country argument larger than 1, only the first element will be used.')
    country <- country[[1]]
  }
  
  # read off version-dependent parameters
  version <- toupper(version)
  user_defined_str <- paste0("user_defined_", version)
  uservsets_str <- paste0("uservsets", version)
  
  states_str <- paste0("states_", version)
  eq5d_str <- paste0("EQ-5D-", version)
  
  if(!user_defined_str %in% names(pkgenv)) {
    message(paste0("No user-defined value sets exist among the ", version, " datasets. Exiting."))
    return(FALSE)
  } else {
    udc <- pkgenv[[user_defined_str]]
    if(!is.null(country)) {
      tmp <- which(toupper(as.matrix(udc)) == toupper(country), arr.ind = T)
      
      if(nrow(tmp)) {
        country <- udc$ISO3166Alpha2[tmp[1,1]]
        yesno = readline(prompt = paste0('Are you sure you want to delete user-defined country "', country,'" from the ', version, 'value sets? ([Y]es/[N]o) : '))
        if(tolower(yesno) %in% c("yes", "y")) {
          message('Removing ', country, ' from user-defined value sets.')
          udc <- udc[-tmp[1,1],]
          if(NROW(udc)) {
            assign(x = user_defined_str, value = udc, envir = pkgenv)
          } else {
            rm(list = user_defined_str, envir = pkgenv)
          }
          tmp <- pkgenv[[uservsets_str]]
          tmp[, which(colnames(tmp) == country)] <- NULL
          assign(x = uservsets_str, value = tmp, envir = pkgenv)
          # Handle save options
          if (saveOption == 2) {
            path <- pkgenv$cache_path
            if (!dir.exists(path)) {
              dir.create(path, recursive = TRUE)
            }
          } else if (saveOption == 3) {
            if (is.null(savePath) || !nzchar(savePath)) {
              stop("Option 3 requires a valid 'savePath'.")
            } else {
              if (!dir.exists(savePath)) {
                stop("The specified 'savePath' does not exist.")
              } else {
                path <- savePath
              }
            }
          }
          if(saveOption == 1){
            .fixPkgEnv(saveCache = FALSE)
          }
          if (saveOption == 2 || saveOption == 3) {
            filePath <- file.path(path, 'cache.Rdta.')
            .fixPkgEnv(saveCache = TRUE, filePath = filePath)
            message(paste0('Cache data saved to ', file.path(path, 'cache.Rdta.')))
          }
          return(TRUE)
        } else {
          message("OK. Exiting.")
          return(FALSE)
        }
      } else {
        message('User-defined value set identified as ', country, ' not found among the ', version, ' value sets.')
      }
    }
  }
  
  udc <- udc[, 3:1]
  colnames(udc) <- c('Code', 'Name', 'Description')
  message('Please enter which value set you wish to drop, or enter any other value to exit.')
  .prettyPrint(udc)
  yesno = trimws(toupper(readline(prompt = paste0('Please enter value set name, description, or code: '))))
  tmp <- which(toupper(as.matrix(udc)) == yesno, arr.ind = T)
  if(NROW(tmp)) return(eqvs_drop(tmp), version)
  message('Exiting.')
  return(FALSE)
}

#' @title eqvs_display
#' @description Display available value sets, which can also be used as (reverse) crosswalks.
#' @param return_df If set to TRUE, the function will return information on the names of the available value sets in a data.frame. Defaults to FALSE
#' @param version Version of the EQ-5D instrument. Can take values 5L (default) or 3L.
#' @return Default NULL, if return_df == TRUE, returns a data.frame with the displayed information.
#' @examples 
#' # Display available value sets.
#' eqvs_display
#' @export
eqvs_display <- function(version = "5L", return_df = FALSE) {
  pkgenv <- getOption("eq.env")
  
  # read off version name
  version <- toupper(version)
  user_defined_str <- paste0("user_defined_", version)
  
  message(str_c("Available national value sets for ", version, " version:"))
  .prettyPrint(pkgenv$country_codes[[version]], 'l')
  if(NROW(pkgenv[[user_defined_str]])) {
    message('User-defined value sets:')    
    .prettyPrint(pkgenv[[user_defined_str]], 'l')
  } else {
    message('No user-defined value sets available.')
  }
  if(return_df) 
    return(rbind(cbind(Type = 'Value set', pkgenv$country_codes[[version]]), if(NROW(pkgenv[[user_defined_str]])) cbind(Type = 'User-defined', pkgenv[[user_defined_str]]) else NULL))
  retval <- NULL
}


#' @title eq5d
#' @description Get EQ-5D index values for the -3L, -5L, crosswalk (-3L value set applied to -5L health states),  reverse crosswealk (-5L value set applied to -3L health states), and -Y-3L
#' @param x A vector of 5-digit EQ-5D-3L state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param version String indicating which version to use. Options are '5L'  (default), '3L', 'xw', 'xwr', and 'Y3L'.
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A vector of dimension names to identify dimension columns.
#' @return A vector of values or data.frame with one column for each value set requested.
#' @examples 
#' # US -3L value set
#' eq5d(c(11111, 12321, 32123, 33333), 'US', '3L') 
#' # Danish and US -5L value sets applied to -3L descriptives, i.e. reverse crosswalk
#' eq5d(make_all_EQ_states('3L'), c('DK', 'US'), 'XWR') 
#' # US -5L value set
#' eq5d(c(11111, 12321, 32153, 55555), 'US', '5L') 
#' @export
eq5d <- function(x, country = NULL, version = '5L', dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  pkgenv <- getOption("eq.env")
  version <- toupper(version)
  if(!version %in% c('3L', '5L', 'Y3L', 'XW', 'RXW', 'XWR', 'CW', 'CWR', 'RCW')) stop("No valid argument for eq-5d version.")
  version <- c('3L', '5L', 'Y3L', 'XW', 'XWR', 'XWR', 'XW', 'XWR', 'XWR')[match(version, c('3L', '5L', 'Y3L', 'XW', 'RXW', 'XWR', 'CW', 'CWR', 'RCW'))]
  
  vers <- c('3L', '5L', 'Y3L', '3L', '5L')[match(x = version, table = c('3L', '5L', 'Y3L', 'XW', 'XWR'))]
  
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    # colnames(x) <- tolower(colnames(x))
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    if(!all(dim.names %in% colnames(x))) stop("Provided dimension names not available in matrix/data.frame.")
    x <- toEQ5Dindex(x = x, dim.names = dim.names)
  }
  
  country <- .fixCountries(country, EQvariant = vers)
  if(any(is.na(country))) {
    isnas <- which(is.na(country))
    for(i in isnas)  warning('Country ', names(country)[i], ' not found. Dropped.')
    country <- country[!is.na(country)]
  }
  
  if(length(country)==0) {
    message('No valid countries listed. These value sets are currently available.')
    eqvs_display(version = vers)
    stop('No valid countries listed.')
  }

  
  if(length(country)>1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eq5d(x, count, version, dim.names))))
  }
  
  xorig <- x
  x <- as.integer(x)
  if(version %in% c('3L', 'Y3L', 'XWR')) {
    x[!regexpr("^[1-3]{5}$", x)==1] <- NA
  } else {
    x[!regexpr("^[1-5]{5}$", x)==1] <- NA
  } 
  
  vset <- switch(EXPR = version,
                 '3L' = pkgenv$vsets3L_combined,
                 '5L' = pkgenv$vsets5L_combined,
                 'XW' = pkgenv$xwsets,
                 'XWR' = pkgenv$xwrsets,
                 'Y3L' = pkgenv$vsetsY3L_combined)
  svec <- switch(EXPR = version,
                 '3L' = pkgenv$states_3L,
                 '5L' = pkgenv$states_5L,
                 'XW' = pkgenv$states_5L,
                 'XWR' = pkgenv$states_3L,
                 'Y3L' = pkgenv$states_3L)
  
  xout <- rep(NA, length(x))
  
  xout[!is.na(x)] <- vset[match(x[!is.na(x)], svec$state), country]
  names(xout) <- xorig
  xout
  
}

#' @title eq5d3l
#' @description Get EQ-5D-3L index values from individual responses to the five dimensions of the EQ-5D-3L. 
#' @param x A vector of 5-digit EQ-5D-3L state indexes or a matrix/data.frame with columns corresponding to EQ-5D-3L state dimensions.
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-3L dimensions.  Default is c("mo", "sc", "ua", "pd", "ad"). 
#' @return A vector of EQ-5D-3L values or data.frame with one column for each value set requested.
#' @examples 
#' eq5d3l(c(11111, 12321, 32123, 33333), 'US') # US -3L value set
#' eq5d3l(make_all_EQ_states('3L'), c('DK', 'CA')) # Danish and Canada -3L value sets 
#' @export
eq5d3l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  argl <- as.list(match.call(expand.dots = TRUE))[-1]
  argl[['version']] <- "3L"
  do.call(eq5d, argl)
}

#' @title eq5d5l
#' @description Get EQ-5D-5L index values from individual responses to the five dimensions of the EQ-5D-5L. 
#' @param x A vector of 5-digit EQ-5D-5L state indexes or a matrix/data.frame with columns corresponding to EQ-5D-5L state dimensions.
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-5L dimensions.  Default is c("mo", "sc", "ua", "pd", "ad"). 
#' @return A vector of EQ-5D-5L values or data.frame with one column for each value set requested.
#' @examples 
#' eq5d5l(c(11111, 12321, 32423, 55555), 'IT') # Italy -5L value set
#' eq5d5l(make_all_EQ_states('5L'), c('Japan', 'China')) # Japon and China -5L value sets 
#' @export
eq5d5l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  argl <- as.list(match.call(expand.dots = TRUE))[-1]
  argl[['version']] <- "5L"
  do.call(eq5d, argl)
}

#' @title eq5dy3l
#' @description Get EQ-5D-Y3L index values from individual responses to the five dimensions of the EQ-5D-Y3L. 
#' @param x A vector of 5-digit EQ-5D-Y3L state indexes or a matrix / data frame with columns for each dimension.
#' @param country String vector indicating country names or  ISO3166 Alpha 2 / 3 country codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-Y3L dimensions.  Default is c("mo", "sc", "ua", "pd", "ad"). 
#' @return A vector of EQ-5D-Y3L values or data.frame with one column for each value set requested.
#' @examples 
#' # Slovenia -Y3L value set
#' eq5dy3l(x = c(11111, 12321, 33333), country = 'SI') 
#' # Germany and Spain -Y3L value sets 
#' eq5dy3l(make_all_EQ_states('3L'), c('Germany', 'Spain')) 
#' @export
eq5dy3l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  argl <- as.list(match.call(expand.dots = TRUE))[-1]
  argl[['version']] <- "Y3L"
  do.call(eq5d, argl)
}
