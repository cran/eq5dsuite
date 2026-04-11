#' Convert EQ-5D dimension scores to a five-digit profile index
#' @param x A data.frame, matrix, or named numeric vector of EQ-5D dimension
#'   scores. Each dimension must contain integer values (typically 1–3 or 1–5).
#' @param dim.names Character vector of length 5 giving the dimension names, in
#'   the conventional MO, SC, UA, PD, AD order.
#' @param na.rm Logical. If \code{FALSE} (default), any \code{NA} in a row
#'   produces \code{NA} in the output. If \code{TRUE}, \code{NA} dimensions are
#'   treated as 0 — use with care, as this silently changes the index value.
#' @param quiet Logical. Suppress informational messages about assumed column /
#'   name order. Default \code{FALSE} so existing scripts see the same messages;
#'   set \code{TRUE} inside pipelines.
#' @return An integer vector with one element per row (data.frame/matrix input)
#'   or a single integer (vector input).  Works inside \code{dplyr::mutate()}
#'   without \code{rowwise()}.
#' @examples
#' # Named vector — scalar usage unchanged
#' toEQ5Dindex(c(mo=1, sc=2, ua=3, pd=1, ad=2))
#' @export

toEQ5Dindex <- function(
    x,
    dim.names = c("mo", "sc", "ua", "pd", "ad"),
    na.rm     = FALSE,
    quiet     = FALSE
) {
  
  # ── 1. Validate dim.names ─────────────────────────────────────────────────
  if (length(dim.names) != 5L)
    stop("'dim.names' must be a character vector of exactly 5 names.")
  if (anyDuplicated(dim.names))
    stop("'dim.names' contains duplicate entries: ",
         paste(dim.names[duplicated(dim.names)], collapse = ", "), ".")
  
  msg <- function(...) if (!quiet) message(...)
  
  # ── 2. Weights: 10^4, 10^3, 10^2, 10^1, 10^0 ────────────────────────────
  weights <- 10L ^ (4L:0L)
  
  # ── 3. Matrix / data.frame branch (vectorised) ───────────────────────────
  if (length(dim(x)) == 2L) {
    
    # Normalise column names to lower-case
    if (!is.null(colnames(x))) colnames(x) <- tolower(colnames(x))
    
    # Assign default names if none present
    if (is.null(colnames(x))) {
      if (ncol(x) == 5L) {
        msg("No column names found; assuming conventional order: ",
            paste(toupper(dim.names), collapse = ", "), ".")
        colnames(x) <- dim.names
      } else {
        stop("'x' has no column names and ", ncol(x),
             " columns (expected 5). ",
             "Provide a named data.frame/matrix or set 'dim.names'.")
      }
    }
    
    missing_cols <- setdiff(dim.names, colnames(x))
    if (length(missing_cols))
      stop("Required dimension column(s) not found in 'x': ",
           paste(missing_cols, collapse = ", "), ".")
    
    m <- as.matrix(x[, dim.names, drop = FALSE])
    mode(m) <- "integer"
    
    # NA handling
    if (!na.rm && anyNA(m)) {
      # Propagate NA row-wise without changing non-NA rows
      row_has_na <- rowSums(is.na(m)) > 0L
      result <- integer(nrow(m))
      result[!row_has_na] <- as.integer(
        m[!row_has_na, , drop = FALSE] %*% weights
      )
      result[row_has_na] <- NA_integer_
      return(result)
    }
    
    if (na.rm) m[is.na(m)] <- 0L
    return(as.integer(m %*% weights))
  }
  
  # ── 4. Named-vector branch (scalar, backward-compatible) ─────────────────
  if (length(x) < 5L)
    stop("'x' has ", length(x), " element(s); at least 5 required.")
  
  if (is.null(names(x))) {
    if (length(x) == 5L) {
      msg("No names found in vector; assuming conventional order: ",
          paste(toupper(dim.names), collapse = ", "), ".")
      names(x) <- dim.names
    } else {
      stop("'x' has no names and length != 5. ",
           "Provide a named vector or a data.frame/matrix.")
    }
  } else {
    names(x) <- tolower(names(x))
  }
  
  missing_names <- setdiff(dim.names, names(x))
  if (length(missing_names))
    stop("Required dimension name(s) not found in 'x': ",
         paste(missing_names, collapse = ", "), ".")
  
  vals <- x[dim.names]
  
  if (!na.rm && anyNA(vals))
    return(NA_integer_)
  
  if (na.rm) vals[is.na(vals)] <- 0L
  as.integer(vals %*% weights)
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
  states <- do.call(make_all_EQ_states, list(version = version, dim.names = dim.names))
  toEQ5Dindex(states, dim.names = dim.names)
  #toEQ5Dindex(do.call(make_all_EQ_states, as.list(match.call()[-1])))
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
#' @param countryCode Optional string. If not NULL, will be used as the two-digit code for the value set. Must be different from any existing national value set code.
#' @param VSCode Optional string. If not NULL, will be used as the three-digit code for the value set. Must be different from any existing national value set code.
#' @param description Optional string. If not NULL, will be used as a descriptive text for the user-defined value set. 
#' @param saveOption Integer indicating how the cache data should be saved. 1: Do not save (default), 2: Save in package folder, 3: Save in another path.
#' @param savePath A path where the cache data should be saved when `saveOption` is 3. Please use `eqvs_load` to load it in your next session.
#' @return True/False, indicating success or error.
#' @examples 
#' # make nonsense value set
#' new_df <- data.frame(state = make_all_EQ_indexes(), TEST = runif(3125))
#' # Add as value set for Fantasia
#' eqvs_add(
#'    new_df,
#'    version = "5L",
#'    country = 'Fantasia',
#'    countryCode = "MyCountry",
#'    VSCode = "FAN",
#'    saveOption = 1
#' )
#' eq5d5l(55555,country = "FAN")
#' @importFrom utils read.csv
#' @export

eqvs_add <- function(df, version = "5L", country = NULL, countryCode = NULL, VSCode = NULL, description = NULL, saveOption = 1, savePath = NULL) {
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
  thisName <- ifelse(is.null(VSCode), colnames(df)[2], VSCode)
  if(thisName %in% colnames(pkgenv[[uservsets_str]])) {
    warning(paste0("New country name already in user-defined ", eq5d_str, " value set list."))
    return(0)
  }
  
  if(any(is.na(df[,2]*-1.1))) stop("Non-numeric values in second column of df.")
  
  # No problems
  tmp <- pkgenv[[uservsets_str]]
  tmp[, thisName] <- df[, 2]
  assign(x = uservsets_str, value = tmp, envir = pkgenv)
  
  tmp <- data.frame(Version = version,
                    Name = ifelse(is.null(country), NA, country), 
                    Name_short = ifelse(is.null(country), NA, country), 
                    Country_code = ifelse(is.null(countryCode), colnames(df)[2], countryCode), 
                    VS_code = ifelse(is.null(VSCode), colnames(df)[2], VSCode),
                    doi = ifelse(is.null(description), NA, description))
  
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

  message(paste("The value set", country, "was added."))
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
#'   eqvs_add(
#'    new_df,
#'    version = "5L",
#'    country = 'Fantasia',
#'    countryCode = "MyCountry",
#'    VSCode = "FAN",
#'    saveOption = 1
#'   )
#'   # Test the new value set
#'   eq5d5l(55555,country = "FAN")
#'   # Drop value set for Fantasia
#'   eqvs_drop(country = 'FAN', saveOption = 1)
#' }
#' @export
eqvs_drop <- function(country = NULL, version = "5L", saveOption = 1, savePath = NULL) {
  
  # Ensure saveOption is valid
  if (!saveOption %in% c(1, 2, 3)) {
    stop("Invalid 'saveOption'. It must be 1, 2, or 3.")
  }
  
  pkgenv <- getOption("eq.env")
  version <- toupper(version)
  
  user_defined_str <- paste0("user_defined_", version)
  uservsets_str <- paste0("uservsets", version)
  
  if (!user_defined_str %in% names(pkgenv)) {
    message(paste0("No user-defined value sets exist for ", version, ". Exiting."))
    return(FALSE)
  }
  
  udc <- pkgenv[[user_defined_str]]
  
  # Ensure `udc` exists and is not empty
  if (is.null(udc) || nrow(udc) == 0) {
    message("No user-defined value sets found. Exiting.")
    return(FALSE)
  }
  
  # Handle case where multiple value sets exist for the same country
  matched_rows <- which(toupper(udc$Country_code) == toupper(country) | toupper(udc$VS_code) == toupper(country))
  
  if (length(matched_rows) == 0) {
    message("No matching user-defined value set found for country: ", country, ". Exiting.")
    return(FALSE)
  }
  
  if (length(matched_rows) > 1) {
    message(paste0("There are ", length(matched_rows), " value sets available for country code '", country, "'."))
    options_table <- udc[matched_rows, c("Version", "Name", "Country_code", "VS_code", "doi")]
    print(options_table)
    
    # Ask user which value set to delete
    repeat {
      user_input <- readline(prompt = "Please enter the VS_code you want to delete: ")
      if (user_input %in% udc$VS_code[matched_rows]) {
        country <- user_input
        break
      } else {
        message("Invalid VS_code. Please enter one from the table above.")
      }
    }
  } else {
    country <- udc$VS_code[matched_rows]  # If only one match, proceed
  }
  
  # Ask for confirmation
  yesno <- readline(prompt = paste0('Are you sure you want to delete value set "', country, '" for ', version, '? ([Y]es/[N]o) : '))
  if (tolower(yesno) %in% c("yes", "y")) {
    
    message('Removing ', country, ' from user-defined value sets.')
    
    # Remove from user-defined dataset
    udc <- udc[udc$VS_code != country, ]
    if (nrow(udc) > 0) {
      assign(x = user_defined_str, value = udc, envir = pkgenv)
    } else {
      rm(list = user_defined_str, envir = pkgenv)
    }
    
    # Remove from value set matrix
    tmp <- pkgenv[[uservsets_str]]
    if (country %in% colnames(tmp)) {
      tmp <- tmp[, !colnames(tmp) %in% country, drop = FALSE]
      assign(x = uservsets_str, value = tmp, envir = pkgenv)
    }
    
    # Handle save options
    if (saveOption == 2) {
      path <- pkgenv$cache_path
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
    } else if (saveOption == 3) {
      if (is.null(savePath) || !nzchar(savePath)) {
        stop("Option 3 requires a valid 'savePath'.")
      } else if (!dir.exists(savePath)) {
        stop("The specified 'savePath' does not exist.")
      } else {
        path <- savePath
      }
    }
    
    if (saveOption == 1) {
      .fixPkgEnv(saveCache = FALSE)
    }
    
    if (saveOption == 2 || saveOption == 3) {
      filePath <- file.path(path, 'cache.Rdta.')
      .fixPkgEnv(saveCache = TRUE, filePath = filePath)
      message(paste0('Cache data saved to ', filePath))
    }
    message(paste("The value set", country, "was deleted."))
  } else {
    message("OK. Exiting without deletion.")
  }
}


#' @title eqvs_display
#' @description Display available value sets, which can also be used as (reverse) crosswalks.
#' @param return_df If set to TRUE, the function will return information on the names of the available value sets in a data.frame. Defaults to FALSE
#' @param version Version of the EQ-5D instrument. Can take values 5L (default), 3L or 3LY.
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
  
  message(paste0("Available national value sets for ", version, " version:"))
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

  valid_versions <- c('3L', '5L', 'Y3L', 'XW', 'RXW', 'XWR', 'CW', 'CWR', 'RCW')
  if (!version %in% valid_versions) stop("No valid argument for EQ-5D version.")
  
  # Normalize version mapping
  version <- c('3L', '5L', 'Y3L', 'XW', 'XWR', 'XWR', 'XW', 'XWR', 'XWR')[match(version, valid_versions)]
  vers <- c('3L', '5L', 'Y3L', '3L', '5L')[match(version, c('3L', '5L', 'Y3L', 'XW', 'XWR'))]
  
  if (length(dim.names) != 5) stop("Argument dim.names must be of length 5.")
  
  if (is.matrix(x) || is.data.frame(x)) {
    if (is.null(colnames(x))) {
      message("No column names detected.")
      if (NCOL(x) == 5) {
        message("Assuming dimensions in order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      }
    }
    if (!all(dim.names %in% colnames(x))) stop("Provided dimension names not found in input matrix/data.frame.")
    x <- toEQ5Dindex(x = x, dim.names = dim.names)
  }
  
  country <- .fixCountries(country, EQvariant = vers)
  
  # Handle cases where the country input is invalid
  if (any(is.na(country))) {
    invalid_countries <- names(country)[is.na(country)]
    warning("The following countries were not found and will be ignored: ", paste(invalid_countries, collapse = ", "))
    country <- country[!is.na(country)]
  }
  
  if (length(country) == 0) {
    message("No valid countries listed. Available value sets are:")
    eqvs_display(version = vers)
    stop("No valid countries listed.")
  }
  
  # If multiple countries, apply the function iteratively
  if (length(country) > 1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eq5d(x, count, version, dim.names))))
  }
  
  # Validate and match states
  xorig <- x
  x <- as.integer(x)
  pattern <- if (version %in% c('3L', 'Y3L', 'XWR')) "^[1-3]{5}$" else "^[1-5]{5}$"
  x[!grepl(pattern, x)] <- NA
  
  # Select the appropriate value set and state mapping
  vset <- switch(version,
                 '3L' = pkgenv$vsets3L_combined,
                 '5L' = pkgenv$vsets5L_combined,
                 'XW' = pkgenv$xwsets,
                 'XWR' = pkgenv$xwrsets,
                 'Y3L' = pkgenv$vsetsY3L_combined)
  svec <- switch(version,
                 '3L' = pkgenv$states_3L,
                 '5L' = pkgenv$states_5L,
                 'XW' = pkgenv$states_5L,
                 'XWR' = pkgenv$states_3L,
                 'Y3L' = pkgenv$states_3L)
  
  # Compute EQ-5D index values
  xout <- rep(NA, length(x))
  xout[!is.na(x)] <- vset[match(x[!is.na(x)], svec$state), country]
  names(xout) <- xorig
  xout
}

#' @title eq5d3l
#' @description
#' Get EQ-5D-3L index values from individual responses to the five
#' dimensions of the EQ-5D-3L.
#' @param x A vector of 5-digit EQ-5D-3L state indexes, or a matrix/data.frame
#'   with columns corresponding to the EQ-5D-3L dimensions.
#' @param country String vector indicating country names or ISO3166 Alpha 2 / 3
#'   country codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-3L
#'   dimensions. Default is `c("mo", "sc", "ua", "pd", "ad")`.
#' @return A numeric vector of EQ-5D-3L values, or a data.frame with one column
#'   for each requested value set.
#' @examples
#' # Example 1: utility values from EQ-5D-3L profile codes
#' eq5d3l(c(11111, 12321, 32123, 33333), country = "US")
#'
#' # Example 2: request multiple value sets
#' eq5d3l(make_all_EQ_states("3L"), country = c("DK", "CA"))
#'
#' # Example 3: use a data.frame with dimension columns
#' df3l <- data.frame(
#'   mo = c(1, 2, 3),
#'   sc = c(1, 2, 2),
#'   ua = c(1, 3, 1),
#'   pd = c(2, 2, 3),
#'   ad = c(1, 1, 2)
#' )
#' eq5d3l(df3l, country = "US")
#'
#' # Example 4: use custom dimension column names
#' df3l_named <- data.frame(
#'   mobility = c(1, 2, 3),
#'   self_care = c(1, 2, 2),
#'   usual_activities = c(1, 3, 1),
#'   pain_discomfort = c(2, 2, 3),
#'   anxiety_depression = c(1, 1, 2)
#' )
#' eq5d3l(
#'   df3l_named,
#'   country = "US",
#'   dim.names = c(
#'     "mobility", "self_care", "usual_activities",
#'     "pain_discomfort", "anxiety_depression"
#'   )
#' )
#' @export
eq5d3l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  eq5d(x = x, country = country, version = "3L", dim.names = dim.names)
}

#' @title eq5d5l
#' @description
#' Get EQ-5D-5L index values from individual responses to the five
#' dimensions of the EQ-5D-5L.
#'
#' @param x A vector of 5-digit EQ-5D-5L state indexes, or a matrix/data.frame
#'   with columns corresponding to the EQ-5D-5L dimensions.
#' @param country String vector indicating country names or ISO3166 Alpha 2 / 3
#'   country codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-5L
#'   dimensions. Default is `c("mo", "sc", "ua", "pd", "ad")`.
#'
#' @return A numeric vector of EQ-5D-5L values, or a data.frame with one column
#'   for each requested value set.
#'
#' @examples
#' # Example 1: utility values from EQ-5D-5L profile codes
#' eq5d5l(c(11111, 12321, 32423, 55555), country = "IT")
#'
#' # Example 2: request multiple value sets
#' eq5d5l(make_all_EQ_states("5L"), country = c("ES", "DE"))
#'
#' # Example 3: use a data.frame with dimension columns
#' df5l <- data.frame(
#'   mo = c(1, 2, 5),
#'   sc = c(1, 2, 4),
#'   ua = c(1, 3, 3),
#'   pd = c(2, 4, 2),
#'   ad = c(1, 5, 1)
#' )
#' eq5d5l(df5l, country = "ES")
#'
#' # Example 4: use custom dimension column names from a real-world style dataset
#' df5l_named <- data.frame(
#'   mobility = c(1, 5, 3),
#'   self_care = c(2, 4, 2),
#'   usual_activities = c(3, 3, 1),
#'   pain_discomfort = c(4, 2, 2),
#'   anxiety_depression = c(5, 1, 3)
#' )
#' eq5d5l(
#'   df5l_named,
#'   country = "ES",
#'   dim.names = c(
#'     "mobility", "self_care", "usual_activities",
#'     "pain_discomfort", "anxiety_depression"
#'   )
#' )
#'
#' @export
eq5d5l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  eq5d(x = x, country = country, version = "5L", dim.names = dim.names)
}

#' @title eq5dy3l
#' @description
#' Get EQ-5D-Y-3L index values from individual responses to the five
#' dimensions of the EQ-5D-Y-3L.
#' @param x A vector of 5-digit EQ-5D-Y-3L state indexes, or a matrix/data.frame
#'   with columns corresponding to the EQ-5D-Y-3L dimensions.
#' @param country String vector indicating country names or ISO3166 Alpha 2 / 3
#'   country codes.
#' @param dim.names A character vector specifying the names of the EQ-5D-Y-3L
#'   dimensions. Default is `c("mo", "sc", "ua", "pd", "ad")`.
#' @return A numeric vector of EQ-5D-Y-3L values, or a data.frame with one
#'   column for each requested value set.
#' @examples
#' # Example 1: utility values from EQ-5D-Y-3L profile codes
#' eq5dy3l(x = c(11111, 12321, 33333), country = "SI")
#' 
#' # Example 2: request multiple value sets
#' eq5dy3l(make_all_EQ_states("3L"), country = c("ES", "DE"))
#'
#' # Example 3: use a data.frame with dimension columns
#' dfy3l <- data.frame(
#'   mo = c(1, 2, 3),
#'   sc = c(1, 1, 2),
#'   ua = c(1, 2, 3),
#'   pd = c(2, 2, 3),
#'   ad = c(1, 3, 2)
#' )
#' eq5dy3l(dfy3l, country = "SI")
#'
#' # Example 4: use custom dimension column names
#' dfy3l_named <- data.frame(
#'   mobility = c(1, 2, 3),
#'   self_care = c(1, 1, 2),
#'   usual_activities = c(1, 2, 3),
#'   pain_discomfort = c(2, 2, 3),
#'   anxiety_depression = c(1, 3, 2)
#' )
#' eq5dy3l(
#'   dfy3l_named,
#'   country = "SI",
#'   dim.names = c(
#'     "mobility", "self_care", "usual_activities",
#'     "pain_discomfort", "anxiety_depression"
#'   )
#' )
#' @export
eq5dy3l <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")){
  eq5d(x = x, country = country, version = "Y3L", dim.names = dim.names)
}
