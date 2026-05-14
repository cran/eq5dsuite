# Wrapper for file.copy that throws if any files fail to copy. Use name
# file.copy2 to make clear that it's a wrapper for a base function, and not for
# fs::file_copy.
file.copy2 <- function(from, to, ...) {
  res <- file.copy(from, to, ...)
  if (!all(res)) {
    stop("Error copying files: ", paste0(from[!res], collapse = ", "))
  }
}

dir.create2 <- function(path, ...) {
  res <- dir.create(path, ...)
  if (!res) {
    stop("Error creating directory: ", path)
  }
}

# .fixCountries <- function(countries, EQvariant = '5L') {
#   pkgenv <- getOption("eq.env")
#   cntrs <- rbind(pkgenv$country_codes[[EQvariant]], pkgenv[[paste0("user_defined_", EQvariant)]])
#   
#   sapply(countries, function(country) {
#     tmp <- which(toupper(as.matrix(cntrs)) == toupper(country), arr.ind = T)
#     if(nrow(tmp))  return(cntrs$ISO3166Alpha2[tmp[1,1]])
#     NA
#   })
# }

.fixCountries <- function(countries, EQvariant = '5L') {
  pkgenv <- getOption("eq.env")
  cc <- pkgenv$country_codes[[EQvariant]]
  ud <- pkgenv[[paste0("user_defined_", EQvariant)]]
  if (!is.null(ud) && nrow(ud) > 0) {
    common_cols <- intersect(colnames(cc), colnames(ud))
    cntrs <- rbind(cc[, common_cols, drop = FALSE],
                   ud[, common_cols, drop = FALSE])
  } else {
    cntrs <- cc
  }
  
  result <- sapply(countries, function(country) {
    matched_rows <- which(toupper(cntrs$Country_code) == toupper(country) | 
                            toupper(cntrs$VS_code) == toupper(country), arr.ind = TRUE)
    
    if (length(matched_rows) == 0) {
      return(NA)
    } else if (length(matched_rows) == 1) {
      return(cntrs$VS_code[matched_rows])
    } else {
      # If multiple value sets exist for the country, ask the user to specify
      message(paste0("There are ", length(matched_rows), " value sets available for country code '", country, "'."))
      options_table <- cntrs[matched_rows, c("Version", "Name", "Country_code", "VS_code", "doi")]
      print(options_table)
      
      # Ask user to input the VS_code
      repeat {
        user_input <- readline(prompt = "Please enter the VS_code you would like to use: ")
        if (user_input %in% cntrs$VS_code[matched_rows]) {
          return(user_input)
        } else {
          message("Invalid VS_code. Please enter one from the table above.")
        }
      }
    }
  }, USE.NAMES = TRUE)
  
  return(result)
}


find_cache_dir <- function(pkg) {
  # In R 4.0 and above, CRAN wants us to use the new tools::R_user_dir().
  # If not present, fall back to rappdirs::user_cache_dir().
  R_user_dir <- getNamespace('tools')$R_user_dir
  if (!is.null(R_user_dir)) {
    R_user_dir(pkg, which = "cache")
  } else {
    rappdirs::user_cache_dir(pkg, "R")
  }
}

.prettyPrint <- function(df, justify = 'r') {
  cnames <- colnames(df)
  if(length(justify)<length(cnames)) justify <- rep(justify, length(cnames))
  n      <- as.matrix(nchar(cnames))
  
  d <- as.matrix(as.data.frame(apply(df, 2, format, simplify = F)))
  d[which(trimws(d) == "NA", arr.ind = T)] <- ""
  n <- apply(cbind(n, nchar(d[1,])), 1, max)
  
  fmts <- vapply(1:length(justify), FUN.VALUE = 'aha', function(i) paste0("%",ifelse(tolower(justify[[i]]) == 'l','-', ''), n[[i]], "s"))
  for(i in 1:length(cnames)) {
    cnames[i] <- sprintf(fmts[i], cnames[i])
    d[,i] <- sprintf(fmts[i], trimws(d[,i]))
  }
  d <- rbind(cnames, d)
  
  for(i in 1:NROW(d)) cat(d[i,], '\r\n')
}