##  Reverse crosswalk for R. Using EQ-5D-5L value sets for EQ-5D-3L data.
## 
##  This file is part of eqxwr.
##
##  eqxwr is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##
##  eqxwr is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with eqxwr If not, see <http://www.gnu.org/licenses/>.

.onLoad <- function(libname, pkgname) {
  # Check if 'eq.env' is already an option
  if('eq.env' %in% names(.Options)) {
    pkgenv <- getOption('eq.env')
  } else {
    options("eq.env" = (pkgenv <- new.env(parent=emptyenv())))
  }
  
  # Assign cache path. This is always recomputed for the machine we are
  # running on, and is deliberately never restored from the cache file.
  cache_path <- find_cache_dir('eq5dsuite')
  assign(x = "cache_path", value = cache_path, envir = pkgenv)

  # Restore the user's own value sets, if any. .apply_cache() validates and
  # where necessary migrates the cache, copies in only the user's objects, and
  # never writes to disk: the library folder may be read-only, and writing at
  # load time would be an unexpected side effect. A migrated cache is written
  # back the next time the user saves via eqvs_add()/eqvs_drop().
  .apply_cache(pkgenv, cache_path)

  .fixPkgEnv(saveCache = FALSE)
}

# Join the built-in and user-defined value sets for one instrument.
#
# Both arguments have a `state` column and one column per value set. The join
# must be on `state` alone: merge() otherwise joins on every shared column
# name, so a user-defined set carrying a built-in code would become part of the
# join key and the combined table would collapse to the rows where the two sets
# happen to agree -- in practice none, leaving every lookup for that instrument
# broken.
#
# Such a code is refused by eqvs_add() and dropped by .apply_cache(), each with
# its own message. Dropping it again here, silently, is the structural
# guarantee behind those two: whatever route a colliding code arrives by, the
# combined table stays whole. It is deliberately quiet, because a warning here
# would repeat on every eqvs_add() and eqvs_drop().
.combine_vsets <- function(builtin, user) {
  builtin_codes <- setdiff(colnames(builtin), "state")
  user_codes    <- setdiff(colnames(user), "state")
  clash <- user_codes[toupper(user_codes) %in% toupper(builtin_codes)]
  if (length(clash))
    user <- user[, !colnames(user) %in% clash, drop = FALSE]
  merge(builtin, user, by = "state")
}

.fixPkgEnv <- function(saveCache = FALSE, filePath = NULL) {
  pkgenv <- getOption('eq.env')
  
  # possible 3L/5L/Y3L states
  if(!'states_3L' %in% names(pkgenv)) assign(x = "states_3L", value = make_all_EQ_states(version = '3L', append_index = T), envir = pkgenv)
  if(!'states_5L' %in% names(pkgenv)) assign(x = "states_5L", value =  make_all_EQ_states(version = '5L', append_index = T), envir = pkgenv)
  
  # generate empty uservsets3L/5L
  if(!'uservsets3L' %in% names(pkgenv)) assign(x = "uservsets3L", value =  pkgenv$states_3L[,"state", drop = F], envir = pkgenv)
  if(!'uservsets5L' %in% names(pkgenv)) assign(x = "uservsets5L", value =  pkgenv$states_5L[,"state", drop = F], envir = pkgenv)
  if(!'uservsetsY3L' %in% names(pkgenv)) assign(x = "uservsetsY3L", value =  pkgenv$states_3L[,"state", drop = F], envir = pkgenv)
  
  # combined core and user-defined sets
  assign(x = "vsets3L_combined", value = .combine_vsets(.vsets3L, pkgenv$uservsets3L), envir = pkgenv)
  assign(x = "vsets5L_combined", value = .combine_vsets(.vsets5L, pkgenv$uservsets5L), envir = pkgenv)
  assign(x = "vsetsY3L_combined", value = .combine_vsets(.vsetsY3L, pkgenv$uservsetsY3L), envir = pkgenv)
  
  # variables used for reverse crosswalk
  if(!'PPP' %in% names(pkgenv)) assign(x = "PPP", value = .EQxwrprob(par = .EQrxwmod7), envir = pkgenv)
  if(!'probs' %in% names(pkgenv)) assign(x = "probs", value = .pstate3t5(pkgenv$PPP), envir = pkgenv)
  if('probs' %in% names(pkgenv)) assign(x = 'xwrsets', value = pkgenv$probs %*% cbind(as.matrix(.vsets5L[, -1, drop = F]), if("uservsets5L" %in% names(pkgenv)) as.matrix(pkgenv$uservsets5L[,-1, drop = F]) else NULL), envir = pkgenv)
  
  # variables used for crosswalk
  if (!'probs5t3' %in% names(pkgenv)) assign(x = "probs5t3", value = .pstate5t3(.EQxwprob), envir = pkgenv)
  if('probs5t3' %in% names(pkgenv)) assign(x = 'xwsets', value = pkgenv$probs5t3 %*% cbind(as.matrix(.vsets3L[, -1, drop = F]), if("uservsets3L" %in% names(pkgenv)) as.matrix(pkgenv$uservsets3L[,-1, drop = F]) else NULL), envir = pkgenv)
  
  # variables used for NICE crosswalk
  if (!'crosswalk_NICE' %in% names(pkgenv)) {
    assign(x = "crosswalk_NICE", value = .crosswalk_NICE, envir = pkgenv)
  }  
  
  EQvariants <- c('5L' = '5L', '3L' = '3L', "Y3L" = "Y3L")
  assign(x = 'country_codes', envir = pkgenv, value = lapply(EQvariants, function(EQvariant) {
    tmp <- .cntrcodes[.cntrcodes$Version == EQvariant & 
                      .cntrcodes$VS_code %in% colnames(get(paste0('.vsets', EQvariant)))[-1],]
    rownames(tmp) <- NULL
    tmp
  }))
  
  if(saveCache){
    # Only the user's own value sets are cached, together with a schema stamp.
    # Built-in data is always rebuilt from the installed package above, so a
    # stale cache can no longer shadow it. See R/cache_schema.R.
    return(invisible(.save_cache(pkgenv, filePath)))
  }

  invisible(TRUE)
}
