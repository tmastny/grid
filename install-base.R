tools_ns <- asNamespace("tools")
unlockBinding(".check_package_description", tools_ns)

tools_ns$.check_package_description <- function(dfile, strict = FALSE)
{
  dfile <- file_path_as_absolute(dfile)
  db <- .read_description(dfile)

  standard_package_names <- .get_standard_package_names()

  valid_package_name_regexp <-
    .standard_regexps()$valid_package_name
  valid_package_version_regexp <-
    .standard_regexps()$valid_package_version

  is_base_package <- FALSE
  # !is.na(priority <- db["Priority"]) && priority == "base"

  out <- list()                       # For the time being ...

  ## Check encoding-related things first.

  ## All field tags must be ASCII.
  if(any(ind <- !.is_ASCII(names(db))))
    out$fields_with_non_ASCII_tags <- names(db)[ind]
  ## For all fields used by the R package management system, values
  ## must be ASCII as well (so that the RPM works in a C locale).
  ASCII_fields <- c(.get_standard_repository_db_fields(),
                    "Encoding", "License")
  ASCII_fields <- intersect(ASCII_fields, names(db))
  if(any(ind <- !.is_ASCII(db[ASCII_fields])))
    out$fields_with_non_ASCII_values <- ASCII_fields[ind]

  ## Determine encoding and re-encode if necessary and possible.
  if("Encoding" %in% names(db)) {
    encoding <- db["Encoding"]
    if(Sys.getlocale("LC_CTYPE") %notin% c("C", "POSIX"))
      db <- iconv(db, encoding, sub = "byte")
  }
  else if(!all(.is_ISO_8859(db))) {
    ## No valid Encoding metadata.
    ## Determine whether we can assume Latin1.
    out$missing_encoding <- TRUE
  }

  if(anyNA(nchar(db, "c", TRUE))) {
    ## Ouch, invalid in the current locale.
    ## (Can only happen in a MBCS locale.)
    ## Try re-encoding from Latin1.
    db <- iconv(db, "latin1")
  }

  ## Check Authors@R and expansion if needed.
  if(!is.na(aar <- db["Authors@R"]) &&
     (is.na(db["Author"]) || is.na(db["Maintainer"]))) {
    res <- .check_package_description_authors_at_R_field(aar)
    if(is.na(db["Author"]) &&
       !is.null(s <- attr(res, "Author")))
      db["Author"] <- s
    if(is.na(db["Maintainer"]) &&
       !is.null(s <- attr(res, "Maintainer")))
      db["Maintainer"] <- s
    mostattributes(res) <- NULL     # Keep names.
    out <- c(out, res)
  }

  val <- package_name <- db["Package"]
  if(!is.na(val)) {
    tmp <- character()
    ## We allow 'R', which is not a valid package name.
    if(!grepl(sprintf("^(R|%s)$", valid_package_name_regexp), val))
      tmp <- c(tmp, gettext("Malformed package name"))
    # if(!is_base_package) {
    #   if(val %in% standard_package_names$base)
    #     tmp <- c(tmp,
    #              c("Invalid package name.",
    #                "This is the name of a base package."))
    #   else if(val %in% standard_package_names$stubs)
    #     tmp <- c(tmp,
    #              c("Invalid package name.",
    #                "This name was used for a base package and is remapped by library()."))
    # }
    if(length(tmp))
      out$bad_package <- tmp
  }
  if(!is.na(val <- db["Version"])
     && !is_base_package
     && !grepl(sprintf("^%s$", valid_package_version_regexp), val))
    out$bad_version <- val
  if(!is.na(val <- db["Maintainer"])
     && !grepl(.valid_maintainer_field_regexp, val))
    out$bad_maintainer <- val

  ## Optional entries in DESCRIPTION:
  ##   Depends/Suggests/Imports/Enhances, Namespace, Priority.
  ## These must be correct if present.

  val <- db[match(c("Depends", "Suggests", "Imports", "Enhances"),
                  names(db), nomatch = 0L)]
  if(length(val)) {
    depends <- trimws(unlist(strsplit(val, ",")))
    bad_dep_entry <- bad_dep_op <- bad_dep_version <- character()
    dep_regexp <-
      paste0("^[[:space:]]*",
             paste0("(R|", valid_package_name_regexp, ")"),
             "([[:space:]]*\\(([^) ]+)[[:space:]]+([^) ]+)\\))?",
             "[[:space:]]*$")
    for(dep in depends) {
      if(!grepl(dep_regexp, dep)) {
        ## Entry does not match the regexp.
        bad_dep_entry <- c(bad_dep_entry, dep)
        next
      }
      if(nzchar(sub(dep_regexp, "\\2", dep))) {
        ## If not just a valid package name ...
        if(sub(dep_regexp, "\\3", dep) %notin%
           c("<=", ">=", "<", ">", "==", "!="))
          bad_dep_op <- c(bad_dep_op, dep)
        else if(grepl("^[[:space:]]*R", dep)) {
          if(!grepl(sprintf("^(r[0-9]+|%s)$",
                            valid_package_version_regexp),
                    sub(dep_regexp, "\\4", dep)))
            bad_dep_version <- c(bad_dep_version, dep)
        } else if(!grepl(sprintf("^%s$",
                                 valid_package_version_regexp),
                         sub(dep_regexp, "\\4", dep)))
          bad_dep_version <- c(bad_dep_version, dep)
      }
    }
    if(length(c(bad_dep_entry, bad_dep_op, bad_dep_version)))
      out$bad_depends_or_suggests_or_imports <-
      list(bad_dep_entry = bad_dep_entry,
           bad_dep_op = bad_dep_op,
           bad_dep_version = bad_dep_version)
  }
  if(strict && !is.na(val <- db["VignetteBuilder"])) {
    depends <- trimws(unlist(strsplit(val, ",")))
    if(length(depends) < 1L || !all(grepl("^[[:alnum:].]*$", depends)))
      out$bad_vignettebuilder <- TRUE
  }
  if(!is.na(val <- db["Priority"])
     && !is.na(package_name)
     && (tolower(val) %in% c("base", "recommended", "defunct-base"))
     && (package_name %notin% unlist(standard_package_names)))
    out$bad_priority <- val

  ## Minimal check (so far) of Title and Description.
  if(strict && !is.na(val <- db["Title"])
     && grepl("[.]$", val)
     && !grepl("[[:space:]][.][.][.]|et[[:space:]]al[.]", trimws(val)))
    out$bad_Title <- TRUE
  ## some people put punctuation inside quotes, some outside.
  if(strict && !is.na(val <- db["Description"])
     && !grepl("[.!?]['\")]?$", trimws(val)))
    out$bad_Description <- TRUE

  class(out) <- "check_package_description"
  out
}

environment(tools_ns$.check_package_description) <- tools_ns
