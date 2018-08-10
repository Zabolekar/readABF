# this function can be applied to the return value of readABF
as.data.frame.ABF <- function (x, row.names = NULL, optional = FALSE, ..., sweep = NULL, type = c("all", "conductance"),
                               current, voltage, unit = NULL) {
  if (is.null(sweep)) {
    if (length(x$data) > 1) {
      stop("...") # TODO: old error message
    } else {
      sweep <- 1
    }
  }
  
  if (sweep > length(x$data)) {
    stop("This file contains less sweeps than variable 'sweep'") # TODO: better error message
  }
  
  type <- match.arg(type)
  
  si <- x$header$si # sampling interval (aka dt) in us
  si <- si * 1e-6 # converting to s
  
  m <- x$data[[sweep]]
  result <- data.frame(seq(0, by = si, length.out = nrow(m)))
  colnames(result)[1] <- "Time [s]" # allows correct handling of []
  for(i in seq_along(x$header$channel_names)) {
    channel_name <- trimws(x$header$channel_names[i])
    channel_unit <- trimws(x$header$channel_units[i])
    full_name <- paste0(channel_name, " [", channel_unit, "]")
    result[[full_name]] <- m[,i]
  }

  if (type == "conductance") {
    if (!grepl("A", current_unit)) {
      warning("Channel ", current, " has unit ", current_unit, " and might not contain current")
    }
    if (!grepl("V", voltage_unit)) {
      warning("Channel ", voltage, " has unit ", voltage_unit, " and might not contain voltage")
    }
    
    if (is.null(unit)) {
      current_unit <- x$header$channel_units[current]
      voltage_unit <- x$header$channel_units[voltage]

      unit <- paste0(current_unit, "/", voltage_unit)
      # e.g. "pA/mV" (which means nanoSiemens)
    }
    
    current <- result[,current+1]
    voltage <- result[,voltage+1]
    
    result <- data.frame("Time [s]" = result[,1])
    result[[paste0("Conductance [", unit, "]")]] = current/voltage
  }

  result
}

plot.ABF <- function (x, pch = ".", ..., sweep = 1, type = c("all", "conductance"), current, voltage, unit = NULL) {
  df <- as.data.frame(x, sweep = sweep, type = type, current = current, voltage = voltage, unit = unit)
  
  plot(df, pch = pch, ...)
}

print.ABF <- function (x, ...) {
  cat("Path: ", x$path, "\n")
  cat("Format version: ", x$format_version, "\n")
  cat("Channel names: ", x$header$channel_names, "\n")
  cat("Channel units: ", x$header$channel_units, "\n")
  cat("Number of sweeps: ", length(x$data), "\n")
  cat("Length of the first sweep: ", nrow(x$data[[1]]), "\n")
  invisible(x)
}
