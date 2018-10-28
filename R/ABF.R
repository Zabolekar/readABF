.column_name <- function (header, i) {
   channel_name <- trimws(header$channel_names[i])
   channel_unit <- trimws(header$channel_units[i])
   paste0(channel_name, " [", channel_unit, "]")
}

# this function can be applied to the return value of readABF
as.data.frame.ABF <- function (x, row.names = NULL, optional = FALSE, ...,
                               sweep = NULL, type = c("all", "one"),
                               channels, unit = NULL) {
   if (is.null(sweep)) {
      if (length(x$data) > 1) {
         stop("this ABF object contains more than one sweep, ",
              "please specify in 'sweep' which one should be used")
      } else {
         sweep <- 1
      }
   }

   if (sweep > length(x$data)) {
      stop("sweep number ", sweep, " was requested,",
           " but the ABF object only contains ", length(x$data), " sweeps")
   }

   type <- match.arg(type)

   m <- x$data[[sweep]]
   result <- data.frame(seq(0, by = x$samplingIntervalInSec, length.out = nrow(m)))
   # If we provide a name "Time [s]" on data frame creation,
   # it becomes "Time..s." for some reason.
   # So we assign it after the data frame is created:
   colnames(result) <- "Time [s]"

   if (type == "all") {
      for(i in seq_along(x$header$channel_names)) {
         name <- .column_name(x$header, i)
         result[[name]] <- m[,i]
      }
   } else { # type == "one"
      if (!methods::hasArg(channels)) {
         stop('argument "channels" is required if type == "one"')
      }
      if (length(channels) == 1) {
         name <- .column_name(x$header, channels)
         result[[name]] <- m[,channels]

      } else if (length(channels) == 2) {

         # the names current, voltage and conductance are due to historical
         # reasons, there were times when this part of function code was used
         # to calculate the conductance
         current <- channels[1]
         voltage <- channels[2]
         current_unit <- trimws(x$header$channel_units[current])
         voltage_unit <- trimws(x$header$channel_units[voltage])

         if (is.null(unit)) {
            unit <- paste0(current_unit, "/", voltage_unit)
            # e.g. "pA/mV" (which means nanoSiemens)
         }

         current_data <- m[,current]
         voltage_data <- m[,voltage]
         conductance_data <- current_data/voltage_data
         result[[paste0("Data [", unit, "]")]] <- conductance_data

      } else {
         stop('argument "channels" should be of length 1 or 2')
      }
   }

   result
}

plot.ABF <- function (x, pch = ".", ..., sweep = 1, type = c("all", "one"),
                      channels, unit = NULL) {
   df <- as.data.frame(x, sweep = sweep, type = type, channels = channels,
                       unit = unit)
   plot(df, pch = pch, ...)
}

print.ABF <- function (x, ...) {
   cat("Path: ", x$path, "\n")
   cat("Format version: ", x$format_version, "\n")
   cat("Sampling interval: ", x$samplingIntervalInSec, " s\n")
   cat("Channel names: ", x$header$channel_names, "\n")
   cat("Channel units: ", x$header$channel_units, "\n")
   cat("Number of sweeps: ", length(x$data), "\n")
   cat("Length of the first sweep: ", nrow(x$data[[1]]), "\n")
   invisible(x)
}
