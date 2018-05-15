# comment copied from the matlab file, TODO: adapt, maybe copy more, there was more at the top of the file
# -- si                   sampling interval
# -- dataPtsPerChan       sample points per channel
# -- dataPts              sample points in file
# -- recTime              recording start and stop time in seconds from
#                         midnight (millisecond resolution)
# -- sweepLengthInPts     sample points per sweep (one channel)
# -- sweepStartInPts      the start times of sweeps in sample points
#                         (from beginning of recording)

# TODO: copy some comments from matlab to here

# defaults:
# gap-free:
whereStart <- 0.0
whereStop <- "e"
# episodic:
sweeps <- "a" # "a" means all
# general:
channels <- "a" # TODO: means "all". As stated in the comments, this flag is currently always the same, so maybe get rid of it alltogether
# the size of data chunks (see above) in Mb. 0.05 Mb is an empirical value # TODO: Mb or MB?
# which works well for abf with 6-16 channels and recording durations of 
# 5-30 min
chunk <- 0.05
machineF <- "ieee-le"
verbose <- FALSE
doLoadData <- TRUE
# TODO: info mode (where doLoadData is false) is currently not implemented, maybe never will
BLOCKSIZE <- 512

section_names <- c("ProtocolSection", "ADCSection", "DACSection", "EpochSection", "ADCPerDACSection", "EpochPerDACSection",
                   "UserListSection", "StatsRegionSection", "MathSection", "StringsSection", "DataSection", "TagSection",
                   "ScopeSection", "DeltaSection", "VoiceTagSection", "SynchArraySection", "AnnotationSection", "StatsSection")

readCharDontTruncate <- function (con, nchars) {
   # readChar is not guaranteed to read all nchars characters as it truncates at the first null, we don't want that
   # sadly, our result still can't contain null (it is not allowed in R strings)
   # so we split it into multiple strings
   chars <- readChar(con, rep(1, nchars), useBytes=TRUE)

   one_small_string <- ""
   small_strings <- c()
   for (char in chars) {
      if (char == "") { # "" is R euphemism for null character
         if (one_small_string != "") {
            small_strings <- c(small_strings, one_small_string)
            one_small_string <- ""
         }
      } else {
         one_small_string <- paste0(one_small_string, char)
      }
   }

   small_strings
}

readABF <- function (filename) {

   fileSz <- file.size(filename)

   f <- file(filename, open="rb")
   header <- list()
   # some black black magic
   # TODO: what is this -1 in matlab headers?
   skip <- function (n) invisible(readBin(f, "raw", n=n))
   bool <- function (n=1) as.logical(readBin(f, n=n, "integer", size=1, endian="little"))
   int8 <- function (n=1) readBin(f, n=n, "integer", size=1, endian="little")
   int16 <- function (n=1) readBin(f, n=n, "integer", size=2, endian="little")
   int32 <- function (n=1) readBin(f, n=n, "integer", size=4, endian="little")
   int64 <- function (n=1) readBin(f, n=n, "integer", size=8, endian="little")
   uint32 <- function (n=1) {
      # you would think: readBin(f, n=n, "integer", size=4, signed=FALSE, endian="little")
      # but R can't do that
      # instead we do what the author of abf2 package did:
      # we combine two unsigned integers
      result <- c() # TODO: I never actually want it like this, n is never 0, is it?
      if (n >= 1) {
         for (i in 1:n) {
            lo <- readBin(f, "integer", size=2, signed=FALSE, endian="little")
            hi <- readBin(f, "integer", size=2, signed=FALSE, endian="little")
            result <- c(result, 65536.0*hi+lo)
         }
      }
      return(result)
   }
   float32 <- function (n=1) readBin(f, n=n, "double", size=4, endian="little")
   char <- function (n=1) readChar(f, rep(1, n), useBytes=TRUE)
   chararr <- function (n=1) readChar(f, rep(n, 16), useBytes=TRUE)
   `%:%` <- function (a, b) header[[as.character(substitute(a))]] <<- b()
   `%x%` <- function (f, n) function () f(n)

   header$fFileSignature <- readChar(f, nchars=4, useBytes=TRUE)

   `ABF ` <- function () {
      fFileVersionNumber %:% float32
      nOperationMode %:% int16
      lActualAcqLength %:% int32
      nNumPointsIgnored %:% int16
      lActualEpisodes %:% int32
      skip(4)
      lFileStartTime %:% int32
      skip(12)
      lDataSectionPtr %:% int32
      lTagSectionPtr %:% int32
      lNumTagEntries %:% int32
      skip(40)
      lSynchArrayPtr %:% int32
      lSynchArraySize %:% int32
      nDataFormat %:% int16
      skip(18)
      nADCNumChannels %:% int16
      fADCSampleInterval %:% float32
      skip(4)
      fSynchTimeUnit %:% float32
      skip(4)
      lNumSamplesPerEpisode %:% int32
      lPreTriggerSamples %:% int32
      lEpisodesPerRun %:% int32
      skip(94)
      fADCRange %:% float32
      skip(4)
      lADCResolution %:% int32
      skip(110)
      nFileStartMillisecs %:% int16
      skip(10)
      nADCPtoLChannelMap %:% (int16 %x% 16)
      nADCSamplingSeq %:% (int16 %x% 16)
      sADCChannelName %:% (chararr %x% 10)
      sADCUnits %:% (chararr %x% 8)
      fADCProgrammableGain %:% (float32 %x% 16)
      skip(128)
      fInstrumentScaleFactor %:% (float32 %x% 16)
      fInstrumentOffset %:% (float32 %x% 16)
      fSignalGain %:% (float32 %x% 16)
      fSignalOffset %:% (float32 %x% 16)
      skip(3334)
      nTelegraphEnable %:% (int16 %x% 16)
      skip(32)
      fTelegraphAdditGain %:% (float32 %x% 16)
   }

   `ABF2` <- function () {
      uFileVersionNumber %:% (int8 %x% 4) # name is misleading, u is uint32, but we read four int8's for practical reasons
      # also it is called fFileVersionNumber in abfload.m for consistency with older versions
      uFileInfoSize %:% uint32
      lActualEpisodes %:% uint32 # TODO: lActualEpisodes instead of uActualEpisodes for uint32 is confusing, but currently necessary for our code to work. rewrite
      uFileStartDate %:% uint32
      uFileStartTimeMS %:% uint32
      uStopwatchTime %:% uint32
      nFileType %:% int16
      nDataFormat %:% int16
      nSimultaneousScan %:% int16
      nCRCEnable %:% int16
      uFileCRC %:% uint32
      FileGUID %:% (int32 %x% 4) # abfload.m says it is uint32, but it's not, a GUID should have 16 bytes
      # we don't use it (so we could actually just skip(16)), but its size should be right so the alignment is right
      uCreatorVersion %:% uint32
      uCreatorNameIndex %:% uint32
      uModifierVersion %:% uint32
      uModifierNameIndex %:% uint32
      uProtocolPathIndex %:% uint32
   }

   if (header$fFileSignature == "ABF ") { # note the blank
      `ABF `()
      header$fFileVersionNumber <- round(header$fFileVersionNumber, 2)
      header$lFileStartTime <- header$lFileStartTime + header$nFileStartMillisecs*0.001
   } else if (header$fFileSignature == "ABF2") {
      `ABF2`()
      header$fFileVersionNumber <- sum(header$uFileVersionNumber * (1e-4)*10**(1:4)) # for example, c(5,4,3,2) becomes 2.345
      header$lFileStartTime <- header$uFileStartTimeMS*0.001 # convert ms to s
   } else {
      stop("unknown or incompatible file signature")
      # for example, the signature could be "2FBA" on Mac. We don't implement it because we don't have a Mac to test it
   }
      
   sections <- list()
   
   # in the Matlab file they are called recChNames and recChUnits
   header$channel_names <- c()
   header$channel_units <- c()
   
   # TODO: check whether it does the same thing as in Matlab
   ADC_info <- function (offset) {
      seek(f, offset)
      list(
         nADCNum=int16(),
         nTelegraphEnable=int16(),
         nTelegraphInstrument=int16(),
         fTelegraphAdditGain=float32(),
         fTelegraphFilter=float32(),
         fTelegraphMembraneCap=float32(),
         nTelegraphMode=int16(),
         fTelegraphAccessResistance=float32(),
         nADCPtoLChannelMap=int16(),
         nADCSamplingSeq=int16(),
         fADCProgrammableGain=float32(),
         fADCDisplayAmplification=float32(),
         fADCDisplayOffset=float32(),
         fInstrumentScaleFactor=float32(),
         fInstrumentOffset=float32(),
         fSignalGain=float32(),
         fSignalOffset=float32(),
         fSignalLowpassFilter=float32(),
         fSignalHighpassFilter=float32(),
         nLowpassFilterType=int8(),
         nHighpassFilterType=int8(),
         fPostProcessLowpassFilter=float32(),
         nPostProcessLowpassFilterType=int8(),
         bEnabledDuringPN=bool(),
         nStatsChannelPolarity=int16(),
         lADCChannelNameIndex=int32(),
         lADCUnitsIndex=int32()
      )
   }
   
   Tag_info <- function (offset) {
      seek(f, offset)
      list(
         lTagTime=int32(),
         sComment=(char %x% 56)(),
         nTagType=int16(),
         nVoiceTagNumber_or_AnnotationIndex=int16()
      )
   }
   
   if (header$fFileVersionNumber >= 2) {
      
      seek(f, 76) # TODO: why 76? matlab code says so
      for (name in section_names) {
         sections[[name]] <- list(uBlockIndex=uint32(), uBytes=uint32(), llNumEntries=int64())
      }
      seek(f, sections$StringsSection$uBlockIndex*BLOCKSIZE)
      strings <- readCharDontTruncate(f, sections$StringsSection$uBytes)
      
      keywords <- c("clampex","clampfit","axoscope","patchxpress")
      matches <-  sapply(keywords, function (s) sapply(strings, function (r) grepl(s, r, ignore.case=TRUE)))
      # it results in a boolean matrix with keywords as columns and strings as rows
      if (sum(matches) != 1) {
         warning("problems in StringsSection")
         # TODO: actually it can be worse than a warning
      }
      
      for (i in seq_along(strings)) {
         if (rowSums(matches)[i] > 0) {
            strings <- strings[i:length(strings)]
            break
         }
      }
      
      # TODO if needed keyword <- colnames(matches)[colSums(matches) > 0][1]
      
      ADCsec <- list()
      
      for (i in 1:sections$ADCSection$llNumEntries) {
         ADCsec[[i]] <- ADC_info(sections$ADCSection$uBlockIndex*BLOCKSIZE+sections$ADCSection$uBytes*(i-1))
         ii <- ADCsec[[i]]$nADCNum+1
         header$nADCSamplingSeq[i] <- ADCsec[[i]]$nADCNum
         
         header$channel_names <- c(header$channel_names, strings[ADCsec[[i]]$lADCChannelNameIndex]) # TODO: matlab uses strvcat here, it ignores empty strings, it might be intentional, check if something goes wrong
         unitsIndex <- ADCsec[[i]]$lADCUnitsIndex
         if (unitsIndex > 0) {
           header$channel_units <- c(header$channel_units, strings[unitsIndex]) # strvcat too
         }
         # TODO: the following probably can be written better
         # TODO: also, variables that aren't really part of the "physical" header don't belong in the header object
         header$nTelegraphEnable[ii] <- ADCsec[[i]]$nTelegraphEnable
         header$fTelegraphAdditGain[ii] <- ADCsec[[i]]$fTelegraphAdditGain
         header$fInstrumentScaleFactor[ii] <- ADCsec[[i]]$fInstrumentScaleFactor
         header$fSignalGain[ii] <- ADCsec[[i]]$fSignalGain
         header$fADCProgrammableGain[ii] <- ADCsec[[i]]$fADCProgrammableGain
         header$fInstrumentOffset[ii] <- ADCsec[[i]]$fInstrumentOffset
         header$fSignalOffset[ii] <- ADCsec[[i]]$fSignalOffset
      }
      
      seek(f, sections$ProtocolSection$uBlockIndex*BLOCKSIZE)
      ProtocolSec <- list(
         nOperationMode=int16(),
         fADCSequenceInterval=float32(),
         bEnableFileCompression=bool(),
         sUnused1 = (int8 %x% 3)(),
         uFileCompressionRatio=uint32(),
         fSynchTimeUnit=float32(),
         fSecondsPerRun=float32(),
         lNumSamplesPerEpisode=int32(),
         lPreTriggerSamples=int32(),
         lEpisodesPerRun=int32(),
         lRunsPerTrial=int32(),
         lNumberOfTrials=int32(),
         nAveragingMode=int16(),
         nUndoRunCount=int16(),
         nFirstEpisodeInRun=int16(),
         fTriggerThreshold=float32(),
         nTriggerSource=int16(),
         nTriggerAction=int16(),
         nTriggerPolarity=int16(),
         fScopeOutputInterval=float32(),
         fEpisodeStartToStart=float32(),
         fRunStartToStart=float32(),
         lAverageCount=int32(),
         fTrialStartToStart=float32(),
         nAutoTriggerStrategy=int16(),
         fFirstRunDelayS=float32(),
         nChannelStatsStrategy=int16(),
         lSamplesPerTrace=int32(),
         lStartDisplayNum=int32(),
         lFinishDisplayNum=int32(),
         nShowPNRawData=int16(),
         fStatisticsPeriod=float32(),
         lStatisticsMeasurements=int32(),
         nStatisticsSaveStrategy=int16(),
         fADCRange=float32(),
         fDACRange=float32(),
         lADCResolution=int32(),
         lDACResolution=int32(),
         nExperimentType=int16(),
         nManualInfoStrategy=int16(),
         nCommentsEnable=int16(),
         lFileCommentIndex=int32(),
         nAutoAnalyseEnable=int16(),
         nSignalType=int16(),
         nDigitalEnable=int16(),
         nActiveDACChannel=int16(),
         nDigitalHolding=int16(),
         nDigitalInterEpisode=int16(),
         nDigitalDACChannel=int16(),
         nDigitalTrainActiveLogic=int16(),
         nStatsEnable=int16(),
         nStatisticsClearStrategy=int16(),
         nLevelHysteresis=int16(),
         lTimeHysteresis=int32(),
         nAllowExternalTags=int16(),
         nAverageAlgorithm=int16(),
         fAverageWeighting=float32(),
         nUndoPromptStrategy=int16(),
         nTrialTriggerSource=int16(),
         nStatisticsDisplayStrategy=int16(),
         nExternalTagType=int16(),
         nScopeTriggerOut=int16(),
         nLTPType=int16(),
         nAlternateDACOutputState=int16(),
         nAlternateDigitalOutputState=int16(),
         fCellID = (float32 %x% 3)(),
         nDigitizerADCs=int16(),
         nDigitizerDACs=int16(),
         nDigitizerTotalDigitalOuts=int16(),
         nDigitizerSynchDigitalOuts=int16(),
         nDigitizerType=int16()
      )
      header$nOperationMode <- ProtocolSec$nOperationMode
      header$fSynchTimeUnit <- ProtocolSec$fSynchTimeUnit

      header$nADCNumChannels <- sections$ADCSection$llNumEntries
      header$lActualAcqLength <- sections$DataSection$llNumEntries
      header$lDataSectionPtr <- sections$DataSection$uBlockIndex
      header$nNumPointsIgnored <- 0
      
      header$fADCSampleInterval <- ProtocolSec$fADCSequenceInterval/header$nADCNumChannels
      header$fADCRange <- ProtocolSec$fADCRange
      header$lADCResolution <- ProtocolSec$lADCResolution
      
      header$lSynchArrayPtr <- sections$SynchArraySection$uBlockIndex
      header$lSynchArraySize <- sections$SynchArraySection$llNumEntries
   } else {
      sections$TagSection$llNumEntries <- header$lNumTagEntries
      sections$TagSection$uBlockIndex <- header$lTagSectionPtr
      sections$TagSection$uBytes <- 64
   }

   if (header$lActualAcqLength < header$nADCNumChannels) {
      close(f)
      stop("less data points than sampled channels in file")
   }

   # the numerical value of all recorded channels (numbers 0..15)
   recChIdx <- header$nADCSamplingSeq[1:header$nADCNumChannels]
   # the corresponding indices into loaded data d
   recChInd <- 1:length(recChIdx)
   
   if (header$fFileVersionNumber < 2) {
     # the channel names, e.g. "IN 8" (for ABF version 2.0 these have been
     # extracted above at this point)
     header$channel_names <- header$sADCChannelName[recChIdx+1]
     # same with signal units
     header$channel_units <- header$sADCUnits[recChIdx+1]
   }
   
   # check whether requested channels exist
   chInd <- c()
   eflag <- FALSE
   if (class(channels) == "character") {
      if (channels == "a") {
       chInd <- recChInd
      } else {
         close(f)
         stop("input parameter 'channels' must either be a cell array holding channel names or the single character 'a' (=all channels)")
      }
   }
   
   # gain of telegraphed instruments, if any
   if (header$fFileVersionNumber >=1.65) {
     addGain <- header$nTelegraphEnable * header$fTelegraphAdditGain
     addGain[addGain==0] <- 1
   } else {
     addGain <- rep(1, length(header$fTelegraphAdditGain))
   }
   
   # determine offset at which data start
   if (header$nDataFormat == 0) {
      dataSz <- 2 # bytes/point
      precision <- "int16"
   } else if (header$nDataFormat == 1) {
      dataSz <- 4 # bytes/point
      precision <- "float32"
   } else {
      close(f)
      stop("invalid number format")
   }

   headOffset <- header$lDataSectionPtr*BLOCKSIZE + header$nNumPointsIgnored*dataSz
   # header$fADCSampleInterval is the TOTAL sampling interval
   header$si <- header$fADCSampleInterval * header$nADCNumChannels
   # assign same value to si, which is an output variable
   si <- header$si
   if (sweeps == "a") {
     nSweeps <- header$lActualEpisodes
     sweeps <- 1:header$lActualEpisodes
   } else {
     nSweeps <- length(sweeps)
   }
   
   
   # determine time unit in synch array section
   if (header$fSynchTimeUnit == 0) {
       # time information in synch array section is in terms of ticks
       header$synchArrTimeBase <- 1
   } else {
      # time information in synch array section is in terms of usec
      header$synchArrTimeBase <- header$fSynchTimeUnit
   }

   # read in the TagSection, do a few computations & write to header$tags
   header$tags <- list()   
   if (sections$TagSection$llNumEntries > 0) {
      tmp <- list()
      for (i in 1:sections$TagSection$llNumEntries) {
         tmp[[i]] <- Tag_info(sections$TagSection$uBlockIndex*BLOCKSIZE+sections$TagSection$uBytes*(i-1))
         # time of tag entry from start of experiment in s (corresponding expisode
         # number, if applicable, will be determined later)
         header$tags[[i]] <- list(
            timeSinceRecStart=tmp$lTagTime*header$synchArrTimeBase/1e6, # TODO: how is this supposed to work?
            comment=tmp$sComment
         )
      }
   }
   
# -------------------------------------------------------------------------
#    PART 3: read data (note: from here on code is generic and abf version
#    should not matter)
# -------------------------------------------------------------------------
   
   if (header$nOperationMode == 1) {
      if (verbose) print("data were acquired in event-driven variable-length mode")
      if (header$fFileVersionNumber >=2.0) {
         stop("ABFReader currently does not work with data acquired in event-driven variable-length mode and ABF version 2.0")
      } else {
         if (header$lSynchArrayPtr <= 0 || header$lSynchArraySize <= 0) {
            close(f)
            stop("internal variables 'lSynchArray*' are zero or negative")
         }
         # the byte offset at which the SynchArraySection starts
         header$lSynchArrayPtrByte <- BLOCKSIZE*header$lSynchArrayPtr
         # before reading Synch Arr parameters check if file is big enough to hold them
         # 4 bytes/long, 2 values per episode (start and length)
         if (header$lSynchArrayPtrByte + 2*4*header$lSynchArraySize < fileSz) {
            close(f)
            stop("file seems not to contain complete Synch Array Section")
         }
         tryCatch(seek(f, header$lSynchArrayPtrByte), error = function (e) { # TODO: pretty sure it's a matlab only problem, not applicable to R
            close(f)
            stop("something went wrong positioning file pointer to Synch Array Section", call.=FALSE)
         })
         synchArr <- int32(header$lSynchArraySize*2)
         if (length(synchArr) != header$lSynchArraySize*2) {
            close(f)
            stop("something went wrong reading synch array section")
         }
         # make synchArr a header$lSynchArraySize x 2 matrix
         synchArr <- t(matrix(synchArr, nrow=2))
         # the length of episodes in sample points
         segLengthInPts <- synchArr[,2]/header$synchArrTimeBase
         # the starting ticks of episodes in sample points WITHIN THE DATA FILE
         segStartInPts <- cumsum(c(0, segLengthInPts[1:length(segLengthInPts)-1])*dataSz) + headOffset
         # start time (synchArr[,1]) has to be divided by header$nADCNumChannels to get true value
         # go to data portion
         tryCatch(seek(f, headOffset), error = function (e) { # TODO: pretty sure it's a matlab only problem, not applicable to R
            close(f)
            stop("something went wrong positioning file pointer (too few data points ?)")
         })
         # load data if requested
         if (doLoadData) { # TODO this variable
            for (i in 1:nSweeps) { # TODO what if nSweeps is 0, can it be 0?
               # if selected sweeps are to be read, seek correct position
               if (nSweeps != header$lActualEpisodes) {
                  seek(f, segStartInPts[sweeps[i]])
               }
               tmpd <- list(int16=int16, float32=float32)[[precision]](segLengthInPts[sweeps[i]])
               n <- length(tmpd)
               if (n != segLengthInPts[sweeps[i]]) {
                  warning("something went wrong reading episode ", sweeps[i], ": ", segLengthInPts[sweeps[i]], " points should have been read, ", n, " points actually read")
               }
               header$dataPtsPerChan <- n/header$nADCNumChannels
               if (n %% header$nADCNumChannels > 0) {
                  close(f)
                  stop("number of data points in episode not OK")
               }
               # separate channels
               tmpd <- matrix(tmpd, header$nADCNumChannels, header$dataPtsPerChan)
               # retain only requested channels
               tmpd <- tmpd[chInd,]
               tmpd <- t(tmpd)
               # if data format is integer, scale appropriately; if it's float, tmpd is fine
               if (!header$nDataFormat) {
                  for (j in 1:length(chInd)) {
                     ch <- recChIdx[chInd[j]]+1
                     tmpd[,j] <- tmpd[,j]/(header$fInstrumentScaleFactor[ch]*header$fSignalGain[ch]*header$fADCProgrammableGain[ch]*addGain[ch])*header$fADCRange/header$lADCResolution+header$fInstrumentOffset[ch]-header$fSignalOffset[ch]
                  }
               }
               # tmpd consists of one sweep with channels in columns
               d[[i]] <- tmpd # TODO: we didn't define d before, do we?
            }
         }
      }
   } else if (header$nOperationMode %in% c(2, 4, 5)) {
      if (header$nOperationMode == 2) {
         if (verbose) print("data were acquired in event-driven fixed-length mode")
      } else if (header$nOperationMode == 4) {
         if (verbose) print("data were acquired in high-speed oscilloscope mode")
      } else {
         if (verbose) print("data were acquired in waveform fixed-length mode")
      }
      # extract timing information on sweeps
      if (header$lSynchArrayPtr <= 0 || header$lSynchArraySize <= 0) {
         close(f)
         stop("internal variables 'lSynchArraynnn' are zero or negative")
      }
      # the byte offset at which the SynchArraySection starts
      header$lSynchArrayPtrByte <- BLOCKSIZE*header$lSynchArrayPtr
      # before reading Synch Arr parameters check if file is big enough to hold them
      # 4 bytes/long, 2 values per episode (start and length)
      if (header$lSynchArrayPtrByte+2*4*header$lSynchArraySize > fileSz) {
         close(f)
         stop("file seems not to contain complete Synch Array Section")
      }
      tryCatch(seek(f, header$lSynchArrayPtrByte), error = function (e) { # TODO: pretty sure it's a matlab only problem, not applicable to R
         close(f)
         stop("something went wrong positioning file pointer to Synch Array Section")
      })
      synchArr <- int32(header$lSynchArraySize*2)
      if (length(synchArr) != header$lSynchArraySize*2) {
         close(f)
         stop("something went wrong reading synch array section")
      }
      # make synchArr a header$lSynchArraySize x 2 matrix
      synchArr <- t(matrix(synchArr, nrow=2))
      if (length(unique(synchArr[,2])) > 1) {
        close(f)
        stop("sweeps of unequal length in file recorded in fixed-length mode")
      }
      # the length of sweeps in sample points (**note: parameter lLength of
      # the ABF synch section is expressed in samples (ticks) whereas
      # parameter lStart is given in synchArrTimeBase units)
      header$sweepLengthInPts <- synchArr[1,2]/header$nADCNumChannels
      # the starting ticks of episodes in sample points (t0=1=beginning of recording)
      header$sweepStartInPts <- synchArr[,1]*(header$synchArrTimeBase/header$fADCSampleInterval/header$nADCNumChannels)
      # recording start and stop times in seconds from midnight
      header$recTime <- header$lFileStartTime
      tmpvar <- header$sweepStartInPts[length(header$sweepStartInPts)]
      header$recTime <- header$recTime + c(0, (1e-6*(tmpvar+header$sweepLengthInPts))*header$fADCSampleInterval*header$nADCNumChannels)
      # determine first point and number of points to be read
      startPt <- 0
      header$dataPts <- header$lActualAcqLength
      header$dataPtsPerChan <- header$dataPts/header$nADCNumChannels
      if (header$dataPts %% header$nADCNumChannels > 0 || header$dataPtsPerChan %% header$lActualEpisodes > 0) {
        close(f)
        stop("number of data points not OK")
      }
      # temporary helper var
      dataPtsPerSweep <- header$sweepLengthInPts*header$nADCNumChannels
      tryCatch(seek(f, startPt*dataSz+headOffset), error = function (e) { # TODO: pretty sure it's a matlab only problem, not applicable to R
        close(f)
        stop("something went wrong positioning file pointer (too few data points ?)")
      })
      d <- array(0, c(header$sweepLengthInPts, length(chInd), nSweeps))
      # the starting ticks of episodes in sample points WITHIN THE DATA FILE
      selectedSegStartInPts <- ((sweeps-1)*dataPtsPerSweep)*dataSz + headOffset
      # ** load data if requested
      if (doLoadData) {
         for (i in 1:nSweeps) {
            seek(f, selectedSegStartInPts[i])
            tmpd <- list(int16=int16, float32=float32)[[precision]](dataPtsPerSweep)
            n <- length(tmpd)
            if (n != dataPtsPerSweep) {
               close(f)
               stop("something went wrong reading episode ", sweeps[i], ": ", dataPtsPerSweep, " points should have been read, ", n, " points actually read")
            }
            header$dataPtsPerChan <- n/header$nADCNumChannels
            if (n %% header$nADCNumChannels > 0) {
               close(f)
               stop("number of data points in episode not OK")
            }
            # separate channels..
            tmpd <- matrix(tmpd, header$nADCNumChannels, header$dataPtsPerChan)
            # retain only requested channels
            tmpd <- tmpd[chInd,]
            tmpd <- t(tmpd)
            # if data format is integer, scale appropriately; if it's float, d is fine
            if (!header$nDataFormat) {
               for (j in 1:length(chInd)) {
                  ch <- recChIdx[chInd[j]] + 1
                  tmpd[,j] <- tmpd[,j]/(header$fInstrumentScaleFactor[ch]*header$fSignalGain[ch]*header$fADCProgrammableGain[ch]*addGain[ch])*header$fADCRange/header$lADCResolution+header$fInstrumentOffset[ch]-header$fSignalOffset[ch]
               }
            }
            # now fill 3d array, TODO: think about global type of d
            d[,,i] <- tmpd
         }
      }
   } else if (header$nOperationMode == 3) {
      if (verbose) print("data were acquired in gap-free mode")
      # from whereStart, whereStop, headOffset and header$fADCSampleInterval calculate first point to be read
      #  and - unless whereStop is given as "e" - number of points
      startPt <- floor(1e6*whereStart*(1/header$fADCSampleInterval))
      # this corrects undesired shifts in the reading frame due to rounding errors in the previous calculation
      startPt <- floor(startPt/header$nADCNumChannels)*header$nADCNumChannels
      # if whereStop is a char array, it can only be "e" at this point (other values would have been caught above)
      if (class(whereStop) == "character") {
         header$dataPtsPerChan <- header$lActualAcqLength/header$nADCNumChannels - floor(1e6*whereStart/header$si)
         header$dataPts <- header$dataPtsPerChan * header$nADCNumChannels
      } else {
         header$dataPtsPerChan <- floor(1e6*(whereStop-whereStart)*(1/header$si))
         header$dataPts <- header$dataPtsPerChan * header$nADCNumChannels
         if (header$dataPts <= 0) {
           close(f)
           stop("whereStart is larger than or equal to whereStop")
         }
      }
      if (header$dataPts %% header$nADCNumChannels > 0) {
         close(f)
         stop("number of data points not OK")
      }
      
      tmp <- 1e-6*header$lActualAcqLength*header$fADCSampleInterval
      if (verbose) {
         print(paste("total length of recording:", tmp, "s ~", tmp/60, "min"))
         print(paste("sampling interval:", header$si, "us"))
         # 8 bytes per data point expressed in Mb
         print(paste("memory requirement for complete upload:", round(8*header$lActualAcqLength/2^20), "MB")) # TODO: this is matlab-based, may be wrong in R
      }
      # recording start and stop times in seconds from midnight
      header$recTime <- header$lFileStartTime
      header$recTime <- c(header$recTime, header$recTime+tmp) # TODO: pay attention here, maybe it should be cbind instead of c
      tryCatch(seek(f, startPt*dataSz+headOffset), error = function (e) { # TODO: pretty sure it's a matlab only problem, not applicable to R
         close(f)
         stop("something went wrong positioning file pointer (too few data points ?)")
      })
      if (doLoadData) {
         # *** decide on the most efficient way to read data:
         # (i) all (of one or several) channels requested: read, done
         # (ii) one (of several) channels requested: use the 'skip' feature of
         # fread # TODO: this comment is NOT applicable to R code
         # (iii) more than one but not all (of several) channels requested:
         # 'discontinuous' mode of reading data. Read a reasonable chunk of data
         # (all channels), separate channels, discard non-requested ones (if
         # any), place data in preallocated array, repeat until done. This is
         # faster than reading the data in one big lump, separating channels and
         # discarding the ones not requested
         if (length(chInd) == 1 && header$nADCNumChannels > 1) {
            # --- situation (ii)
            # jump to proper reading frame position in file
            tryCatch(seek(f, (chInd-1)*dataSz, origin="current"), error = function (e) { # TODO: pretty sure it's a matlab only problem, not applicable to R
               close(f)
               stop("something went wrong positioning file pointer (too few data points ?)")
            })
            # read, skipping header$nADCNumChannels-1 data points after each read
            d <- c()
            for (i in 1:header$dataPtsPerChan) {
               d <- c(d, list(int16=int16, float32=float32)[[precision]](1))
               skip(dataSz*(header$nADCNumChannels-1))
            }
            n <- length(d)
            if (n != header$dataPtsPerChan) {
               close(f)
               stop("something went wrong reading file (", header$dataPtsPerChan, " points should have been read, ", n, " points actually read")
            }
         } else if (length(chInd)/header$nADCNumChannels < 1) {
            # --- situation (iii)
            # prepare chunkwise upload:
            # preallocate d
            d <- matrix(NaN, header$dataPtsPerChan, length(chInd)) # TODO: should R have NaN here, too?
            # the number of data points corresponding to the maximal chunk size,
            # rounded off such that from each channel the same number of points is
            # read (do not forget that each data point will by default be made a
            # double of 8 bytes, no matter what the original data format is)
            chunkPtsPerChan <- floor(chunk*2^20/8/header$nADCNumChannels)
            chunkPts <- chunkPtsPerChan*header$nADCNumChannels
            # the number of those chunks..
            nChunk <- floor(header$dataPts/chunkPts)
            # ..and the remainder
            restPts <- header$dataPts - nChunk*chunkPts
            restPtsPerChan <- restPts/header$nADCNumChannels
            # chunkwise row indices into d
            dix <- seq(1, header$dataPtsPerChan, by=chunkPtsPerChan)
            dix <- cbind(dix, dix+chunkPtsPerChan-1)
            dix[nrow(dix),2] <- header$dataPtsPerChan
            if (verbose && nChunk) {
               print(paste("reading file in", nChunk, "chunks of ~", chunk, "Mb"))
            }
            # do it: if no remainder exists loop through all rows of dix,
            # otherwise spare last row for the lines below (starting with
            # 'if restPts')
            for (ci in 1:(nrow(dix) - (restPts>0))) {
               tmpd <- list(int16=int16, float32=float32)[[precision]](chunkPts)
               n <- length(tmpd)
               if (n != chunkPts) {
                  close(f)
                  stop("something went wrong reading chunk #", ci, " (", chunkPts, " points should have been read, ", n, " points actually read")
               }
               # separate channels..
               tmpd <- matrix(tmpd, header$nADCNumChannels, chunkPtsPerChan)
               d[dix[ci,1]:dix[ci,2],] <- t(tmpd[chInd,])
            }
            # collect the rest, if any
            if (restPts) {
               tmpd <- list(int16=int16, float32=float32)[[precision]](restPts)
               n <- length(tmpd)
               if (n != restPts) {
                  close(f)
                  stop("something went wrong reading last chunk (", restPts, " points should have been read, ", n, " points actually read")
               }
               # separate channels..
               tmpd <- matrix(tmpd, header$nADCNumChannels, restPtsPerChan)
               d[dix[nrow(dix),1]:dix[nrow(dix),2],] <- t(tmpd[chInd,])
            }
         } else {
            # --- situation (i)
            d <- list(int16=int16, float32=float32)[[precision]](header$dataPts)
            n <- length(d)
            if (n != header$dataPts) {
               close(f)
               stop("something went wrong reading file (", header$dataPts, " points should have been read, ", n, " points actually read")
            }
            # separate channels..
            d <- matrix(d, header$nADCNumChannels, header$dataPtsPerChan)
            d <- t(d)
         }
         # if data format is integer, scale appropriately; if it's float, d is fine
         if (!header$nDataFormat) {
            for (j in 1:length(chInd)) {
               ch <- recChIdx[chInd[j]]+1
               d[,j]=d[,j]/(header$fInstrumentScaleFactor[ch]*header$fSignalGain[ch]*header$fADCProgrammableGain[ch]*addGain[ch])*header$fADCRange/header$lADCResolution+header$fInstrumentOffset[ch]-header$fSignalOffset[ch]
            }
         }
      }
   } else {
      print("unknown recording mode -- returning empty matrix") # TODO: is it a matrix?
      d <- list() # TODO: in matlab it was a cell array, so it may behave a little differently when assigning empty vectors
      header$si <- c() # TODO: same here, maybe not a vector; also we probably don't even need it here
   }

   close(f) # TODO: cleaner way
   # also TODO: more checks, like if you read integer(0)

   # finally, possibly add information on episode number to tags
   if (length(header$tags) > 0 && !is.null(header$sweepStartInPts)) {
     for (i in 1:length(header$tags)) {
       tmp <- which(header$tags[[i]]$timeSinceRecStart >= header$sweepStartInPts/1e6*header$si)
       header$tags[[i]]$episodeIndex <- tmp[length(tmp)]
      }
   }
   
   result <- list(
      path = normalizePath(filename),
      format_version = sprintf("%.2f", header$fFileVersionNumber),
      header = header,
      data = d
   )
   class(result) <- "ABF"
   result
}

# this function should be applied to the return value of readABF
# it produces a data frame that can be plotted
as.data.frame.ABF <- function (x, current=1, voltage=2) { # TODO: when should I use 1:3? AFAIK, I should divide Im.pA by Vm.mV
   si <- x$header$si # sampling interval (aka dt) in us
   si <- si * 1e-6 # converting to s
   data.frame(
      time = seq(0, by = si, length.out = nrow(x$data)),
      data = x$data[,current]/x$data[,voltage] # unit: nanoSiemens
   )
}

plot.ABF <- function (x, current=1, voltage=2, xlab="Time [s]", ylab="Conductance [nS]", ...) {
   plot(as.data.frame(x, current=1, voltage=2), xlab=xlab, ylab=ylab, ...)
}

print.ABF <- function (x) {
   cat("Path: ", x$path, "\n")
   cat("Format version: ", x$format_version, "\n")
   cat("Channel names: ", x$header$channel_names, "\n")
   cat("Channel units: ", x$header$channel_units, "\n")
   cat("Channel length: ", ncol(r$data), "\n")
   invisible(x)
}