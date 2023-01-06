#' Function to calculate Mel-frequency cepstral coefficents over a list of focal recordings
#'
#' @param input.dir where the .wav files are stored
#' @param min.freq the minimum frequency (Hz) of the signal of interest
#' @param max.freq the maximum frequency (Hz) of the signal of interest
#' @param n.windows the number of time windows to divide the signal by
#' @param num.cep the number of cepstra to calculate for each time window
#' @importFrom tuneR readWave
#' @importFrom stringr str_split_fixed
#' @importFrom seewave duration
#' @importFrom tuneR melfcc
#' @return a datframe with a row for each .wav file
#' @export
#'
#' @examples
#' \donttest{MFCCFunction(input.dir = "FocalRecordings",min.freq = 200,max.freq=10000)}

MFCCFunctionMeanSD <-
  function(input.dir, min.freq = 400, max.freq = 2400,
            num.cep = 12,ndash=2,dashindex=1) {

    call.timing.list <- list.files(input.dir,full.names = T,pattern='.wav')

    call.timing.list.short <- list.files(input.dir,full.names = F,pattern='.wav')
    subsamps <- lapply(1:length(call.timing.list),
                       function(i) tuneR::readWave(call.timing.list[[i]]))


    mfcc.vector.list <- list()
    Class <- stringr::str_split_fixed(call.timing.list.short,pattern = '_',n=ndash)[,dashindex]
    for(x in 1:length(subsamps)){
      #print(paste("processing sound event", x, 'out of',length(subsamps) ))

      short.wav <- subsamps[[x]]
      wav.dur <- seewave::duration(short.wav)
      # Calculate MFCCs
      melfcc.output <- tuneR::melfcc(short.wav, minfreq = min.freq,
                                      maxfreq = max.freq,
                                     numcep = num.cep)

      # Calculate delta cepstral coefficients
      deltas.output <- as.data.frame(tuneR::deltas(melfcc.output))


      melfcc.output <- as.data.frame(melfcc.output)

      mfcc.mean <- colMeans(melfcc.output)
      mfcc.sd <- apply(melfcc.output,2,sd)
      delta.mean <-colMeans(deltas.output)
      delta.sd <- apply(deltas.output,2,sd)

      # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
      mfcc.vector <- c(mfcc.mean,mfcc.sd,delta.mean,delta.sd,wav.dur)
      mfcc.vector.list[[x]] <- mfcc.vector
    }

    mfcc.output <- mfcc.vector.list
    class <- stringr::str_split_fixed(call.timing.list.short,pattern = '_',n=2)[,1]


    mfcc.output.df <- do.call(rbind.data.frame,mfcc.output)
    colnames(mfcc.output.df) <- seq(from=1, to=ncol(mfcc.output.df),by=1)

    mfcc.output.df <- cbind.data.frame(class,mfcc.output.df)
    return(mfcc.output.df)


  }
