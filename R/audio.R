#' Summary audio
#'
#' Audio summaries can be played withing the TensorBoard UI.
#'
#' @param audio Object that will be written as an audio event in the tfevents record.
#' @inheritParams summary_image
#' @param sample_rate The sample rate in Hz associated to the audio values.
#' @returns An audio summary that can be logged with [log_event()].
#' @family summary
#' @examples
#' tmp <- tempfile()
#' with_logdir(tmp, {
#'   summary_audio(array(runif(100), dim = c(1,100, 1)))
#' })
#' @export
summary_audio <- function(audio, ..., metadata = NULL, tag = NA) {
  UseMethod("summary_audio")
}

#' @describeIn summary_audio Creates a summary from a 3D array with dimensions
#'   `(batch_size, n_samples, n_channels)`. Values must be in the range `[-1, 1]`.
#' @export
summary_audio.array <- function(audio, ..., sample_rate = 44100, metadata = NULL,
                          tag = NA) {

  rlang::check_installed("wav")
  temp <- tempfile()
  raw_audios <- apply(audio, 1, simplify = FALSE, function(x) {
    wav::write_wav(t(x), sample_rate = sample_rate, path = temp)
    sze <- fs::file_info(temp)$size
    readBin(temp, n = sze, what = "raw")
  })
  blob_audios <- blob::new_blob(raw_audios)
  summary_audio(
    blob_audios,
    metadata = metadata,
    tag = tag
  )
}

#' @describeIn summary_audio Creates an audio summary from a raw vector containing
#'  a WAV encoded audio file.
#' @export
summary_audio.raw <- function(audio, ..., metadata = NULL, tag = NA) {
  summary_audio(blob::blob(audio), metadata = metadata, tag = tag)
}

#' @describeIn summary_audio Creates an audio summary from a blob (ie list of raw vectors)
#'   containing WAV encoded audio files.
#' @export
summary_audio.blob <- function(audio, ..., metadata = NULL, tag = NA) {

  if (is.null(metadata)) {
    metadata <- summary_metadata(plugin_name = "audio")
  }

  summary_tensor(
    audio,
    dtype = "string",
    metadata = metadata,
    tag = tag
  )
}
