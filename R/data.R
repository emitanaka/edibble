

#' Skittles experiment
#'
#' @description
#' This contains the data from the skittle experiment conducted by
#' Nick Tierney. The goal of the experiment was to assess if people
#' can discern the flavour of the skittle (indicated by color of the skittle)
#' based on taste alone. The participants are blindfolded.
#'
#' The experiment had 3 participants with each participant
#' tasting 10 skittles, 2 of each 5 color, in a random order.
#' \describe{
#' \item{skittle_type}{The type of skittle. Coincides with `real_skittle`.}
#' \item{person}{The participant.}
#' \item{order}{The order the skittle was tasted.}
#' \item{choice}{The participant's choice.}
#' \item{real_skittle}{The actual skittle color.}
#' }
#'
#'
#' @source https://github.com/njtierney/skittles
"skittles"

# TODO Ask Nick whether participants knew if they knew the color was 2 each
