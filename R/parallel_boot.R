#' Parallel wrapper for bootstrap functions (chunked by workers)
#'
#' @description
#' Runs a non-parallel bootstrap function in parallel by splitting the total
#' number of replicates \code{R} into \code{k} chunks and evaluating each chunk
#' in a separate future. Verbose output is disabled in all chunks. Results are
#' combined with \code{rbind}.
#'
#' @param ... arguments forwarded to \code{BOOT.FUN}, e.g., \code{bootstrap_modsem}
#' @param BOOT.FUN Function. The bootstrap function to be wrapped.
#' @param R Integer. Total number of bootstrap replicates.
#' @param k Integer. Number of chunks (defaults to \code{future::nbrOfWorkers()}).
#' @param future.seed Logical or integer. Passed to \code{future.apply::future_lapply()}
#'   as \code{future.seed} for reproducible RNG. Default \code{TRUE}.
#'
#' @return A matrix formed by \code{rbind}-ing the chunk results. Column names
#' are taken from the first non-empty chunk when available.
#'
#' @examples
#' \dontrun{
#' # You might need to install these
#' library(future)
#' library(future.apply)
#' library(modsem)
#' library(modsemTools)
#'
#' plan(multisession, workers = 8)  # multicore also works, but only on Linux/MacOs
#'
#' m1 <- '
#' # Outer Model
#'   X =~ x1 + x2 + x3
#'   Z =~ z1 + z2 + z3
#'   Y =~ y1 + y2 + y3
#'
#' # Inner Model
#'   Y ~ X + Z + X:Z
#' '
#'
#' fit <- modsem(m1, data = oneInt, method = "lms", n.threads = 1L)
#' boot <- bootstrap_parallel(
#'   model = fit,
#'   R = 1000L,
#'   FUN = "coef"
#' )
#' }
#' @export
bootstrap_parallel <- function(...,
                               R = 1000,
                               k = NULL,
                               BOOT.FUN = modsem::bootstrap_modsem,
                               future.seed = TRUE) {
  if (is.null(k)) k <- max(1L, tryCatch(future::nbrOfWorkers(), error = \(...) 1L))
  else            k <- max(1L, as.integer(k))

  # Compute chunk sizes that sum to R (distribute the remainder to the first chunks)
  base <- R %/% k
  rem  <- R %%  k
  Rchunks <- rep.int(base, k)
  if (rem > 0L) Rchunks[seq_len(rem)] <- Rchunks[seq_len(rem)] + 1L

  .boot_i = function(i) {
    if (Rchunks[i] <= 0L) return(NULL)
    args <- c(list(...), list(R = Rchunks[i], verbose = FALSE))
    res <- do.call(BOOT.FUN, args)
  }

  # Launch k futures; skip zero-sized chunks to avoid unnecessary tasks
  parts <- future.apply::future_lapply(
    FUN = .boot_i, X = seq_len(k), future.seed = future.seed
  )

  parts <- Filter(Negate(is.null), parts)
  if (!length(parts))
    return(matrix(numeric(0), nrow = 0, ncol = 0))

  if (is.matrix(parts[[1L]])) do.call(rbind, parts)
  else                        parts
}


