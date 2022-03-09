#' Randomize a Two Proportions Hypothesis Test
#'
#' For a hypothesis test involving two proportions, create a vector of simulated
#' statistics by using randomization.
#'
#' @param failA The number of observed "failures" in "Group A". Must be a 
#' positive, whole number; if not, it will be rounded to the nearest whole
#' number.
#' @param successA The number of observed "successes" in "Group A". Must be a 
#' positive, whole number; if not, it will be rounded to the nearest whole
#' number.
#' @param totalA The sample size of "Group A". Must be a 
#' positive, whole number; if not, it will be rounded to the nearest whole
#' number.
#' @param failB The number of observed "failures" in "Group B". Must be a 
#' positive, whole number; if not, it will be rounded to the nearest whole
#' number.
#' @param successB The number of observed "successes" in "Group B". Must be a 
#' positive, whole number; if not, it will be rounded to the nearest whole
#' number.
#' @param totalB The sample size of "Group B". Must be a 
#' positive, whole number; if not, it will be rounded to the nearest whole
#' number.
#' @param numRepetitions The number of times to repeat the simulation process.
#' Must be a positive, whole number; if not, it will be rounded to the nearest
#' whole number.
#'
#' @return A vector of simulated statistics under the null hypothesis. The vector
#' will be the same length as `numRepetitions`.
#' @export
#'
#' @examples
#'
#' # Group A has 402 failures, 198 successes, 600 in total. Group B has 120 failures, 
#' 80 successes, 200 in total. Run a randomization 1000 times to see the simulated differences
#' in sample proportions (simulated statistics).
#' randomize(failA = 402, successA = 198, totalA = 600, failB = 120, successB = 80, 
#' totalB = 200, numRepetitions = 1000)
#'
randomize <- function(failA, successA, totalA, failB, successB, totalB, 
                      numRepetitions) {
  # round stuff
  failA <- round(failA)
  successA <- round(successA)
  totalA <- round(totalA)
  failB <- round(failB)
  successB <- round(successB)
  totalB <- round(totalB)
  numRepetitions <- round(numRepetitions)
  
  # check for errors in success/failure/total
  if (failA + successA != totalA)
    stop(paste("Group A's values have an error. Recall that the number of failures plus the number of successes must 
               equal the Group A total. Retype your values and try again."))
  
  if (failB + successB != totalB)
    stop(paste("Group B's values have an error. Recall that the number of failures plus the number of successes must 
               equal the Group B total. Retype your values and try again."))
  
  # create the data frame from observed data
  data_name <-data.frame(
    # below is the A rows for Group A and B rows for Group B
    "group" = c(rep("Group A", totalA), rep("Group B", totalB)),
    
    # below is the rate of Failure and Success
    "result" = c(rep("Failure", failA), rep("Success", successA), 
                 # of the first A rows
                 rep("Failure", failB), rep("Success", successB)), 
                 # of the last B rows
    
    stringsAsFactors = TRUE) 

  # simulate numRepetitions times and assign it to simulationResults
  simulationResults <- replicate(numRepetitions, {
    # shuffle the cards
    
    # Get the names of the original data
    dnames <- names(data_name)
    # Create a new data.frame in which the first column is the same as in d and
    # the second column is a permutation of the second column of d.
    shuffle <- data.frame(data_name[, 1],
                    data_name[sample(1:nrow(data_name), size = nrow(data_name), 
                                     replace = FALSE), 2])
    # Give our new data.frame the same names as the input
    names(shuffle) <- dnames
    
    # count up the successes in Group A, then divide by the total in Group A
    shuffleProportion1 <- sum(shuffle$group == "Group A" & shuffle$result == "Success") /  
      sum(shuffle$group == "Group A")
    
    # count up the successes in Group B, then divide by the total in Group B
    shuffleProportion2 <- sum(shuffle$group == "Group B" & shuffle$result == "Success") / 
      sum(shuffle$group == "Group B")
    
    # find the value of the statistic from this shuffle
    shuffleStatistic <- shuffleProportion1 - shuffleProportion2
    
    # report the value of the statistic from this shuffle
    shuffleStatistic
  })
  
  # return the results
  simulationResults 
}

