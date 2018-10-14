#
# Step 2
#
# 1. Pre-processing of the selected log file (split 1)
# 2. Attribute selecion, in this case will be based on expert vision
# 3. Generate the k-fold cross validation structure
#

TRACE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
EVENT_TIMESTAMP_COLUMN_NAME <- "updated_at"
CLOSED_STATUS_VALUE <- "Closed"

S2_FILENAME <- "step1_ds1.csv"

KFOLDS <- 5 # number of folds for the k-fold cross validation strategy

#LOG_LEVEL <- 1

STEP_2()

STEP_2 <- function()
{
   generate_log(" ************* Initiating STEP 2 *************", 1)

   logfile <- read.csv2(file.path("data", S2_FILENAME))
   generate_log(paste("Log file loaded with ", nrow(logfile), " rows and ", ncol(logfile), " columns"),1)

   # pre-processing: date/time field handling, id handling.
   ds = dataset_preproc_version1(logfile,2)

   # attribute selection step
   #sel_attributes <- attribute_selection(logfile,1)
   sel_attributes <- c("incident_state", "category", "priority") # expert

   # generating the indexes for the k-fold cross validation
   # they must be based on the traces, not on the events, so they may have distinct rows
   #require(dplyr)
   traces <- distinct_(ds, TRACE_ID_COLUMN_NAME)
   folds <- generate_kfolds(traces,KFOLDS)

   # will store the logs for the selected traces into separated CSV files
   filePaths <- save_kfolds(ds, traces,folds,KFOLDS)

   horizons <- c(1,3,5,7,Inf)
   ran_ats_list <- vector("list",KFOLDS)
   #ats <- matrix(data=NA, nrow = KFOLDS, ncol = length(horizons))
   for ( i in 1:KFOLDS )
   {
      fold_ats_list <- vector("list",length(horizons))
      trainingFold <- read.csv(file=filePaths[[i]]) #,nrows = 104)
      # build the annotaded transition system (MTA)
      for( j in 1:length(horizons) )
      {
         fold_ats_list[[j]] <- build_ats(trainingFold,horizons[j],sel_attributes)
      }
      ran_ats_list[[i]] <- fold_ats_list
   }
   generate_log(paste("Generated all the Annotated Transition Systems (ATS) for",KFOLDS,"folds."),2)
}



#' Saves the folds into separated CSV files, returning the filepaths
#'
#' @param traces
#' @param folds
#' @param k
#'
#' @return List of filepaths, being odd numbers the training seet and even numbers the testing set
#' @export
#'
#' @examples
save_kfolds <- function(events, traces, folds, k)
{
   filePaths <- vector("list",2*k)
   fileN <- 1
   for(i in 1:k)
   {
      # training file for execution i
      filePaths[fileN] <- file.path("data/test",paste("fold",i,"_train.csv",sep=""))
      sel_traces <- data.table(number=folds[["trainingSet"]][[1]], stringsAsFactors = FALSE)
      sel_events <- events[which(events$number %in% sel_traces$number),]
      write.csv(sel_events, file=filePaths[[fileN]], row.names=FALSE)

      # testing file for execution i
      filePaths[fileN+1] <- file.path("data/test",paste("fold",i,"_test.csv",sep=""))
      sel_traces <- data.table(number=folds[["testingSet"]][[1]], stringsAsFactors = FALSE)
      sel_events <- events[which(events$number %in% sel_traces$number),]
      write.csv(sel_events, file=filePaths[[fileN+1]], row.names=FALSE)

      generate_log(paste("Files generated for cross-validation step",i,":",filePaths[fileN],"and",filePaths[fileN+1]),1)

      fileN = fileN + 2
   }
   return(filePaths)
}



#   install.packages("microbenchmark")
#   library(microbenchmark)
#   microbenchmark(setorder(ds_,number,updated_at_stc))
#   microbenchmark(setDT(ds)[order("number", "updated_at_stc")])


