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

STEP_2_1()

STEP_2_1_AB()

STEP_2_1(infn="step1_ds3.csv",outfn="preproc_ds3.csv")

#rm(events_list,events_row,model,model_list,model_row,predict,testingFold,trainingFold)
#rm(logfile,ds,traces,folds,filePaths,ats_list,eval_stats_arr,eval_stats_arr_,eval_stats_t,eval_stats_t00)


STEP_2 <- function()
{
   generate_log(" ************* Initiating STEP 2 *************", 1)

   logfile <- read.csv2(file.path("data", S2_FILENAME))
   generate_log(paste("Log file loaded with ", nrow(logfile), " rows and ", ncol(logfile), " columns"),1)

   # pre-processing: date/time field handling, id handling.
   ds = dataset_preproc_version1(logfile,2)

   # attribute selection step
   #sel_attributes <- attribute_selection(logfile,1)
   #sel_attributes <- c("incident_state", "category", "priority") # expert

   # generating the indexes for the k-fold cross validation
   # they must be based on the traces, not on the events, so they may have distinct rows
   #require(dplyr)
   traces <- distinct_(ds, TRACE_ID_COLUMN_NAME)
   folds <- generate_kfolds(traces,KFOLDS)

   # will store the logs for the selected traces into separated CSV files
   filePaths <- save_kfolds(ds, traces,folds,KFOLDS)

}

#
# Step to only preprocess the data and save it into one single file
# The output format is the same as the KFOLDs, ready to be processed by the MTA build
#
STEP_2_1 <- function(infn="step1_ds1.csv",outfn="preproc_ds1.csv")
{
   generate_log(" ************* Initiating STEP 2.1 *************", 1)

   logfile <- read.csv2(file.path("data", infn))
   generate_log(paste("Log file loaded with ", nrow(logfile), " rows and ", ncol(logfile), " columns"),1)

   # pre-processing: date/time field handling, id handling.
   ds = dataset_preproc_version1(logfile,2)

   write.csv(ds, file=file.path("data", outfn), row.names=FALSE)

}


#
# Step to only preprocess the data and save it into one single file
# The output format is the same as the KFOLDs, ready to be processed by the MTA build
#
STEP_2_1_AB <- function()
{
   generate_log(" ************* Initiating STEP 2.1 for sets A and B *************", 1)

   logfileA <- read.csv2(file.path("data", "step1_ds1.csv"))
   generate_log(paste("Log file loaded with ", nrow(logfileA), " rows and ", ncol(logfileA), " columns"),1)

   logfileB <- read.csv2(file.path("data", "step1_ds2.csv"))
   generate_log(paste("Log file loaded with ", nrow(logfileB), " rows and ", ncol(logfileB), " columns"),1)

   logfile <- rbind(logfileA,logfileB)
   generate_log(paste("Final log file loaded with ", nrow(logfile), " rows and ", ncol(logfile), " columns"),1)

   # pre-processing: date/time field handling, id handling.
   ds = dataset_preproc_version1(logfile,2)

   write.csv(ds, file=file.path("data", "preproc_ds1and2.csv"), row.names=FALSE)

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
      sel_traces <- data.table(number=folds[["trainingSet"]][[i]], stringsAsFactors = FALSE)
      sel_events <- events[which(events$number %in% sel_traces$number),]
      write.csv(sel_events, file=filePaths[[fileN]], row.names=FALSE)

      # testing file for execution i
      filePaths[fileN+1] <- file.path("data/test",paste("fold",i,"_test.csv",sep=""))
      sel_traces <- data.table(number=folds[["testingSet"]][[i]], stringsAsFactors = FALSE)
      sel_events <- events[which(events$number %in% sel_traces$number),]
      write.csv(sel_events, file=filePaths[[fileN+1]], row.names=FALSE)

      generate_log(paste("Files generated for cross-validation step",i,":",filePaths[fileN],"and",filePaths[fileN+1]),1)

      fileN = fileN + 2
   }
   return(filePaths)
}

save_kfolds2 <- function(events, traces, folds, k)
{
   filePaths <- kfold_filenames(k)
   fileN <- 1
   for(i in 1:k)
   {
      # training file for execution i
      sel_traces <- data.table(number=folds[["trainingSet"]][[i]], stringsAsFactors = FALSE)
      sel_events <- events[which(events$number %in% sel_traces$number),]
      write.csv(sel_events, file=filePaths[[fileN]], row.names=FALSE)

      # testing file for execution i
      sel_traces <- data.table(number=folds[["testingSet"]][[i]], stringsAsFactors = FALSE)
      sel_events <- events[which(events$number %in% sel_traces$number),]
      write.csv(sel_events, file=filePaths[[fileN+1]], row.names=FALSE)

      generate_log(paste("Files generated for cross-validation step",i,":",filePaths[fileN],"and",filePaths[fileN+1]),1)

      fileN = fileN + 2
   }
   return(filePaths)
}

#' Title
#'
#' @param k
#'
#' @return
#' @export
#'
#' @examples
kfold_filenames <- function(k)
{
   filePaths <- vector("list",2*k)
   fileN <- 1
   for(i in 1:k)
   {
      filePaths[fileN] <- file.path("data/test",paste("fold",i,"_train.csv",sep=""))
      filePaths[fileN+1] <- file.path("data/test",paste("fold",i,"_test.csv",sep=""))
      fileN = fileN + 2
   }
   return(filePaths)
}

#   install.packages("microbenchmark")
#   library(microbenchmark)
#   microbenchmark(setorder(ds_,number,updated_at_stc))
#   microbenchmark(setDT(ds)[order("number", "updated_at_stc")])


