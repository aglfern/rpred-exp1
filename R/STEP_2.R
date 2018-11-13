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
   sel_attributes <- c("incident_state", "category", "priority") # expert

   # generating the indexes for the k-fold cross validation
   # they must be based on the traces, not on the events, so they may have distinct rows
   #require(dplyr)
   traces <- distinct_(ds, TRACE_ID_COLUMN_NAME)
   folds <- generate_kfolds(traces,KFOLDS)

   # will store the logs for the selected traces into separated CSV files
   filePaths <- save_kfolds(ds, traces,folds,KFOLDS)

}

STEP_3 <- function(horizons, justFold=0, execName="std")
{
   generate_log(" ************* Initiating STEP 3 *************", 1)

   startTime <- format(Sys.time(), "%Y%m%d-%H%M")

   horizons <- c(1,3,5,6,7,Inf)

   #ats <- matrix(data=NA, nrow = KFOLDS, ncol = length(horizons))

   # ** copying the summarize code
   # sel_traces_row <- NULL
   # mta_model_row <- NULL
   # mta_model_trn <- NULL
   # sel_traces_row <- c(sel_traces_row, list(sel_traces_))
   # mta_model_row <- c(mta_model_row, list(mta_model_))
   # sel_traces_list <- c(sel_traces_list, list(sel_traces_row))
   # mta_model_list <- c(mta_model_list, list(mta_model_row))
   # **

   if ( justFold == 0 )
   {
      startFold <- 1
      endFold <- KFOLDS
   } else {
      startFold <- justFold
      endFold <- justFold
   }

   filePaths <- kfold_filenames(KFOLDS)

   model <- NULL
   predict <- NULL
   model_list <- NULL
   events_list <- NULL
   eval_stats_arr <- NULL
   label <- NULL
   fileN <- 1
   for ( i in startFold:endFold )
   {
      fold_ats_list <- vector("list",length(horizons))
      trainingFold <- read.csv(file=filePaths[[fileN]]) #,nrows = 104)
      testingFold <- read.csv(file=filePaths[[fileN+1]]) #,nrows = 104)
      # build the annotaded transition system (MTA)
      generate_log(paste("***** Starting fold ",i," *****"))
      model_row <- NULL
      events_row <- NULL
      for( j in 1:length(horizons) )
      {
         model <- build_ats(trainingFold,horizons[j],sel_attributes)
         predict <- build_prediction(testingFold,model)
         model_row <- c(model_row, list(model), list(predict))
         events_row <- c(events_row, list(trainingFold), list(testingFold))
         label <- c(label, list(cbind("T",horizons[j])), list(cbind("V",horizons[j])))
      }
      model_list <- c(model_list, list(model_row))
      events_list <- c(events_list, list(events_row))
      fileN = fileN + 2
      # estatisticas de avaliacao
      eval_stats_arr <- rbind(eval_stats_arr, eval_model_gen_fn(events_row))
   }

   eval_stats_df1 <- data.frame(eval_stats_arr)

   eval_stats_df2 <- cbind(expand.grid(fold=c("T","V"),horizon=horizons),eval_stats_df1)

   eval_stats_df3 <- data.frame(
      sel_fields = toString(sel_attributes), k_fold = KFOLDS, n_repet=1, max_itens = Inf,
      rem_outl='none', eval_stats_df2)

   if ( justFold == 0 )
   {
      statsFile <- paste("results_ALLFOLDS_",KFOLDS,"_",startTime,sep="")
   } else {
      statsFile <- paste("results_ONLYFOLD_",justFold,"_",startTime,sep="")
   }

   sfilen <- file.path("data/test",paste(statsFile,"_STATS.csv",sep=""))
   write.csv2(eval_stats_df3, file=sfilen, row.names=FALSE)

   # sfilen <- file.path("data/test",paste(statsFile,"_MODELS.csv",sep=""))
   # write.csv(data_frame(unlist(model_list)), file=sfilen, row.names=FALSE)
   #
   # write.xlsx(as.data.frame(unlist(lpList)), filterResFileName,
   #            sheetName=toString("parameter"))

   grp_eval_stats_tp <- summarise_all (
      group_by(eval_stats_df3, sel_fields, k_fold, n_repet, max_itens, rem_outl, horizon, fold),
      funs(mean, sd), na.rm = TRUE)

   sfilen <- file.path("data/test",paste(statsFile,"_GRSTATS.csv",sep=""))
   write.csv2(grp_eval_stats_tp, file=sfilen, row.names=FALSE)

   generate_log("Step 3 completed successfully.",2)


   return(eval_stats_df2)

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


