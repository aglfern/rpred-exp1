#
# Step 4
#
# Execute the selected configuration to build and MTA over the main set
# and the validation over the validation set separated at the beggining.
# 1. Load the main set (A+B) and build the MTA with the selected attributes and horizon
# 2. Predict using the MTA in the set C

# make sure that all the important constants are set before start
TRACE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
EVENT_TIMESTAMP_COLUMN_NAME <- "updated_at"
CLOSED_STATUS_VALUE <- "Closed"
REMOVE_LONGER_CASES <- FALSE


STEP_4 <- function(horizon=7,training_fn="preproc_ds1.csv",validation_fn="preproc_ds3.csv")
{
   generate_log(" ************* Initiating STEP 4 *************", 1)

   sel_attributes <- c("incident_state", "category", "priority") # expert

   startTime <- format(Sys.time(), "%Y%m%d-%H%M")
   # the base name for the files that will store all the results
   statsFile <- paste("results_STEP4_",startTime,sep="")

   model <- NULL
   predict <- NULL
   eval_stats_arr <- NULL

   trainingFold <- read.csv(file=file.path("data", training_fn)) #,nrows = 104)

   #option to remove from the training the outliers with elapsed time much bigger
   if ( REMOVE_LONGER_CASES == TRUE ) {
      q <- quantile(trainingFold$elapsed_stc,0.99)
      onePerc <- trainingFold[trainingFold$elapsed_stc > q,c("number","elapsed_stc")]
      onePercDist <- distinct(onePerc,onePerc$number)
      colnames(onePercDist) <- c("number")
      generate_log(paste("Removing ",nrow(onePercDist)," traces that have elapsed times bigger than [",q,"] seconds"))
      '%ni%' <- Negate('%in%')
      trainingFold <- trainingFold[trainingFold$number %ni% onePercDist$number,]
   }

   rfn <- file.path("data/test",paste(statsFile,"_pred.csv",sep=""))

   # builds the transition system
   model <- build_ats(trainingFold,horizon,sel_attributes)
   # anotates the transition system
   training_stats <- annotate_model(trainingFold, rfn, "T", 0, horizon)

   # prediction over the validation data set
   testingFold <- read.csv(file=file.path("data", validation_fn)) #,nrows = 104)

   # builds the transition system for teh testing fold
   predict <- build_prediction(testingFold,model)
   validation_stats <- annotate_model(testingFold, rfn, "V", 0, horizon)

   eval_stats_arr <- rbind(training_stats, validation_stats)


   eval_stats_df1 <- data.frame(eval_stats_arr)

   sfilen <- file.path("data/test",paste(statsFile,"_STATS.csv",sep=""))
   write.table(eval_stats_df1, file=sfilen, row.names=FALSE, col.names = TRUE, sep=";", dec=",")

   grp_eval_stats_tp <- summarise_all (
      group_by(eval_stats_df1, horizon, fold),
      funs(mean, sd), na.rm = TRUE)

   sfilen <- file.path("data/test",paste(statsFile,"_GRSTATS.csv",sep=""))
   write.table(eval_stats_df1, file=sfilen, row.names=FALSE, col.names = TRUE, sep=";", dec=",")

   generate_log("Step 4 completed successfully.",2)

   return(eval_stats_df1)

}
