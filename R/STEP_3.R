#
# Step 3
#
# 1. Load one or more folds
# 2. Build the model ATS for each horizon in each training fold
# 3. Build the predict ATS for each horizon in each validation fold
# 4. Calculate the predition and the statistics
#

TRACE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
EVENT_TIMESTAMP_COLUMN_NAME <- "updated_at"
CLOSED_STATUS_VALUE <- "Closed"
REMOVE_LONGER_CASES <- FALSE


KFOLDS <- 5 # number of folds for the k-fold cross validation strategy

#LOG_LEVEL <- 1

STEP_3(c(Inf),1)

STEP_3() # to execute for all folds and the six horizons

#rm(events_list,events_row,model,model_list,model_row,predict,testingFold,trainingFold)
#rm(logfile,ds,traces,folds,filePaths,ats_list,eval_stats_arr,eval_stats_arr_,eval_stats_t,eval_stats_t00)
rm(eval_stats_arr,events_list,events_row,filePaths,model,model_list,model_row,predict,testingFold,trainingFold)
rm(training_stats,validation_stats,rfn,startFold,endFold,statsFile)
rm(fileN,i,j,startTime)

xxeval <- copy(eval_stats_df3)
for(i in 8:27) {
   xxeval[,i] <- as.numeric(as.character(xxeval[,i]))
}
xxgrp <- summarise_all (
   group_by(xxeval, sel_fields, k_fold, n_repet, max_itens, rem_outl, horizon, fold),
   funs(mean, sd))

STEP_3 <- function(horizons=c(1,3,5,6,7,Inf), justFold=0)
{
   generate_log(" ************* Initiating STEP 3 *************", 1)


   # attribute selection step
   #sel_attributes <- attribute_selection(logfile,1)
   sel_attributes <- c("incident_state", "category", "priority") # expert

   startTime <- format(Sys.time(), "%Y%m%d-%H%M")
   statsFile <- NULL # the base name for the files that will store all the results

   # logic to determine which fold files will be loaded
   if ( justFold == 0 )
   {
      startFold <- 1
      endFold <- KFOLDS
      statsFile <- paste("results_ALLFOLDS_",KFOLDS,"_",startTime,sep="")
   } else {
      startFold <- justFold
      endFold <- justFold
      statsFile <- paste("results_ONLYFOLD_",justFold,"_",startTime,sep="")
   }

   filePaths <- kfold_filenames(KFOLDS)

   model <- NULL
   predict <- NULL
   model_list <- NULL
   events_list <- NULL
   eval_stats_arr <- NULL
   #label <- NULL
   fileN <- 1
   for ( i in startFold:endFold )
   {
      #fold_ats_list <- vector("list",length(horizons))
      trainingFold <- read.csv(file=filePaths[[fileN]]) #,nrows = 104)

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

      testingFold <- read.csv(file=filePaths[[fileN+1]]) #,nrows = 104)
      # build the annotaded transition system (MTA)
      generate_log(paste("***** Starting fold ",i," *****"))
      model_row <- NULL
      events_row <- NULL
      rfn <- file.path("data/test",paste(statsFile,"_fold_",i,"_pred.csv",sep=""))
      for( j in 1:length(horizons) )
      {
         # builds the transition system
         model <- build_ats(trainingFold,horizons[j],sel_attributes)
         # anotates the transition system
         training_stats <- annotate_model(trainingFold, rfn, "T", i, horizons[j])

         # builds the transition system for teh testing fold
         predict <- build_prediction(testingFold,model)
         validation_stats <- annotate_model(testingFold, rfn, "V", i, horizons[j])

         model_row <- c(model_row, list(model), list(predict))
         events_row <- c(events_row, list(trainingFold), list(testingFold))
         #label <- c(label, list(cbind("T",horizons[j])), list(cbind("V",horizons[j])))

         eval_stats_arr <- rbind(eval_stats_arr, training_stats, validation_stats)
      }
      model_list <- c(model_list, list(model_row))
      events_list <- c(events_list, list(events_row))
      fileN = fileN + 2
      # estatisticas de avaliacao
      #eval_stats_arr <- rbind(eval_stats_arr, eval_model_gen_fn(events_row))
   }

   eval_stats_df1 <- data.frame(eval_stats_arr)

   #eval_stats_df2 <- cbind(expand.grid(fold=c("T","V"),horizon=horizons),eval_stats_df1)

   eval_stats_df3 <- data.frame(
      sel_fields = toString(sel_attributes), k_fold = KFOLDS, n_repet=1, max_itens = Inf,
      rem_outl='none', eval_stats_df1)

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


   return(eval_stats_df3)

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




xexec2_eval_stats_arr <- eval_stats_arr
xexec2_eval_stats_df3 <- eval_stats_df3
xexec2_model_list <- model_list
xexec2_grp_eval_stats <- grp_eval_stats_tp
