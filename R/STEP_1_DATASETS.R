#
# Step 1: create the datasets
#
#   1. Load the original log file as the main dataset
#   2. Creates x new datasets, according to the specified split
#   3. Saves the subdatasets into new files
#

CASE_ID_COLUMN_NAME <- "number"
STATUS_COLUMN_NAME <- "incident_state"
CLOSED_STATUS_VALUE <- "Closed"
DATASET_SIZES <- c(8000, 12000, 5000)
FILENAME <- "novo_log_sara.csv"

#LOG_LEVEL <- 1

STEP_1()

STEP_1 <- function()
{

   generate_log(" ************* Initiating STEP 1 *************", 1)
   filePath <- file.path("data", FILENAME)
   logfile <-  read.table(filePath, quote = "\"", sep = ",", dec = ".",
                          header = T, encoding = "utf-8", stringsAsFactors = T) #, nrow=100)
   generate_log(paste("Log file",filePath,"loaded with ", nrow(logfile), " rows and ", ncol(logfile), " columns"),1)

   # getting only the closed cases
   only_closed <- as.data.frame(logfile[which(logfile[,STATUS_COLUMN_NAME] == CLOSED_STATUS_VALUE),])
   generate_log(paste("There are ", nrow(only_closed), " rows in [", CLOSED_STATUS_VALUE,"] state"),1)

   # getting the distinct rows, ie, the traces
   traces <- distinct_(only_closed, CASE_ID_COLUMN_NAME)
   generate_log(paste("There are ", nrow(traces), " distinct rows, ie, traces"),1)

   # splitting
   new_datasets <- dataset_split(traces,DATASET_SIZES)

   # dataset 1
   for(i in 1 : length(new_datasets))
   {
      selected <- as.data.frame(new_datasets[i])
      colnames(selected) <- "CaseId"
      ds <- subset.data.frame(logfile, logfile[,CASE_ID_COLUMN_NAME] %in% selected$CaseId)
      newdsfile <- file.path("data", paste("step1_ds",i,".csv",sep=""))
      write.csv2(ds,file=newdsfile)
      generate_log(paste("Split", i, "contains", length(new_datasets[[i]]), "cases and ",
                         nrow(ds), "events; the new dataset was stored on file",newdsfile),1)
   }
}
