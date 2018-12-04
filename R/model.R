
NIL_STATE <- -1


#' Title
#'
#' @param astate_list
#' @param asigma
#' @param amode
#'
#' @return
#' @export
#'
#' @examples
state_fn2 <- function(astate_list, asigma, amode = "sequence")
{
   # pesquisa lista de estados
   lstate_list <- astate_list
   # print(asigma)
   # print(lstate_list)
   Result <- match(asigma, lstate_list)
   if (is.na(Result))
   {
      Result = length(lstate_list) + 1
      # print(Result)
      lstate_list[Result] <- asigma
      eval.parent(substitute(astate_list<-lstate_list))
   }
   return(Result)
}

#
# Alexandre: funcao removida, trocada pelo match, com parametro de retorno quando NO MATCH
#
# find_state_fn2 <- function(astate_list, asigma, amode = "sequence")
# {
#    # pesquisa lista de estados
#    Result <- match(asigma, astate_list)
#    if (is.na(Result))
#    {
#       Result = NIL_STATE
#    }
#    return(Result)
# }

#' funcao de construcao do MTA, recebe o trace, lista de instancias
#' Código fonte original
#'
#' @param events : list of events
#' @param horiz : the horizon
#' @param sel_attributes : list of selected attributes
#'
#' @return
#' @export
#'
#' @examples
build_ats <- function(aevents, horiz, sel_attributes)
{

   # mudanças nos nomes das variáveis
   #  nome no código original    novo nome
   #  asel_traces_lcl            events
   #  sel_traces_lcl             events (copy)
   #  process_instances          traces
   #  trace                      traceEvents

   events <- aevents  # necessário para atualizar a lista original no final da execução

   generate_log(paste("Starting build_ats for horizon",horiz,"and attributes [",paste( unlist(sel_attributes), collapse=', '),"]"),2)

   traces <- distinct_(events, TRACE_ID_COLUMN_NAME)

   traces_states <-list(
      number=traces,
      set_states=list(),
      mset_states=list(),
      seq_states=list()
   )

   # listas com os estados de cada modelo
   seq_state_list <- list()
   set_state_list <- list()
   mset_state_list <- list()


   generate_log("   inicio dos loops",2)

   for(i in 1:nrow(traces))
   {

      #print(paste("inicio loop principal: ",i))

      # busca eventos do caso / search events of the given trace
      traceEvents <- events[events$number == traces$number[i],]

      #generate_log(paste("          trace",traces$number[i],"with",nrow(traceEvents),"events"),2)

      # it must happen due to the backward update to the original events in the end of this function
      traceEvents$seq_state_id <- 0
      traceEvents$set_state_id <- 0
      traceEvents$mset_state_id <- 0
      traceEvents$sojourn_set_stc <- 0
      traceEvents$sojourn_mset_stc <- 0
      traceEvents$sojourn_seq_stc <- 0

      activity_fields <- do.call(paste, as.data.frame(traceEvents[,sel_attributes]))

      # vector with the timestamps for when the event has started
      timestamp_field <-  as.vector(traceEvents[, EVENT_TIMESTAMP_COLUMN_NAME])

      # Step 1: populate the lists of states for each abstraction
      seq_list <- list()
      set_list <- list()
      mset_list <- list()
      for (j in 1:nrow(traceEvents))
      {
         #print(paste("inicio loop 1: ",j))

         # calculate based on the horizon
         horiz_index <- max(j - horiz + 1, 1)

         # generates state for SEQUENCE abstration
         inc_seq <- as.character(activity_fields[horiz_index:j])
         seq_list[[j]]  <- toString(inc_seq)

         # generates state for SET
         inc_set <- as.set(inc_seq)
         set_list[[j]]  <- toString(inc_set)
         #stateId <- state_fn2(set_state_list, set_list[[j]])
         stateId <- match(set_list[[j]],set_state_list,NIL_STATE)
         if ( stateId == NIL_STATE ) {
            set_state_list <- append(set_state_list, set_list[[j]])
            stateId <- length(set_state_list)
         }

         # generates state for multi-set
         inc_gset <- as.gset(inc_seq)
         inc_gset_str <- toString(rbind(gset_support(inc_gset), gset_memberships(inc_gset)))
         mset_list[[j]]  <- inc_gset_str

         if (horiz==1) # horizon == 1, all will be the same
         {
            traceEvents[j,c("set_state_id","mset_state_id","seq_state_id")] <- stateId
         }
         else
         {
            traceEvents[j,"set_state_id"] <- stateId

            # now for the multi-set
            #traceEvents[j,"mset_state_id"] <- state_fn2(mset_state_list, inc_gset_str)

            stateId <- match(mset_list[[j]],mset_state_list,NIL_STATE)
            if ( stateId == NIL_STATE ) {
                mset_state_list <- append(mset_state_list, mset_list[[j]])
                stateId <- length(mset_state_list)
            }
            traceEvents[j,"mset_state_id"] <- stateId

            # and for the sequence
            #traceEvents[j,"seq_state_id"] <- state_fn2(seq_state_list, seq_list[[j]])

            stateId <- match(seq_list[[j]],seq_state_list,NIL_STATE)
            if ( stateId == NIL_STATE ) {
               seq_state_list <- append(seq_state_list, seq_list[[j]])
               stateId <- length(seq_state_list)
            }

            traceEvents[j,"seq_state_id"] <- stateId
         }
      }
      # Step 2, calculate the sojourn
      curr_sojourn_set_state <- traceEvents[1,"set_state_id"]
      curr_sojourn_mset_state <- traceEvents[1,"mset_state_id"]
      curr_sojourn_seq_state <- traceEvents[1,"seq_state_id"]
      curr_sojourn_set_stc <- 0
      curr_sojourn_mset_stc <- 0
      curr_sojourn_seq_stc <- 0
      # first event of each case, the sojourn will be always zero
      for (j in 2:nrow(traceEvents))
      {
         #print(paste("inicio loop 2: ",j))

         elapsed <- traceEvents[j,"elapsed_stc"]
         # advances until it reaches a distinct state - for SET
         if ( traceEvents[j,"set_state_id"] != curr_sojourn_set_state ) {
            curr_sojourn_set_state <- traceEvents[j,"set_state_id"]
            traceEvents[j,"sojourn_set_stc"] <- elapsed - curr_sojourn_set_stc
            curr_sojourn_set_stc <- elapsed
         }

         # now for MULTI-SET
         if ( traceEvents[j,"mset_state_id"] != curr_sojourn_mset_state ) {
            curr_sojourn_mset_state <- traceEvents[j,"mset_state_id"]
            traceEvents[j,"sojourn_mset_stc"] <- elapsed - curr_sojourn_mset_stc
            curr_sojourn_mset_stc <- elapsed
         }

         # and now for SEQUENCE
         if ( traceEvents[j,"seq_state_id"] != curr_sojourn_seq_state ) {
            curr_sojourn_seq_state <- traceEvents[j,"seq_state_id"]
            traceEvents[j,"sojourn_seq_stc"] <- elapsed - curr_sojourn_seq_stc
            curr_sojourn_seq_stc <- elapsed
         }

      } # fim j
      # armazena resultado das transicoes de estado para instancia atual
      # modelo de abstracao sequencia
      traces_states$seq_states[i] <- list(traceEvents[,"seq_state_id"])

      # modelo de abstracao set
      traces_states$set_states[i] <- list(traceEvents[,"set_state_id"])

      # modelo de abstracao mset
      traces_states$mset_states[i] <- list(traceEvents[,"mset_state_id"])

      # guardo a estado no evento
      events[events$number == traces$number[i],]$seq_state_id <- traceEvents$seq_state_id
      events[events$number == traces$number[i],]$set_state_id <- traceEvents$set_state_id
      events[events$number == traces$number[i],]$mset_state_id <- traceEvents$mset_state_id
      events[events$number == traces$number[i],]$sojourn_set_stc <- traceEvents$sojourn_set_stc
      events[events$number == traces$number[i],]$sojourn_mset_stc <- traceEvents$sojourn_mset_stc
      events[events$number == traces$number[i],]$sojourn_seq_stc <- traceEvents$sojourn_seq_stc

   } # fim i

   generate_log("   fim dos loops",2)

   # retorna resultado - precisa atualizar a lista de eventos original, para os cálculos
   eval.parent(substitute(aevents<-events))

   mta_model <- list(traces_states=traces_states,
                     seq_state_list=seq_state_list,
                     set_state_list=set_state_list,
                     mset_state_list=mset_state_list,
                     horiz=horiz,
                     sel_attributes=sel_attributes
   )

   #generate_log("Finalizando build_ats",2)

   return(mta_model)
}





#' funcao de construcao dos MTA recebe o trace, lista de instancias
#'
#' @param asel_traces_lcl
#' @param process_instances
#' @param ats
#'
#' @return
#' @export
#'
#' @examples
#build_prediction <- function(asel_traces_lcl, process_instances, mta_in)
build_prediction <- function(aevents, ats)
{
   generate_log("Starting build_prediction",2)

   events <- aevents  # necessário para atualizar a lista original no final da execução

   traces <- distinct_(events, TRACE_ID_COLUMN_NAME)

#   process_instances_states <-list(
   traces_states <-list(
      number=traces,
      set_states=list(),
      mset_states=list(),
      seq_states=list()
   )

   # listas com os estados de cada modelo
   seq_state_list <- ats$seq_state_list
   set_state_list <- ats$set_state_list
   mset_state_list <- ats$mset_state_list

   for(i in 1:nrow(traces))
   {
      # busca eventos do caso / search events of the given trace
      traceEvents <- events[events$number == traces$number[i],]

      traceEvents$seq_state_id <- 0
      traceEvents$set_state_id <- 0
      traceEvents$mset_state_id <- 0
      traceEvents$sojourn_set_stc <- 0
      traceEvents$sojourn_mset_stc <- 0
      traceEvents$sojourn_seq_stc <- 0

      # label utilizado na funÃ§Ã£o de eventos (campos do modelo)
      activity_fields <- do.call(paste, as.data.frame(traceEvents[,ats$sel_attributes]))

      # tempo em que ocorreu o evento
      timestamp_field <-  as.vector(traceEvents[, EVENT_TIMESTAMP_COLUMN_NAME])

      # listas com os modelos de abstraÃ§Ã£o usados para representaÃ§Ã£o dos eventos
      seq_list <- list()
      set_list <- list()
      mset_list <- list()
      for (j in 1:nrow(traceEvents))
      {
         # calculate the horizon
         horiz_index <- max(j - ats$horiz + 1, 1)

         # generates state for SEQUENCE abstration
         inc_seq <- as.character(activity_fields[horiz_index:j])
         seq_list[[j]]  <- toString(inc_seq)

         # generates state for SET
         inc_set <- as.set(inc_seq)
         set_list[[j]]  <- toString(inc_set)

         # multi-set (requires package sets)
         inc_gset <- as.gset(inc_seq)
         inc_gset_str <- toString(rbind(gset_support(inc_gset), gset_memberships(inc_gset)))
         mset_list[[j]]  <- inc_gset_str

         #traceEvents[j,"set_state_id"] <- find_state_fn2(set_state_list, set_list[[j]], "set")
         stateId <- match(set_list[[j]], set_state_list, NIL_STATE)

         if (ats$horiz==1) # horizonte 1, todos iguais
         {
            traceEvents[j,c("set_state_id","mset_state_id","seq_state_id")] <- stateId
         }
         else
         {
            traceEvents[j,"set_state_id"] <- stateId
            # idem com modelo multiset
            #traceEvents[j,"mset_state_id"] <- find_state_fn2(mset_state_list, mset_list[[j]] , "mset")
            traceEvents[j,"mset_state_id"] <- match(mset_list[[j]], mset_state_list, NIL_STATE)
            # modelo de abstracao sequencia
            #traceEvents[j,"seq_state_id"] <- find_state_fn2(seq_state_list, seq_list[[j]], "sequence")
            traceEvents[j,"seq_state_id"] <- match(seq_list[[j]], seq_state_list, NIL_STATE)
         }
      }

      # Step 2, calculate the sojourn
      curr_sojourn_set_state <- traceEvents[1,"set_state_id"]
      curr_sojourn_mset_state <- traceEvents[1,"mset_state_id"]
      curr_sojourn_seq_state <- traceEvents[1,"seq_state_id"]
      curr_sojourn_set_stc <- 0
      curr_sojourn_mset_stc <- 0
      curr_sojourn_seq_stc <- 0
      # first event of each case, the sojourn will be always zero
      for (j in 2:nrow(traceEvents))
      {
         #print(paste("inicio loop 2: ",j))

         elapsed <- traceEvents[j,"elapsed_stc"]
         # advances until it reaches a distinct state - for SET
         if ( traceEvents[j,"set_state_id"] != curr_sojourn_set_state ) {
            curr_sojourn_set_state <- traceEvents[j,"set_state_id"]
            traceEvents[j,"sojourn_set_stc"] <- elapsed - curr_sojourn_set_stc
            curr_sojourn_set_stc <- elapsed
         }

         # now for MULTI-SET
         if ( traceEvents[j,"mset_state_id"] != curr_sojourn_mset_state ) {
            curr_sojourn_mset_state <- traceEvents[j,"mset_state_id"]
            traceEvents[j,"sojourn_mset_stc"] <- elapsed - curr_sojourn_mset_stc
            curr_sojourn_mset_stc <- elapsed
         }

         # and now for SEQUENCE
         if ( traceEvents[j,"seq_state_id"] != curr_sojourn_seq_state ) {
            curr_sojourn_seq_state <- traceEvents[j,"seq_state_id"]
            traceEvents[j,"sojourn_seq_stc"] <- elapsed - curr_sojourn_seq_stc
            curr_sojourn_seq_stc <- elapsed
         }
      } # fim j

      # armazena resultado das transiÃ§Ãµes de estado para instancia atual
      # modelo de abstraÃ§Ã£o sequencia
      traces_states$seq_states[i] <- list(traceEvents[,"seq_state_id"])

      # modelo de abstraÃ§Ã£o set
      traces_states$set_states[i] <- list(traceEvents[,"set_state_id"])

      # modelo de abstraÃ§Ã£o mset
      traces_states$mset_states[i] <- list(traceEvents[,"mset_state_id"])

      # guardo a estado no evento
      events[events$number == traces$number[i],]$seq_state_id <- traceEvents$seq_state_id
      events[events$number == traces$number[i],]$set_state_id <- traceEvents$set_state_id
      events[events$number == traces$number[i],]$mset_state_id <- traceEvents$mset_state_id
      events[events$number == traces$number[i],]$sojourn_set_stc <- traceEvents$sojourn_set_stc
      events[events$number == traces$number[i],]$sojourn_mset_stc <- traceEvents$sojourn_mset_stc
      events[events$number == traces$number[i],]$sojourn_seq_stc <- traceEvents$sojourn_seq_stc

   } # fim i

   # retorna resultado - precisa atualizar a lista de eventos original, para os cálculos
   eval.parent(substitute(aevents<-events))

   mta_model <- list(traces_states=traces_states,
                     seq_state_list=ats$seq_state_list,
                     set_state_list=ats$set_state_list,
                     mset_state_list=ats$mset_state_list,
                     horiz=ats$horiz,
                     sel_attributes=ats$sel_attributes
   )

   #generate_log("Finalizando build_prediction",2)

   return(mta_model)
}



#' Title
#'
#' @param lsel_traces_list
#'
#' @return
#' @export
#'
#' @examples
#eval_model_gen_fn <- function(lsel_traces_list)
eval_model_gen_fn <- function(events)
{
   summary_pred_stats <- NULL
   result <- NULL
#   for (sel_trace_ in lsel_traces_list)
   for (fold_events in events)
   {
      #incidentevtlog_anot<- as.data.frame(
      events_anot <- as.data.frame(
         fold_events[, c("number", "updated_at", "incident_state", "seq_state_id","set_state_id", "mset_state_id",
                        "sojourn_set_stc","sojourn_mset_stc","sojourn_seq_stc","elapsed_stc", "remaining_stc")]
      )

      # teste estatistica convertida # Alexandre: o que faz esse código aqui?
      events_anot$remaining_stc <- events_anot$remaining_stc

      # gerar as contagens e medias por estado
      # num_secs <- 1 * 60 * 60 # em horas
      # num_secs <- 1 # em segundos
      # inc.outlier <- T

      # Gera informaÃ§Ãµes de prediÃ§Ã£o por estado
      # TODO: Avaliar o calculo retirando os valores de outlier 1.5 * IQR
      # prediÃ§Ã£o no primeiro conjunto treinamento - demais validaÃ§Ã£o
      if (is.null(summary_pred_stats))
      {
         # filtrar os valores que sao estados finais pois distorcem a media
         incidentevtlog_anot_st <- events_anot[events_anot$remaining_stc > 0,]

         summary_set <- gen_summary_pred_fn(incidentevtlog_anot_st, 'set_state_id','remaining_stc')
         summary_mset <- gen_summary_pred_fn(incidentevtlog_anot_st, 'mset_state_id','remaining_stc')
         summary_seq <- gen_summary_pred_fn(incidentevtlog_anot_st, 'seq_state_id','remaining_stc')

         summary_sj_set <- gen_summary_pred_fn(incidentevtlog_anot_st, 'set_state_id','sojourn_set_stc')
         summary_sj_mset <- gen_summary_pred_fn(incidentevtlog_anot_st, 'mset_state_id','sojourn_mset_stc')
         summary_sj_seq <- gen_summary_pred_fn(incidentevtlog_anot_st, 'seq_state_id','sojourn_seq_stc')

         #armazena totais
         summary_pred_stats <- list(summary_set, summary_mset, summary_seq,
                                    summary_sj_set, summary_sj_mset, summary_sj_seq)
      }

      # atualiza predited values media, mediana e desvio padrão
      # set
      events_anot$remaining_stc_pset_mean <-
         summary_set$mean[match(events_anot$set_state_id, summary_set$set_state_id)] +
         summary_sj_set$mean[match(events_anot$set_state_id, summary_sj_set$set_state_id)] -
         events_anot$sojourn_set_stc
      events_anot$remaining_stc_pset_median <-
         summary_set$median[match(events_anot$set_state_id, summary_set$set_state_id)] +
         summary_sj_set$median[match(events_anot$set_state_id, summary_sj_set$set_state_id)] -
         events_anot$sojourn_set_stc
      events_anot$remaining_stc_pset_sd <-
         summary_set$sd[match(events_anot$set_state_id, summary_set$set_state_id)] +
         summary_sj_set$sd[match(events_anot$set_state_id, summary_sj_set$set_state_id)] -
         events_anot$sojourn_set_stc

      # multi set
      events_anot$remaining_stc_pmset_mean <-
         summary_mset$mean[match(events_anot$mset_state_id, summary_mset$mset_state_id)] +
         summary_sj_mset$mean[match(events_anot$mset_state_id, summary_sj_mset$mset_state_id)] -
         events_anot$sojourn_mset_stc
      events_anot$remaining_stc_pmset_median <-
         summary_mset$median[match(events_anot$mset_state_id, summary_mset$mset_state_id)] +
         summary_sj_mset$median[match(events_anot$mset_state_id, summary_sj_mset$mset_state_id)] -
         events_anot$sojourn_mset_stc
      events_anot$remaining_stc_pmset_sd <-
         summary_mset$sd[match(events_anot$mset_state_id, summary_mset$mset_state_id)] +
         summary_sj_mset$sd[match(events_anot$mset_state_id, summary_sj_mset$mset_state_id)] -
         events_anot$sojourn_mset_stc

      # sequence
      events_anot$remaining_stc_pseq_mean <-
         summary_seq$mean[match(events_anot$seq_state_id, summary_seq$seq_state_id)] +
         summary_sj_seq$mean[match(events_anot$seq_state_id, summary_sj_seq$seq_state_id)] -
         events_anot$sojourn_seq_stc
      events_anot$remaining_stc_pseq_median <-
         summary_seq$median[match(events_anot$seq_state_id, summary_seq$seq_state_id)] +
         summary_sj_seq$median[match(events_anot$seq_state_id, summary_sj_seq$seq_state_id)] -
         events_anot$sojourn_seq_stc
      events_anot$remaining_stc_pseq_sd <-
         summary_seq$sd[match(events_anot$seq_state_id, summary_seq$seq_state_id)] +
         summary_sj_seq$sd[match(events_anot$seq_state_id, summary_sj_seq$seq_state_id)] -
         events_anot$sojourn_seq_stc


      # remove valorers sem match para calculo erro
      incidentevtlog_anot_err <- na.omit(events_anot)
      # remove valores dos estados finais Target = 0 que distorcem a mÃ©dia
      # valores do ultimo estado serÃ£o sempre precisos
      incidentevtlog_anot_err <- incidentevtlog_anot_err[incidentevtlog_anot_err$remaining_stc > 0,]

      # calculo erro  MAPE e RMSPE todos os registros

      #MAPE(y_pred, y_true)
      mape_val <- c(
         MAPE(incidentevtlog_anot_err$remaining_stc_pset_mean, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pset_median, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pset_sd, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pmset_mean, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pmset_median, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pmset_sd, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pseq_mean, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pseq_median, incidentevtlog_anot_err$remaining_stc),
         MAPE(incidentevtlog_anot_err$remaining_stc_pseq_sd, incidentevtlog_anot_err$remaining_stc)
      )
      names(mape_val) <- c(
         "val_mape_pset_mean","val_mape_pset_median","val_mape_pset_sd",
         "val_mape_pmset_mean","val_mape_pmset_median","val_mape_pmset_sd",
         "val_mape_pseq_mean","val_mape_pseq_median","val_mape_pseq_sd"
      )
      mape_val

      #RMSPE(y_pred, y_true)
      rmspe_val <- c(
         RMSPE(incidentevtlog_anot_err$remaining_stc_pset_mean, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pset_median, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pset_sd, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pmset_mean, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pmset_median, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pmset_sd, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pseq_mean, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pseq_median, incidentevtlog_anot_err$remaining_stc),
         RMSPE(incidentevtlog_anot_err$remaining_stc_pseq_sd, incidentevtlog_anot_err$remaining_stc)
      )
      names(rmspe_val) <- c(
         "val_rmspe_pset_mean","val_rmspe_pset_median","val_rmspe_pset_sd",
         "val_rmspe_pmset_mean","val_rmspe_pmset_median","val_rmspe_pmset_sd",
         "val_rmspe_pseq_mean","val_rmspe_pseq_median","val_rmspe_pseq_sd"
      )
      rmspe_val

      #non fitting
      non_fit_arr <- c(
         nrow(fold_events),
         nrow(events_anot),
         nrow(events_anot[events_anot$set_state_id == NIL_STATE,]),
         nrow(events_anot[events_anot$mset_state_id == NIL_STATE,]),
         nrow(events_anot[events_anot$seq_state_id == NIL_STATE,]),
         length(unique(events_anot$set_state_id)),
         length(unique(events_anot$mset_state_id)),
         length(unique(events_anot$seq_state_id))
      )
      names(non_fit_arr) <- c("num_evt_tot","num_evt_ok","num_evt_nf_set",
                              "num_evt_nf_mset","num_evt_nf_seq", "num_set_states",
                              "num_mset_states", "num_seq_states")
      #print(non_fit_arr)
      non_fit_per_arr <- c(
         non_fit_arr[c("num_evt_nf_set")] / non_fit_arr[c("num_evt_ok")],
         non_fit_arr[c("num_evt_nf_mset")] / non_fit_arr[c("num_evt_ok")],
         non_fit_arr[c("num_evt_nf_seq")] / non_fit_arr[c("num_evt_ok")]
      )
      names(non_fit_per_arr) <- c("perr_nf_set","perr_nf_mset","perr_nf_seq")

      non_fit_per_arr <- non_fit_per_arr * 100

      # retorna o menor erro - media ou mediana
      perr_tot_arr <- c(
         min(mape_val[c("val_mape_pset_mean")], mape_val[c("val_mape_pset_median")]),
         min(mape_val[c("val_mape_pmset_mean")], mape_val[c("val_mape_pmset_median")]),
         min(mape_val[c("val_mape_pseq_mean")], mape_val[c("val_mape_pseq_median")])
      )
      names(perr_tot_arr) <- c(
         "perr_tot_set","perr_tot_mset","perr_tot_seq"
      )
      perr_tot_arr

      # filtro para eventos com fit
      incidentevtlog_anot_err_set1 <- incidentevtlog_anot_err[incidentevtlog_anot_err$set_state_id != NIL_STATE,]
      incidentevtlog_anot_err_mset1 <- incidentevtlog_anot_err[incidentevtlog_anot_err$mset_state_id != NIL_STATE,]
      incidentevtlog_anot_err_seq1 <- incidentevtlog_anot_err[incidentevtlog_anot_err$seq_state_id != NIL_STATE,]
      #MAPE(y_pred, y_true)
      mape_val1 <- c(
         MAPE(incidentevtlog_anot_err_set1$remaining_stc_pset_mean, incidentevtlog_anot_err_set1$remaining_stc),
         MAPE(incidentevtlog_anot_err_set1$remaining_stc_pset_median, incidentevtlog_anot_err_set1$remaining_stc),
         MAPE(incidentevtlog_anot_err_set1$remaining_stc_pset_sd, incidentevtlog_anot_err_set1$remaining_stc),
         MAPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_mean, incidentevtlog_anot_err_mset1$remaining_stc),
         MAPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_median, incidentevtlog_anot_err_mset1$remaining_stc),
         MAPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_sd, incidentevtlog_anot_err_mset1$remaining_stc),
         MAPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_mean, incidentevtlog_anot_err_seq1$remaining_stc),
         MAPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_median, incidentevtlog_anot_err_seq1$remaining_stc),
         MAPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_sd, incidentevtlog_anot_err_seq1$remaining_stc)
      )
      names(mape_val1) <- c(
         "val_mape_pset_mean1","val_mape_pset_median1","val_mape_pset_sd1",
         "val_mape_pmset_mean1","val_mape_pmset_median1","val_mape_pmset_sd1",
         "val_mape_pseq_mean1","val_mape_pseq_median1","val_mape_pseq_sd1"
      )
      mape_val1

      rmspe_val1 <- c(
         RMSPE(incidentevtlog_anot_err_set1$remaining_stc_pset_mean, incidentevtlog_anot_err_set1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_set1$remaining_stc_pset_median, incidentevtlog_anot_err_set1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_set1$remaining_stc_pset_sd, incidentevtlog_anot_err_set1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_mean, incidentevtlog_anot_err_mset1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_median, incidentevtlog_anot_err_mset1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_sd, incidentevtlog_anot_err_mset1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_mean, incidentevtlog_anot_err_seq1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_median, incidentevtlog_anot_err_seq1$remaining_stc),
         RMSPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_sd, incidentevtlog_anot_err_seq1$remaining_stc)
      )
      names(rmspe_val1) <- c(
         "val_rmspe_pset_mean1","val_rmspe_pset_median1","val_rmspe_pset_sd1",
         "val_rmspe_pmset_mean1","val_rmspe_pmset_median1","val_rmspe_pmset_sd1",
         "val_rmspe_pseq_mean1","val_rmspe_pseq_median1","val_rmspe_pseq_sd1"
      )
      rmspe_val1

      #non_fit_arr
      result <- rbind(
         result,
         c(mape_val, rmspe_val, non_fit_arr, non_fit_per_arr, perr_tot_arr,
           mape_val1, rmspe_val1)
      )
   }
   return(result)
}


#' Title
#'
#' @param data
#' @param groupvars
#' @param measurevar
#' @param na.rm
#' @param conf.interval
#' @param .drop
#'
#' @return
#' @export
#'
#' @examples
#'  summary_set <- gen_summary_pred_fn(incidentevtlog_anot_st, 'set_state_id','remaining_stc')

gen_summary_pred_fn <- function(data=NULL, groupvars=NULL, measurevar,  na.rm=TRUE,
                                conf.interval=.95, .drop=TRUE) {

   # New version of length which can handle NA's: if na.rm==T, don't count them
   length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
   }

   # This does the summary. For each group's data frame, return a vector with
   # N, mean, and sd
   datac <- ddply(data, groupvars, .drop=.drop,
                  .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       mean = ceiling(mean   (xx[[col]], na.rm=na.rm)),
                       sd   = ceiling(sd     (xx[[col]], na.rm=na.rm)),
                       median   = ceiling(median (xx[[col]], na.rm=na.rm)),
                       min   = ceiling(min     (xx[[col]], na.rm=na.rm)),
                       max   = ceiling(max     (xx[[col]], na.rm=na.rm))
                     )
                  },
                  measurevar
   )

   # Rename the "mean" column
   #datac <- rename(datac, c("mean" = measurevar))

   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

   # Confidence interval multiplier for standard error
   # Calculate t-statistic for confidence interval:
   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
   ciMult <- qt(conf.interval/2 + .5, datac$N-1)

   datac$ci <- datac$se * ciMult

   # registro para valores nÃ£o encontrados #non_fitting
   datac <- rbind(datac,
                  c(NIL_STATE, sum(datac$N),  mean(datac$mean),  sd(datac$mean),
                    median(datac$mean), min(datac$mean), max(datac$mean))
   )

   return(datac)
}





#' Title
#'
#' @param fold_events
#' @param resultFile
#' @param type
#' @param fold
#' @param horiz
#'
#' @return
#' @export
#'
#' @examples
#'
#' This function was extrated from the original function
#' #eval_model_gen_fn <- function(lsel_traces_list)
#' #eval_model_gen_fn <- function(events)
#'
annotate_model <- function(fold_events, resultFile, type, fold, horiz)
{
   summary_pred_stats <- NULL
   result <- NULL

   events_anot <- as.data.frame(
      fold_events[, c("number", "updated_at", "incident_state", "seq_state_id","set_state_id", "mset_state_id",
                      "sojourn_set_stc","sojourn_mset_stc","sojourn_seq_stc","elapsed_stc", "remaining_stc")]
   )

   # filtrar os valores que sao estados finais pois distorcem a media
   events_anot_filtered  <- events_anot[events_anot$remaining_stc > 0,]

   summary_set <- gen_summary_pred_fn(events_anot_filtered, 'set_state_id','remaining_stc')
   summary_mset <- gen_summary_pred_fn(events_anot_filtered, 'mset_state_id','remaining_stc')
   summary_seq <- gen_summary_pred_fn(events_anot_filtered, 'seq_state_id','remaining_stc')

   summary_sj_set <- gen_summary_pred_fn(events_anot_filtered, 'set_state_id','sojourn_set_stc')
   summary_sj_mset <- gen_summary_pred_fn(events_anot_filtered, 'mset_state_id','sojourn_mset_stc')
   summary_sj_seq <- gen_summary_pred_fn(events_anot_filtered, 'seq_state_id','sojourn_seq_stc')

   #armazena totais
   summary_pred_stats <- list(summary_set, summary_mset, summary_seq,
                              summary_sj_set, summary_sj_mset, summary_sj_seq)

   # atualiza predited values media, mediana e desvio padrão
   # set
   events_anot$remaining_stc_set_state_mean <-
      summary_set$mean[match(events_anot$set_state_id, summary_set$set_state_id)]
   events_anot$sojourn_set_state_mean <-
      summary_sj_set$mean[match(events_anot$set_state_id, summary_sj_set$set_state_id)]

   # prediction based in the mean and sojourn
   events_anot$remaining_stc_pset_mean <-
      events_anot$remaining_stc_set_state_mean +
      events_anot$sojourn_set_state_mean -
      events_anot$sojourn_set_stc

   # multi set
   events_anot$remaining_stc_mset_state_mean <-
      summary_mset$mean[match(events_anot$mset_state_id, summary_mset$mset_state_id)]
   events_anot$sojourn_mset_state_mean <-
      summary_sj_mset$mean[match(events_anot$mset_state_id, summary_sj_mset$mset_state_id)]

   # prediction based in the mean and sojourn
   events_anot$remaining_stc_pmset_mean <-
      events_anot$remaining_stc_mset_state_mean +
      events_anot$sojourn_mset_state_mean -
      events_anot$sojourn_mset_stc


   # sequence
   events_anot$remaining_stc_seq_state_mean <-
      summary_seq$mean[match(events_anot$seq_state_id, summary_seq$seq_state_id)]
   events_anot$sojourn_seq_state_mean <-
      summary_sj_seq$mean[match(events_anot$seq_state_id, summary_sj_seq$seq_state_id)]

   # prediction based in the mean and sojourn
   events_anot$remaining_stc_pseq_mean <-
      events_anot$remaining_stc_seq_state_mean +
      events_anot$sojourn_seq_state_mean -
      events_anot$sojourn_seq_stc



   # remove valorers sem match para calculo erro
   # funcao remove todas as linhas que tiverem alguma coluna missing (nula)
   events_anot_filtered <- na.omit(events_anot)
   # remove valores dos estados finais Target = 0 que distorcem a media
   # valores do ultimo estado serao sempre precisos
   events_anot_filtered <- events_anot_filtered[events_anot_filtered$remaining_stc > 0,]

   # calculo erro  MAPE p todos os registros

   #MAPE(y_pred, y_true)
   mape_val <- c(
      MAPE(events_anot_filtered$remaining_stc_pset_mean, events_anot_filtered$remaining_stc),
      MAPE(events_anot_filtered$remaining_stc_pmset_mean, events_anot_filtered$remaining_stc),
      MAPE(events_anot_filtered$remaining_stc_pseq_mean, events_anot_filtered$remaining_stc)
   )
   names(mape_val) <- c("val_mape_pset_mean","val_mape_pmset_mean","val_mape_pseq_mean")

   #non fitting
   non_fit_arr <- c(
      nrow(fold_events),
      nrow(events_anot),
      nrow(events_anot[events_anot$set_state_id == NIL_STATE,]),
      nrow(events_anot[events_anot$mset_state_id == NIL_STATE,]),
      nrow(events_anot[events_anot$seq_state_id == NIL_STATE,]),
      length(unique(events_anot$set_state_id)),
      length(unique(events_anot$mset_state_id)),
      length(unique(events_anot$seq_state_id))
   )
   names(non_fit_arr) <- c("num_evt_tot","num_evt_ok","num_evt_nf_set",
                           "num_evt_nf_mset","num_evt_nf_seq", "num_set_states",
                           "num_mset_states", "num_seq_states")
   non_fit_per_arr <- c(
      non_fit_arr[c("num_evt_nf_set")] / non_fit_arr[c("num_evt_ok")],
      non_fit_arr[c("num_evt_nf_mset")] / non_fit_arr[c("num_evt_ok")],
      non_fit_arr[c("num_evt_nf_seq")] / non_fit_arr[c("num_evt_ok")]
   )
   names(non_fit_per_arr) <- c("perr_nf_set","perr_nf_mset","perr_nf_seq")

   non_fit_per_arr <- non_fit_per_arr * 100

   perr_tot_arr <- c(mape_val[c("val_mape_pset_mean")],
                     mape_val[c("val_mape_pmset_mean")],
                     mape_val[c("val_mape_pseq_mean")])

   names(perr_tot_arr) <- c("perr_tot_set","perr_tot_mset","perr_tot_seq")

   # filtro para eventos com fit
   incidentevtlog_anot_err_set1 <- events_anot_filtered[events_anot_filtered$set_state_id != NIL_STATE,]
   incidentevtlog_anot_err_mset1 <- events_anot_filtered[events_anot_filtered$mset_state_id != NIL_STATE,]
   incidentevtlog_anot_err_seq1 <- events_anot_filtered[events_anot_filtered$seq_state_id != NIL_STATE,]

   mape_val1 <- c(
      MAPE(incidentevtlog_anot_err_set1$remaining_stc_pset_mean, incidentevtlog_anot_err_set1$remaining_stc),
      MAPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_mean, incidentevtlog_anot_err_mset1$remaining_stc),
      MAPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_mean, incidentevtlog_anot_err_seq1$remaining_stc)
   )
   names(mape_val1) <- c("val_mape_pset_mean1", "val_mape_pmset_mean1", "val_mape_pseq_mean1")

   # appends to the file that contains all the stats so the error can be recalculated later
   events_anot <- cbind(type,fold,horiz,events_anot)
   write.table(events_anot, file=resultFile, row.names=FALSE, col.names = TRUE, append=TRUE, sep=";", dec=",")

   # returns only the summarized results for the given fold and horizon
   result <- c(fold=type, horizon=horiz, mape_val, non_fit_arr, non_fit_per_arr, perr_tot_arr, mape_val1)

   return(result)

}

