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

#' funcao de construcao do MTA, recebe o trace, lista de instancias
#' C�digo fonte original
#'
#' @param events : list of events
#' @param horiz : the horizon
#' @param sel_attributes : list of selected attributes
#'
#' @return
#' @export
#'
#' @examples
build_ats <- function(events, horiz, sel_attributes)
{

   generate_log(paste("Starting build_ats for horizon",horiz,"and attributes [",paste( unlist(sel_attributes), collapse=', '),"]"),2)

   time0<-as.numeric(Sys.time())

   traces <- distinct_(events, TRACE_ID_COLUMN_NAME)

   process_instances_states <-list(
      number=traces,
      set_states=list(),
      mset_states=list(),
      seq_states=list()
   )

   # listas com os estados de cada modelo
   seq_state_list <- list()
   set_state_list <- list()
   mset_state_list <- list()


   for(i in 1:length(traces))
   {
      # busca eventos do caso / search events of the given trace
      #i <- 1
      traceEvents <- events[events$number == traces$number[i],]

      # it should be all initialized by this time
      # traceEvents$seq_state_id <- 0
      # traceEvents$set_state_id <- 0
      # traceEvents$mset_state_id <- 0
      # traceEvents$sojourn_set_stc <- 0
      # traceEvents$sojourn_mset_stc <- 0
      # traceEvents$sojourn_seq_stc <- 0

      activity_fields <- do.call(paste, as.data.frame(traceEvents[,sel_attributes]))

      # tempo em que ocorreu o evento
      timestamp_field <-  as.vector(traceEvents[, EVENT_TIMESTAMP_COLUMN_NAME])
      #timestamp_field <-  as.vector(trace[,c("sys_updated_on")])
      # listas com os modelos de abstração usados para representação dos eventos
      seq_list <- list()
      set_list <- list()
      mset_list <- list()
      for (j in 1:nrow(traceEvents))
      {
         # calculo do horizonte
         horiz_index <- max(j - horiz + 1, 1)

         inc_seq <- as.character(activity_fields[horiz_index:j])

         # gera abstrações
         # sequence
         #print(inc_seq)

         #seq <- activity_fields[,1][1:j]
         #cat(j, " - " , seq, "\n")
         #seq_list[[j]] <- inc_seq
         seq_list[[j]]  <- toString(inc_seq)

         # set
         #set <- as.character(as.set(seq))
         inc_set <- as.set(inc_seq)
         #cat(j, " - " , set, "\n")
         #set_list[[j]]  <- inc_set
         set_list[[j]]  <- toString(inc_set)

         # multi-set
         #mset_list[j] <- list(as.gset(seq))
         inc_gset <- as.gset(inc_seq)

         inc_gset_str <- toString(
            rbind(gset_support(inc_gset), gset_memberships(inc_gset))
         )

         #mset_list[[j]]  <- inc_gset
         mset_list[[j]]  <- inc_gset_str

         # chama função para avaliar os estados e gerar o estado atual
         # argumentos: lista de estados atual
         #             trace dos eventos anteriores + evento atual
         # retorna:    novo estado
         # modelo de abstração sequencia
         traceEvents[j,"set_state_id"] <- state_fn2(set_state_list, set_list[[j]], "set")

         if (horiz==1) # horizonte 1, todos iguais
         {
            # idem com modelo multiset (ainda não concluido)
            traceEvents[j,"mset_state_id"] <- traceEvents[j,"set_state_id"]
            # modelo de abstração sequencia
            traceEvents[j,"seq_state_id"] <- traceEvents[j,"set_state_id"]
         }
         else
         {
            # idem com modelo multiset
            traceEvents[j,"mset_state_id"] <- state_fn2(mset_state_list, mset_list[[j]] , "mset")
            # modelo de abstração sequencia
            traceEvents[j,"seq_state_id"] <- state_fn2(seq_state_list, seq_list[[j]], "sequence")
         }

         if (j==1) # primeiro registro guarda elapsed quando entrou no estado
         {
            curr_sojourn_set_stc <- traceEvents[j,"elapsed_stc"]
            curr_sojourn_mset_stc <- traceEvents[j,"elapsed_stc"]
            curr_sojourn_seq_stc <- traceEvents[j,"elapsed_stc"]
            curr_sojourn_set_state <- traceEvents[j,"set_state_id"]
            curr_sojourn_mset_state <- traceEvents[j,"mset_state_id"]
            curr_sojourn_seq_state <- traceEvents[j,"seq_state_id"]
         }
         else
         { # set
            if (curr_sojourn_set_state == traceEvents[j,"set_state_id"])
               traceEvents[j,"sojourn_set_stc"] <- traceEvents[j,"elapsed_stc"] - curr_sojourn_set_stc
            else
            {
               curr_sojourn_set_stc <- traceEvents[j,"elapsed_stc"]
               curr_sojourn_set_state <- traceEvents[j,"set_state_id"]
            }
            # mset
            if (curr_sojourn_mset_state == traceEvents[j,"mset_state_id"])
               traceEvents[j,"sojourn_mset_stc"] <- traceEvents[j,"elapsed_stc"] - curr_sojourn_mset_stc
            else
            {
               curr_sojourn_mset_stc <- traceEvents[j,"elapsed_stc"]
               curr_sojourn_mset_state <- traceEvents[j,"mset_state_id"]
            }
            #seq
            if (curr_sojourn_seq_state == traceEvents[j,"seq_state_id"])
               traceEvents[j,"sojourn_seq_stc"] <- traceEvents[j,"elapsed_stc"] - curr_sojourn_seq_stc
            else
            {
               curr_sojourn_seq_stc <- traceEvents[j,"elapsed_stc"]
               curr_sojourn_seq_state <- traceEvents[j,"seq_state_id"]
            }
         }

      } # fim j
      # armazena resultado das transições de estado para instancia atual
      # modelo de abstração sequencia
      process_instances_states$seq_states[i] <- list(traceEvents[,"seq_state_id"])

      # modelo de abstração set
      process_instances_states$set_states[i] <- list(traceEvents[,"set_state_id"])

      # modelo de abstração mset
      process_instances_states$mset_states[i] <- list(traceEvents[,"mset_state_id"])

      # guardo a estado no evento
      events[events$number == traces$number[i],]$seq_state_id <- traceEvents$seq_state_id
      events[events$number == traces$number[i],]$set_state_id <- traceEvents$set_state_id
      events[events$number == traces$number[i],]$mset_state_id <- traceEvents$mset_state_id
      events[events$number == traces$number[i],]$sojourn_set_stc <- traceEvents$sojourn_set_stc
      events[events$number == traces$number[i],]$sojourn_mset_stc <- traceEvents$sojourn_mset_stc
      events[events$number == traces$number[i],]$sojourn_seq_stc <- traceEvents$sojourn_seq_stc

      #print(trace)
   } # fim i
   #i <- 4
   # tempos de execução
   time1<-as.numeric(Sys.time())
   timetot.2var <- time1-time0
   #print(timetot.2var/60)
   #print(sel_traces_lcl)

   # retorna resultado
   #eval.parent(substitute(asel_traces_lcl<-sel_traces_lcl))
   mta_model <- list(process_instances_states=process_instances_states,
                     seq_state_list=seq_state_list,
                     set_state_list=set_state_list,
                     mset_state_list=mset_state_list,
                     horiz=horiz,
                     sel_attributes=sel_attributes,
                     lcl_timestamp_fld=EVENT_TIMESTAMP_COLUMN_NAME
   )

   #generate_log("Finalizando build_ats",2)

   return(mta_model)

}
