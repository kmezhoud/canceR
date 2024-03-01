#' Survival plot
#' @usage getSurvival(Coxph)
#' @param Coxph if Coxph = 0 : plot Kaplan-Meier curves else Coxph= 1 : plot Cox Proportional Hazard Model
#' @return Survival plot
#' @examples 
#' surv <- 11
#' \dontrun{
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/gbm_tcgaPlotTwoGenProf.rds", sep=""))
#' getSurvival(Coxph = 1)
#' }
#' @export
#' 
#' @importFrom survival survfit
#' @importFrom survival coxph 
#' @importFrom survival Surv
#' @importFrom survival cox.zph
#' 
getSurvival<- function(Coxph){
    #library(survival)
    
    ## verify checked cases
    if(!exists("curselectCases", envir = ENV) || length(ENV$curselectCases)!=1){
        msgNoCases <- "Select ONE Case"
        tkmessageBox(message=msgNoCases)
        stop(msgNoCases)
    }
    
    
    
    ## Correspond the name study to selectec case in multiple studies choice
    LengthCases <- 0
    
    for (s in 1:length(ENV$checked_Studies)){
        
        LengthCases <- LengthCases + ENV$LCases[s]+1
        
        if(ENV$curselectCases <= LengthCases){
            
            Case<- ENV$CasesRefStudies[ENV$curselectCases]
            ClinicData <- getClinicalData(ENV$cgds,Case)
            
            
            ##Verify if there are Age,OS_STATUS, OS_MONTHS, DFS_STATUS
            if(length(grep("^OS_STATUS$", names(ClinicData), ignore.case = TRUE))==1&&   
                   length(grep("^OS_MONTHS$", names(ClinicData), ignore.case = TRUE))==1 &&
                   length(grep("^DFS_STATUS$", names(ClinicData), ignore.case = TRUE))==1
            ){
                
                ClinicData[ClinicData== "DECEASED"] <- 1
                ClinicData[ClinicData== "Deceased"] <- 1
                ClinicData[ClinicData== "LIVING"] <- 0
                ClinicData[ClinicData== "Living"] <- 0
                ClinicData$OS_STATUS <- as.numeric(ClinicData$OS_STATUS)
                ENV$ClinicData <- ClinicData
                ## Subset plot space into 3
                #layout(matrix(c(1,1,2,3), 2, 2, byrow = FALSE))
                if(Coxph==0){
                    dialogOptionGSEAlm(s,ClinicData)
                    n<- length(names(table(ClinicData[ENV$curselect])))
                    #fit a Kaplan-Meier and plot it
                    par(mfrow=c(1,1))
                    fit <- survival::survfit(ENV$variable, type="kaplan-meier", conf.type="log",data=ClinicData)
                    plot(fit,pch=20,conf.int = FALSE, lty=2:3,lwd=2 ,xlab="OS_MONTHS", ylab="OS_STATUS: Survival", main=ENV$StudyChoice[s], cex.main = 0.8, cex.lab= 0.7, cex.axis= 0.7, col=1:n)
                    legend("topright", c(names(table(ClinicData[ENV$curselect]))), col=1:n,lty = 2:3, cex=0.7, bty = 'n')
                    #                 ## 45, 1.09
                    ############
                    #                 plotCommand<- function(){
                    #                     #fit a Kaplan-Meier and plot it
                    #                     par(mfrow=c(1,1))
                    #                     fit <- survfit(Surv(OS_MONTHS,OS_STATUS)~DFS_STATUS, data=ClinicData)
                    #                     
                    # #                     if (inherits(try(plot(fit,pch=20,lty=2:3, xlab="OS_MONTHS", ylab="OS_STATUS: Survival", main=ENV$StudyChoice[s], cex.main = 0.8, cex.lab= 0.7, cex.axis= 0.7  ), silent=TRUE),"try-error"))
                    # #                     {
                    # #                         msgBadMARGIN <- paste(" Error in plot.new() : figure margins too large ")
                    # #                         tkmessageBox(message=msgBadMARGIN, icon="warning")
                    # #                         stop(msgBadMARGIN)
                    # #                     } else{         
                    #                         plot(fit,pch=20,lty=2:3, xlab="OS_MONTHS", ylab="OS_STATUS: Survival", main=ENV$StudyChoice[s], cex.main = 0.8, cex.lab= 0.7, cex.axis= 0.7  )
                    #                         legend(45, 1.09, c("DiseaseFree", "Recurred/Progressed"), lty = 2:3, cex=0.7, bty = 'n')
                    #                         
                    #                #     }
                    #                 }
                    #                 Title<- paste (ENV$StudyRefCase,"Kaplan-Meier fit", sep=": ")
                    #                 plotModel(plotCommand, title=Title,hscale=1 vscale=1)
                    #                 #plotCommand(s)
                    #                 #######
                    
                    
                    
                }else if(Coxph==1){
                    if(exists("age", envir=ENV)){
                        rm(age, envir = ENV)
                    }
                    dialogOptionGSEAlm(s,ClinicData)
                    n<- length(names(table(ClinicData[ENV$curselect])))
                    
                    #if(length(grep("^AGE$", names(ClinicData),ignore.case = TRUE))==1){
                        
                        ttAge <- tktoplevel()
                        tktitle(ttAge) <- "Choose numeric variable"
                        Age_Entry  = tclVar(median(ClinicData[,ENV$curselect], na.rm=TRUE))
                        ScaleAge <- tkscale(ttAge,length=200,from=min(as.numeric(ClinicData[,ENV$curselect]), na.rm=TRUE),to=max(as.numeric(ClinicData[,ENV$curselect]), na.rm = TRUE),showvalue=TRUE,
                                            variable=Age_Entry,resolution=1,orient='horiz')
                        tkgrid(ScaleAge)
                        
                        OkOn<-function(){
                            
                            #Entry Threshold of miRNA correlation
                            ENV$age <- as.numeric(tclvalue(Age_Entry))
                            tkdestroy(ttAge) 
                            
                        }
                        
                        Ok.but <- tkbutton(ttAge, text= "OK", command= OkOn)
                        
                        tkgrid(Ok.but)
                        tkwait.window(ttAge)
                        #tkgrid.configure(Ok.but, sticky="n", column=0)
                
                        #fit a Cox proportional hazards model and plot the  
                        #predicted survival for a 60 year old 
                        par(mfrow=c(1,2))
                        fit<- survival::coxph(ENV$variable, data= ENV$ClinicData, method="breslow", na.action=na.exclude)
                    
                    #ClinicData <- ENV$ClinicData
                        #title <- paste("Cox: Predicted survival for a",ENV$age, names(ClinicData[ENV$curselect]))
                        plot(survfit(fit, newdata=data.frame(ClinicData[ENV$curselect]== ENV$age)),xlab = "OS_MONTHS", ylab="OS_STATUS: Survival", main=ENV$StudyChoice[s], cex=1, col=1:n, cex.main = 0.8, cex.lab= 0.7, cex.axis= 0.7) 
                        
                        #legend("bottomleft", c(names(table(ClinicData[ENV$curselect]))),col=1:n, lty = 2:3, cex=0.7, bty = 'n')
                        text(60,1.0,paste("Cox: Predicted survival for a",ENV$age, names(ClinicData[ENV$curselect])), cex=0.65)
                        
                        res <- survival::cox.zph(fit)
                        ##capture results 
                        resCap <- capture.output(print(res))
                        plot(res,main= resCap, cex.main = 0.8, cex.lab= 0.7, cex=0.6,cex.axis= 0.7 ) # plot curves
                        #text(0, 0,main= resCap, cex=0.7)
#                     }else{
#                         msgNoAGE <- "There is no AGE data"
#                         tkmessageBox(message=msgNoAGE, icon="warning")
#                         stop(msgNoAGE)
#                     } 
                    #tkwait.window(ttAge)
                }
                
            }else{
                msgNoData <- "There is no \n OS_STATUT or OS_MONTHS or DFS_STATUS data"
                tkmessageBox(message=msgNoData, icon="warning")
                stop(msgNoData)
            }
        }
    }
}