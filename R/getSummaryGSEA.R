getSummaryGSEA <- function(){
    
    Variable <- as.data.frame(getClinicData_MultipleCases(getSummaryGSEAExists=1))
    ##remove empty cases
    Variable <- subset(Variable, !Variable[,1]=="")
    #tkwait.variable(ttClinData_cb)
    
    if(ncol(Variable)!=1){
        tkmessageBox(message="Select only ONE Variable that has 2 classes as OS_STATUS or DFS_STATUS", icon="warning" )
        stop("Select only ONE Variable that has 2 classes as OS_STATUS or DFS_STATUS")
    }
    
    FDR<-dialogSummary_GSEA(Variable)
    
    tab1 <-read.table( myGlobalEnv$location1, header=TRUE, sep="\t")
    
    cnames<- colnames(tab1)
    ##select only signicant pVal 0.01
    tab1 <-tab1[which(tab1$FDR.q.val<FDR),]
    ##Order Biological process by significant
    tab1 <- tab1[order(tab1$NES),]
    
    
    tab2 <-read.table(myGlobalEnv$location2, header=TRUE, sep="\t")
    
    ##select only signicant pVal 0.01
    tab2 <-tab2[which(tab2$FDR.q.val<FDR),]
    ##Order Biological process by significant
    tab2 <- tab2[order(tab2$NES),]
    
    
    if(nrow(tab1)== 0){
        tab1 <- t(cbind(rep(0,13)))
        rownames(tab1)[1] <- names(table(Variable[,1]))[1]
        print(paste(names(table(Variable[,1]))[1], "= 0 Genes Sets"))
    } else {
        print(paste(names(table(Variable[,1]))[1],"=", nrow(tab1), "Genes Sets"))
        rownames(tab1)[1] <- names(table(Variable[,1]))[1]
    }
    
    if(nrow(tab2)==0){
        tab2 <- t(cbind(rep(0,13)))
        rownames(tab2)[1] <- names(table(Variable[,1]))[2]
        print(paste(names(table(Variable[,1]))[2], "= 0 Genes Sets"))
    } else{
        print(paste(names(table(Variable[,1]))[2],"=", nrow(tab2), " Genes Sets"))
        rownames(tab2)[1] <- names(table(Variable[,1]))[2]
    }    
    table <- rbind.na(tab1, tab2)
    ## Theses 2 colums remain without name???
    colnames(table)[c(1,3)]<- c("GS", "SOURCE")
    title = paste("GSEA SUMMARY RESULTS REPORT: FDR.q.val < ", FDR)
    getInTable( table=table, title = title)
    
}