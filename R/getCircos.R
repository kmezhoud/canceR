#' get Circos Layout for selected studies and selected dimensions
#' @usage
#' getCircos(dimension)
#' @param dimension string (All,mRNA, CNA, Met,RPPA, miRNA, Mut) 
#' @return a plot with Circos style
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/Circos.rds", sep=""))
#' \dontrun{
#' getCircos(dimension ="All")
#' }
#' @import circlize
getCircos <- function(dimension){
    #library(circlize)
    circos.clear()
    if(length(myGlobalEnv$ListProfData$Expression)!=0){
        getCor_ExpCNAMet(myGlobalEnv$ListProfData$Expression, dimension = "Exp")
    }
    if(length(myGlobalEnv$ListProfData$CNA)!=0){
        getCor_ExpCNAMet(myGlobalEnv$ListProfData$CNA, dimension = "CNA")
    }
    
    if(length(myGlobalEnv$ListMetData$HM450)!=0){
        if (inherits(try(getCor_ExpCNAMet(myGlobalEnv$ListMetData$HM450, dimension = "MetHM450"), silent=TRUE),"try-error"))
        {
            msgNoHM27 <- paste("There is more than 2 empty matrices from HM450 Data. Skip HM450")
            tkmessageBox(message=msgNoHM27, icon="warning")
            
        } else{  
            print("Computing of Correlaton HM450 ...")
            getCor_ExpCNAMet(myGlobalEnv$ListMetData$HM450, dimension = "MetHM450")
        }
        
        
    }
    
    
    if(length(myGlobalEnv$ListMetData$HM27)!=0){
        if (inherits(try(getCor_ExpCNAMet(myGlobalEnv$ListMetData$HM27, dimension = "MetHM27"), silent=TRUE),"try-error"))
        {
            msgNoHM27 <- paste("There is more than 2 empty matrices from HM27 Data. Skip HM27")
            tkmessageBox(message=msgNoHM27, icon="warning")
            
        } else{  
            print("Computing of Correlaton HM27 ...")
            getCor_ExpCNAMet(myGlobalEnv$ListMetData$HM27, dimension = "MetHM27")
        }
        
        
    }
    
    if(length(myGlobalEnv$ListProfData$RPPA)!=0){
        getCor_ExpCNAMet(myGlobalEnv$ListProfData$RPPA, dimension = "RPPA")
    }
    if(length(myGlobalEnv$ListProfData$miRNA)!=0){
        getCor_ExpCNAMet(myGlobalEnv$ListProfData$miRNA, dimension = "miRNA")
    }
    #df1 = read.table("~/CGDS-R//Cor_Exp")
    #df2 = read.table("~/CGDS-R//Cor_CNA")
    
    if(dimension == "Exp"){
        #all_disease <- unique(myGlobalEnv$Cor_Exp[[2]])
        all_disease <- myGlobalEnv$checked_Studies
        #all_genes <- unique(myGlobalEnv$Cor_Exp[[1]])
        all_genes <- myGlobalEnv$GeneList
        n_gene <- length(all_genes)
        n_disease <- length(all_disease) - 1
        
        
    } else if(dimension=="MetHM450"){
        all_disease <- unique(myGlobalEnv$Cor_Met$HM450[[2]])
        all_genes <- unique(myGlobalEnv$Cor_Met$HM450[[1]])
        n_gene <- length(all_genes)
        n_disease <- length(all_disease) - 1
        
    } else if(dimension=="MetHM27") {
        all_disease <- unique(myGlobalEnv$Cor_Met$HM27[[2]])
        all_genes <- unique(myGlobalEnv$Cor_Met$HM27[[1]])
        n_gene <- length(all_genes)
        n_disease <- length(all_disease) - 1
        
    } else if (dimension=="CNA"){
        all_disease <- unique(myGlobalEnv$Cor_CNA[[2]])
        all_genes <- unique(myGlobalEnv$Cor_CNA[[1]])
        n_gene <- length(all_genes)
        n_disease <- length(all_disease) - 1
    } else if (dimension=="All"){
        all_disease <- unique(myGlobalEnv$Cor_Exp[[2]])
        #all_disease <- myGlobalEnv$checked_Studies
        all_genes <- unique(myGlobalEnv$Cor_Exp[[1]])
        #all_genes <- myGlobalEnv$GeneList
        n_gene <- length(all_genes)
        n_disease <- length(all_disease) - 1 
        
    }
    
    
    if(exists("GeneListMSigDB", envir = myGlobalEnv)){
        ## set color for Gene Sets
        gene_set <- myGlobalEnv$GeneListMSigDB[[1]]
        names(gene_set) <- myGlobalEnv$GeneListMSigDB[[2]]
        gene_set_col <- rand_color(length(unique(gene_set)))
        names(gene_set_col) <- unique(gene_set)
        
    }
    ## Dialog Option Circos
    dialogOptionCircos()
    
    
    circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 90)
    circos.initialize(factors = all_disease, xlim = c(0, n_gene))
    
    
    ## Disease/genes Track                         
    circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
        disease = get.cell.meta.data("sector.index")
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        circos.text(mean(xlim), min(ylim)+.2, disease, facing = "bending.inside" , cex= 0.6)
        
        ## Add Gene Symbol
        ## replace 1 by 0 from Data frames 
        if(length(nrow(myGlobalEnv$Freq_DfMutData))==1){
            myGlobalEnv$Freq_DfMutData[is.na(myGlobalEnv$Freq_DfMutData)] <- 0
        }
        if(length(nrow(myGlobalEnv$Cor_Exp))==1){
            myGlobalEnv$Cor_Exp[,c(-1,-2)][myGlobalEnv$Cor_Exp[,c(-1,-2)]==1] <- 0
        }
        if(length(nrow(myGlobalEnv$Cor_CNA))==1){
            myGlobalEnv$Cor_CNA[,c(-1,-2)][myGlobalEnv$Cor_CNA[,c(-1,-2)]==1] <- 0
        }
        if(length(nrow(myGlobalEnv$Cor_Met$HM450))==1){
            myGlobalEnv$Cor_Met$HM450[,c(-1,-2)][myGlobalEnv$Cor_Met$HM450[,c(-1,-2)]==1] <- 0
        }
        if(length(nrow(myGlobalEnv$Cor_Met$HM27))==1){
            myGlobalEnv$Cor_Met$HM27[,c(-1,-2)][myGlobalEnv$Cor_Met$HM27[,c(-1,-2)]==1] <- 0
        }
        if(length(nrow(myGlobalEnv$ListProfData$RPPA))==1){
            myGlobalEnv$RPPA[,c(-1,-2)][myGlobalEnv$RPPA[,c(-1,-2)]==1] <- 0
        }
        ## plot only gene with high Level > Threshold
        for(i in 1:n_gene){
                      
            
            if(myGlobalEnv$ReturnCBoxThrCircos[7] == 1&&max(myGlobalEnv$Freq_DfMutData[i,]) > myGlobalEnv$ReturnThreshCircos[7] ){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5, col="darkgoldenrod3")
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040")
            }else if(myGlobalEnv$ReturnCBoxThrCircos[1] == 1&&max(myGlobalEnv$Cor_Exp[i,c(-1,-2)])  > myGlobalEnv$ReturnThreshCircos[1]){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="red3")
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040")
            }else if(myGlobalEnv$ReturnCBoxThrCircos[1] == 1&&max(myGlobalEnv$Cor_Exp[i,c(-1,-2)])  < -(myGlobalEnv$ReturnThreshCircos[1])){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="blue")    
            }else if(myGlobalEnv$ReturnCBoxThrCircos[2] == 1&&max(myGlobalEnv$Cor_CNA[i,c(-1,-2)])  > myGlobalEnv$ReturnThreshCircos[2]){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5, col="green4")
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040")
                }else if(myGlobalEnv$ReturnCBoxThrCircos[3] == 1 && max(myGlobalEnv$Cor_Met$HM450[i,c(-1,-2)])  > myGlobalEnv$ReturnThreshCircos[3]){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="mediumorchid4")
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040") 
                }else if(myGlobalEnv$ReturnCBoxThrCircos[3] == 1 && max(myGlobalEnv$Cor_Met$HM450[i,c(-1,-2)])< -(myGlobalEnv$ReturnThreshCircos[3])){
                 circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="springgreen4")   
            }else if(myGlobalEnv$ReturnCBoxThrCircos[4] == 1&&max(myGlobalEnv$Cor_Met$HM27[i,c(-1,-2)])  > myGlobalEnv$ReturnThreshCircos[4] ){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5, col="mediumorchid4")
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040") 
                }else if(myGlobalEnv$ReturnCBoxThrCircos[4] == 1 && max(myGlobalEnv$Cor_Met$HM27[i,c(-1,-2)])< -(myGlobalEnv$ReturnThreshCircos[4])){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="springgreen4")   
            }else if(myGlobalEnv$ReturnCBoxThrCircos[5] == 1&& max(myGlobalEnv$Cor_RPPA[i,c(-1,-2)])  > myGlobalEnv$ReturnThreshCircos[5]){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="red3" )
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040")
                }else if (myGlobalEnv$ReturnCBoxThrCircos[5] == 1&& max(myGlobalEnv$Cor_RPPA[i,c(-1,-2)])  < -(myGlobalEnv$ReturnThreshCircos[5])){
                    circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5,col="blue" ) 
            }else if(myGlobalEnv$ReturnCBoxThrCircos[6] == 1&&max(myGlobalEnv$Cor_miRNA[i,c(-1,-2)])  > myGlobalEnv$ReturnThreshCircos[6]){
                circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5, col="red2")
                #circos.link("brca_tcga", i , "brca_tcga", 365,rou1=0.8,rou2=0.8, col = "#00000040")
                }else if(myGlobalEnv$ReturnCBoxThrCircos[6] == 1&&max(abs(myGlobalEnv$Cor_miRNA[i,c(-1,-2)]))  < -(myGlobalEnv$ReturnThreshCircos[6])){
                    circos.text(min(xlim)+i, max(ylim)+.5, all_genes[i], facing="clockwise", cex = 0.5, col="blue")
            }
        }
        
    }, track.height = 0.05, bg.border = NA)
    
    ## GetSet Track
    if(exists("GeneListMSigDB", envir = myGlobalEnv)){
        
        circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            disease = get.cell.meta.data("sector.index")
            # genes that belong to this disease
            genes = myGlobalEnv$Cor_Exp[myGlobalEnv$Cor_Exp[[2]] == disease, 1]
            ## rearrange factor geneList as in all_gene (No alphabetic)
            genes <- factor(genes, levels = all_genes)
            
            for(i in seq_along(genes)) {
                circos.rect(i-0.5, 0, i+0.5, 1, col = gene_set_col[gene_set[genes[i]]], border = NA)
            }
            
            circos.text(min(xlim)+15, max(ylim)+0.5, "Gene Sets", facing= "bending.inside" ,cex = 0.5, nice.facing=TRUE)
            
            legend(x=-1.1, y=-0.8, pch = 15, col = gene_set_col, legend = names(gene_set_col),bty = "n" ,y.intersp = 0.7,pt.cex = 1, cex=0.455)
            
        }, track.height = 0.02, bg.border = "black")
        
        
    }
    
    
    # Track for Gene Expression
    if(length(nrow(myGlobalEnv$Cor_Exp))==1 && ncol(myGlobalEnv$Cor_Exp) == n_disease +3&&myGlobalEnv$ReturnCBoxCircos[1]==1){
        
        print("Getting Track for Gene Expression...")
        
        circos.trackPlotRegion(ylim = c(0, n_disease), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            disease <- get.cell.meta.data("sector.index")
            mat <- as.matrix(myGlobalEnv$Cor_Exp[myGlobalEnv$Cor_Exp[[2]] == disease, 3:(n_disease+3)])
            mat <- mat[, -which(all_disease %in% disease)]
            corr_col_fun <- colorRamp2(c(-1,0 ,1), c("blue","white" ,"red3"))
            
            for(i in seq_len(n_gene)) {
                for(j in seq_len(n_disease)) {
                    circos.rect(i-1, j-1, i, j, col = corr_col_fun(mat[i, j]), border = NA)
                }
            }
            circos.text(min(xlim)+15, max(ylim), "mRNA Exp", facing= "bending.inside" ,cex = 0.5)
            
            legend(x=-1.11, y=1.1, title = "mRNA",pch = 15, col = corr_col_fun(seq(-1,1, by = 0.5)), legend = seq(-1, 1, by = 0.5),bty = "n",y.intersp=0.7,pt.cex = 1, cex=0.5)
            
            
        }, track.height = 0.1)
        
    }
    
    
    # Track for CNA
    if(length(nrow(myGlobalEnv$Cor_CNA))==1 && ncol(myGlobalEnv$Cor_CNA) == n_disease +3&&myGlobalEnv$ReturnCBoxCircos[2]==1){
        print("Getting Track for CNA...")
        
        
        circos.trackPlotRegion(ylim = c(0, n_disease), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            disease <- get.cell.meta.data("sector.index")
            mat <- as.matrix(myGlobalEnv$Cor_CNA[myGlobalEnv$Cor_CNA[[2]] == disease, 3:(n_disease+3)])
            mat <- mat[, -which(all_disease %in% disease)]
            corr_col_fun <- colorRamp2(c( 0.7,0.8 ,1), c("white","greenyellow" ,"green4"))
            
            for(i in seq_len(n_gene)) {
                for(j in seq_len(n_disease)) {
                    circos.rect(i-1, j-1, i, j, col = corr_col_fun(mat[i, j]), border = NA)
                }
            }
            circos.text(min(xlim)+15, max(ylim), "CNA", facing= "bending.inside" ,cex = 0.5)
            
            legend(x=0.6,y=1.1, title = "CNA",pch = 15, col = corr_col_fun(seq(0.8, 1, by = 0.1)), legend = seq(0.8, 1, by = 0.1),bty = "n", y.intersp=0.7, pt.cex = 1, cex=0.5)
        }, track.height = 0.1)
        
    }
    
    ##track for methylation HM450
    if(length(nrow(myGlobalEnv$Cor_Met$HM450))==1 && ncol(myGlobalEnv$Cor_Met$HM450) == n_disease +3&&myGlobalEnv$ReturnCBoxCircos[3]==1){
        
        print("getting Track for methylation HM450...")
        
        circos.trackPlotRegion(ylim = c(0, n_disease), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            disease = get.cell.meta.data("sector.index")
            mat = as.matrix(myGlobalEnv$Cor_Met$HM450[myGlobalEnv$Cor_Met$HM450[[2]] == disease, 3:(n_disease+3)])
            mat = mat[, -which(all_disease %in% disease)]
            corr_col_fun = colorRamp2(c( -1,0 ,1), c("springgreen4","white" ,"mediumorchid4"))
            
            for(i in seq_len(n_gene)) {
                for(j in seq_len(n_disease)) {
                    circos.rect(i-1, j-1, i, j, col = corr_col_fun(mat[i, j]), border = NA)
                }
            }
            circos.text(min(xlim)+15, max(ylim), "Met HM450", facing= "bending.inside" ,cex = 0.5)
            
            legend(x=-0.9,y=1.1, title = "Meth",pch = 15, col = corr_col_fun(seq(-1,1, by = 0.5)), legend = seq(-1, 1, by = 0.5),bty = "n" ,y.intersp=0.7,pt.cex = 1, cex=0.5)        
        }, track.height = 0.1)
        
    }
    
    ##track for methylation HM27
    if(length(nrow(myGlobalEnv$Cor_Met$HM27))==1 && ncol(myGlobalEnv$Cor_Met$HM27) == n_disease +3 && myGlobalEnv$ReturnCBoxCircos[4]==1){
        print("getting Track for methylation HM27...")
        
        circos.trackPlotRegion(ylim = c(0, n_disease), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            disease = get.cell.meta.data("sector.index")
            mat = as.matrix(myGlobalEnv$Cor_Met$HM27[myGlobalEnv$Cor_Met$HM27[[2]] == disease, 3:(n_disease+3)])
            mat = mat[, -which(all_disease %in% disease)]
            corr_col_fun = colorRamp2(c(-1,0 ,1), c("springgreen4","white" ,"mediumorchid4"))
            
            for(i in seq_len(n_gene)) {
                for(j in seq_len(n_disease)) {
                    circos.rect(i-1, j-1, i, j, col = corr_col_fun(mat[i, j]), border = NA)
                }
            }
            circos.text(min(xlim)+15, max(ylim), "Met HM27", facing= "bending.inside" ,cex = 0.5)
            
            legend(x=-0.9,y=1.1, title = "Meth",pch = 15, col = corr_col_fun(seq(-1,1, by = 0.5)), legend = seq(-1, 1, by = 0.5),bty = "n" ,y.intersp=0.7,pt.cex = 1, cex=0.5)        
        }, track.height = 0.1)
        
    }
    
    
    # Track for Reverse Phase Protein Affinity
    if(length(nrow(myGlobalEnv$Cor_RPPA))==1 && ncol(myGlobalEnv$Cor_RPPA) == n_disease +3 &&myGlobalEnv$ReturnCBoxCircos[5]==1){
        
        print("Getting Track for RPPA...")
        
        circos.trackPlotRegion(ylim = c(0, n_disease), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            disease <- get.cell.meta.data("sector.index")
            mat <- as.matrix(myGlobalEnv$Cor_RPPA[myGlobalEnv$Cor_Exp[[2]] == disease, 3:(n_disease+3)])
            mat <- mat[, -which(all_disease %in% disease)]
            corr_col_fun <- colorRamp2(c(-1,0 ,1), c("blue","white" ,"red3"))
            
            for(i in seq_len(n_gene)) {
                for(j in seq_len(n_disease)) {
                    circos.rect(i-1, j-1, i, j, col = corr_col_fun(mat[i, j]), border = NA)
                }
            }
            circos.text(min(xlim)+15, max(ylim), "RPPA", facing= "bending.inside" ,cex = 0.5)
            #legend("bottomleft", title = "RPPA",pch = 15, col = corr_col_fun(seq(-1,1, by = 0.5)), legend = seq(-1, 1, by = 0.5), pt.cex = 1, cex=0.6)        
            
        }, track.height = 0.1)
        
    }
    
    ## Track for miRNA
    
    
    ## track for Gene Mutation
    if(length(nrow(myGlobalEnv$Freq_DfMutData))==1 &&myGlobalEnv$ReturnCBoxCircos[7]==1){
        print("Getting Track for Gene Mutation Frequency...")
        
        mat <- as.matrix(myGlobalEnv$Freq_DfMutData)
        mat[is.na(mat)] <- 0
        corr_col_fun <- colorRamp2(c(median(mat),mean(mat),30,70,max(mat)), c("white","white","gold","darkgoldenrod2", "darkgoldenrod4"))
        
        circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
            xlim = get.cell.meta.data("xlim")
            ylim = get.cell.meta.data("ylim")
            
            disease <- get.cell.meta.data("sector.index")
            mut = mat[, disease]
            names(mut) = rownames(mat)
            
            mut[is.na(mut)] <- 0
            
            for(i in seq_len(n_gene)) {
                circos.rect(i-0.5, 0, i+0.5, 1, col = corr_col_fun(mut[i]), border = NA)
            }
            circos.text(min(xlim)+15, max(ylim), "Mut", facing= "bending.inside" ,cex = 0.5)
        }, track.height = 0.1)
        interval <- (max(mat)-20)%/%3 
        legend(x=0.8,y=1.1, title = "Mutation",pch = 15, col = corr_col_fun(seq(20, max(mat), by = interval)), legend = seq(20, max(mat), by =interval ),bty = "n", y.intersp = 0.7 ,pt.cex = 1, cex=0.5)
        #legend(x=-0.9,y=1.1, title = "Meth",pch = 15, col = corr_col_fun(seq(-1,1, by = 0.5)), legend = seq(-1, 1, by = 0.5),bty = "n" ,y.intersp=0.7,pt.cex = 1, cex=0.5)        
        
    }
    
    circos.clear()
}