#' Get gene correlation for multiple dimensions. 
#' @usage 
#' getCor_ExpCNAMet(ListMatrix, dimension)
#' 
#' @param ListMatrix  is a List of numeric matrices
#' @param dimension  Exp,CNA, Met , miRNA , RPPA
#' 
#' @return correlation matrix
#' @examples 
#' load(paste(path.package("canceR"),"/data/Circos.RData", sep=""))
#' \dontrun{
#' getListProfData()
#' getCor_ExpCNAMet(myGlobalEnv$ListProfData$Expression, dimension="mRNA")
#' head(myGlobalEnv$Cor_Exp)
#' }
#' @import plyr
#' 
getCor_ExpCNAMet <- function(ListMatrix, dimension){
    
    #library(plyr)
    
    ##Merge data frame by rownames keeping the order of the first one
    MergeRowNames <- function(x,n){
        
        keeping.order <- function(data, fn, ...) { 
            col <- ".sortColumn"
            data[,col] <- 1:nrow(data) 
            out <- fn(data, ...) 
            if (!col %in% colnames(out)) stop("Ordering column not preserved by function") 
            out <- out[order(out[,col]),] 
            out[,col] <- NULL 
            out 
        } 
        
        rownames(x) = make.names(x[,1], unique=TRUE)
        ## ordering genes in MutData as in GeneList
        
        df_GeneList <- as.data.frame(rep(t(myGlobalEnv$GeneList), n))
        #df_GeneList <- as.data.frame(myGlobalEnv$GeneList)
        rownames(df_GeneList) <- make.names(df_GeneList[,1], unique =TRUE)
        
        df_merge <- keeping.order(df_GeneList, merge, y=x, by = "row.names")
        x <- df_merge[,c(-1,-2)]
        return(x)
    }
    
#     
#     if(exists("Cor_Exp", envir = myGlobalEnv)){
#         rm(Cor_Exp, envir=myGlobalEnv)
#         
#     }    
#     if(exists("Cor_CNA", envir = myGlobalEnv)){
#         rm(Cor_CNA, envir=myGlobalEnv)
#         
#     }   
#     if(exists("Cor_Met", envir = myGlobalEnv)){
#         rm(Cor_Met, envir=myGlobalEnv)
#         
#     }
#     if(exists("Cor_RPPA", envir = myGlobalEnv)){
#         rm("Cor_RPPA", envir=myGlobalEnv)
#         
#     }
#     
    
    
    
    ## Rotate the cube of Matrices with 90°
    ## get the same column of a list of matrices
    getColumn <- function(x, colNum, len = nrow(x)){
        y <- x[,colNum]
        length(y) <- len
        y
    }
    
    ## get Matrices with each same column of all matrices 
    getMatrices <- function(colNums, dataList = x){
        # the number of rows required
        n <- max(sapply(dataList, nrow))
        lapply(colNums, function(x, dat, n) { # iterate along requested columns
            do.call(cbind, lapply(dat, getColumn,x, len=n)) # iterate along input data list
        }, dataList, n)
    }
    
    ## Define Lists
    
    #Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
    #ListProfData <- vector("list", Lchecked_GenProf)
    #ListProfData$Expression <- myGlobalEnv$ListProfData$Expression
    #myGlobalEnv$Cor_Met <- vector("list", 2)
    
    ## Rotate the list of Matrices by 90°
    ListMatrix90 <- getMatrices(c(1:ncol(ListMatrix[[1]])),dataList=ListMatrix)

    ## Replace NA by 0
    ListMatrix90 <- lapply(ListMatrix90, function(x) {x[is.na(x)] <- 0; x })
    
    if(dimension == "CNA"){
        # Ordering value
        ListMatrix90 <- lapply(ListMatrix90,function(x)apply(x, 2, function(x) x[order(x)]))
    }
    ## Convet the list of matrices to Array
    #ArrayMatrix90 <- array(unlist( ListMatrix90), dim = c(nrow( ListMatrix90[[1]]), ncol( ListMatrix90[[1]]), length( ListMatrix90)))
    #dimnames(ArrayMatrix90) <- list(1: dim(ArrayMatrix90)[1],names(ListProfData$Expression),colnames(ListProfData$Expression[[1]]) )
    
    #cor.balance(t(ListMatrix90[[1]][1:136,]),m=1,G=4)
    #cor(ListMatrix90[[1]], method="spearman" , use= "na")
    
    Cor_ListMatrix <- lapply(ListMatrix90,function(x) cor(x,use="na", method="spearman"))
    
    #lapply(Cor_ListProfData$Expression, function(x) x[abs(x)>0.8])
    
    ## convert the list of correlation matrices to Array
    Cor_ArrayMatrix <- array(unlist( Cor_ListMatrix), dim = c(nrow(Cor_ListMatrix[[1]]), ncol(Cor_ListMatrix[[1]]), length(Cor_ListMatrix)))
    dimnames(Cor_ArrayMatrix) <- list(names(ListMatrix), names(ListMatrix), colnames(ListMatrix[[1]]))
    
    #################
    #aaply(Cor_ArrayMatrix, c(3,1), function(x) x[myGlobalEnv$GeneList,,drop=FALSE])
    
    #ProfData[,(as.factor(myGlobalEnv$GeneList))][1,1:7]
    #ProfData[,(as.factor(myGlobalEnv$GeneList))][1,1:7]
    ################
    
    #DF1_CorMatrix <- adply(Cor_ArrayMatrix,1:2)
    
    ## Convert array to df by 3,1 dimensions
    Cor_DfMatrix <- plyr::adply(Cor_ArrayMatrix,c(3,1))
    Cor_DfMatrix[is.na(Cor_DfMatrix)]<-0
    
    ## keeping the order of geneList
    Cor_DfMatrix <-MergeRowNames(Cor_DfMatrix, length(myGlobalEnv$checked_Studies))

    if(dimension == "Exp"){
        myGlobalEnv$Cor_Exp <- Cor_DfMatrix        
    } else if(dimension == "MetHM450") {
        myGlobalEnv$Cor_Met$HM450 <- Cor_DfMatrix
    } else if (dimension=="MetHM27"){
        myGlobalEnv$Cor_Met$HM27 <- Cor_DfMatrix
    } else if(dimension == "CNA") {
        myGlobalEnv$Cor_CNA <- Cor_DfMatrix
    } else if(dimension == "RPPA") {
        myGlobalEnv$Cor_RPPA <- Cor_DfMatrix
        } else if(dimension == "miRNA"){
            myGlobalEnv$Cor_miRNA <- Cor_DfMatrix
        }
    
    #     ## aggregate only correlation bigger that threshol by rowname
    #     temp1 <-aggregate(temp[,c(-1,-2)], list(temp[,2]), function(x) sum(abs(x)>.2))
    #     rownames(temp1)<-temp1[,1]
    #     temp1 <- temp1[,-1]
    #     ## NA to 0
    #     temp1[is.na(temp1)]<-0
    
    ## get 0 to the dialonal
    #                     for(cn in intersect(rownames(temp1), colnames(temp1))) {
    #                         temp1[cn, cn] = 0
    #                     }
    #                     
    
    #     ###mapping genes by disease
    #     tempGene<-adply(Cor_ArrayMatrix,c(3))
    #     tempGene1 <-aggregate(tempGene[,-1], list(tempGene[,1]), function(x) sum(x>.2&& x<1,na.rm = TRUE))
    #     
    #     rownames(tempGene1) <- tempGene1[,1]
    #     tempGene1 <- tempGene1[-1]
    #     tempGene1[tempGene1[,3]==1,]
    #     
    #     tempGene1[tempGene1[,6]==1,]
    #     L <-apply(tempGene1,2,function(x) x[x==1] )
    
    #     library(circlize)
    #     chordDiagram(as.matrix(temp1),symmetric = TRUE, directional = TRUE,diffHeight = 0.06)
    #     #chordDiagram(cor(temp1),symmetric = TRUE, directional = TRUE,diffHeight = 0.06)
    #     circos.clear()
    
    #                     library(reshape2)
    #                     
    #                     DF2_CorMatrix <- melt(DF1_CorMatrix)
    #                     DF2_CorMatrix <- subset(DF2_CorMatrix, abs(value) <1)
    #                     DF2_CorMatrix <- subset(DF2_CorMatrix, abs(value) >0.2)
    #                     
    #FilterDiseaseCor <- apply(Cor_ArrayMatrix,MARGIN=c(1,2) ,function(x) x[abs(x)>0.7])
    
    
    
    ## Sum the significant correlated gene between studies and get matrix
    ## Studies/Studies/numberOfCorrelatedGenes
    
    #                     mat<-matrix(1:25,5)
    #                     pmean <- function(x,y) (x+y)/2
    #                     mat[] <- pmean(mat, matrix(mat, nrow(mat), byrow=TRUE))
    #                     rownames(mat) <- paste0("Cancer",1:5)
    #                     colnames(mat) <- paste0("Cancer",1:5)
    
    
    
    
}

