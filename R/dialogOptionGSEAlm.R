#' Dialogbox to select variables from Clinical data
#' @usage
#' dialogOptionGSEAlm(k,ClinicalData)
#' @param k  integer 1
#' @param ClinicalData dataframe with clinical variables
#'
#' @return permutaion value, p-value, coVariables
#' @export
#'
#' @examples
#' #data(ClinicalData)
#' \dontrun{
#' getOptionGSEAlm()
#' }
dialogOptionGSEAlm <- function(k, ClinicalData){
    Lchecked_Cases <- length(ENV$curselectCases)
    ttClinData_cb <- tktoplevel()
    #tkwm.geometry(ttClinData_cb,"180x250")

    tktitle(ttClinData_cb) <- paste(ENV$StudyRefCase[k],ENV$CasesStudies[ENV$curselectCases[k]+1], sep=" ")

    permutEntry <- tclVar(1000)
    rEntry  <- tclVar(.05)

    #tkgrid(tklabel(ttClinData_cb, text="Select CoVariables:"))
    frameCoVariables <- tkframe(ttClinData_cb,relief="groove",borderwidth=2)
    tkgrid(tklabel(frameCoVariables, text="Select Variables:"))
    tkgrid(frameCoVariables)
    tkgrid.configure(frameCoVariables, sticky="new")
    #font <- tkfont.create(size=12,weight="bold")
    #tkgrid(tklabel(frameCoVariables, text="Numerics OR Factors", font=font),columnspan=3)



    ####################
    tkwm.geometry(ttClinData_cb, "430x220")
    yscr1 <- tkscrollbar(ttClinData_cb, repeatinterval=2,
                         command=function(...)tkyview(ttc,...))
    xscr1 <- tkscrollbar(ttClinData_cb, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(ttc,...))
    ttc<-tklistbox(ttClinData_cb,height=20, width= 50 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")

    tkgrid(ttc,yscr1, columnspan=1)
    tkgrid.configure(yscr1,rowspan=20, columnspan=1,sticky="nsw")
    tkgrid(xscr1,columnspan=2)
    tkgrid.configure(xscr1,rowspan=2,columnspan=2,sticky="ew")

    #tkinsert(ttc,"end","All")
    cbIValue=0
    for(j in 1: length(names(ClinicalData))){
        tkinsert(ttc,"end",names(ClinicalData)[j])

    }



    #####################

#     tkgrid(frameCoVariables)
#     tkgrid.configure(frameCoVariables, sticky="new")
#
#     cbIValue=0
#     for(i in 1: length(names(ClinicalData))){
#
#         cbi <- paste ("cb", i, sep="")
#         cbi <- tkcheckbutton(frameCoVariables)
#         cbiValue <- paste("cb", i, "Value", sep="")
#         cbIValue[i] <- cbiValue
#         cbIValue[i] <- tclVar("0")
#
#         tkconfigure(cbi,variable=cbiValue)
#         labeli <- paste ("label", i , sep="")
#         labelI <- labeli
#         labelI <- tklabel(frameCoVariables,text= names(ClinicalData[i]))
#         tkgrid(labelI,cbi)
#     }
    OnOK <- function(){

        ######################

        ENV$curselect <- as.numeric(tkcurselection(ttc))+1
        ENV$ClinicalData <- ClinicalData[ENV$curselect-1]

        namesClinicalData <- paste("0",names(ClinicalData[ENV$curselect]), sep="+")
        ENV$class2 <- names(table(ClinicalData[ENV$curselect]))[2]
        coVariables <- names(ClinicalData[ENV$curselect])
        #######################


        ### Listen permutation Value
        ENV$permutVal <- as.numeric(tclvalue(permutEntry))

        ##Listen pValue
        ENV$seuilpVal <- as.numeric(tclvalue(rEntry))

        print(paste("permutation: ",ENV$permutVal, sep=" "))
        print (paste("p-Value: ", ENV$seuilpVal, sep=" "))


#         namesClinicalData<-0
#         for (i in 1: length(names(ClinicalData))){
#             cbiValue <- paste("cb", i, "Value", sep="")
#             cbIValue <- cbiValue
#             cbiVal <- paste("cb", i, "Val", sep="")
#             cbIVal<-cbiVal
#
#             cbIVal[i] <- tclvalue(cbIValue)
#
#             if (cbIVal[i]=="1"){
#
#                 namesClinicalData <- paste(namesClinicalData,names(ClinicalData[i]), sep="+")
#
#                 ENV$class2 <- names(table(ClinicalData[i]))[2]
#             }
#         }

        ENV$namesClinicalData <- namesClinicalData

        ### only for Survival plot
        ENV$variable <- as.formula(sprintf("Surv(OS_MONTHS,OS_STATUS)~%s", coVariables))

        #coVariables<-paste0(strsplit(namesClinicalData, '')[[1]][c(-1,-2)], collapse = '')
        coVariables <- sprintf("~%s", coVariables)
        ENV$coVariables <- as.formula(coVariables)
        print(paste("coVariables:", ENV$coVariables, sep=" ")[2])
        tkdestroy(ttClinData_cb)
    }

    framePermutation<- tkframe(ttClinData_cb,relief="groove",borderwidth=2)
    tkgrid(tklabel(framePermutation, text="Specify Permutation number:"))
    tkgrid(framePermutation)
    tkgrid.configure(framePermutation, sticky="new")
    tkgrid(tkentry(framePermutation,width=12,textvariable=permutEntry))



    frameTHRESHOLD<- tkframe(ttClinData_cb,relief="groove",borderwidth=2)
    tkgrid(tklabel(frameTHRESHOLD, text="Specify Threshold pValue:"))
    tkgrid(frameTHRESHOLD)
    tkgrid.configure(frameTHRESHOLD, sticky="new")

    tkgrid(tkscale(frameTHRESHOLD,from=0.01,to=0.1,showvalue=TRUE,
                   variable=rEntry,resolution=.005,orient='horiz'))

    OK.but <- tkbutton(ttClinData_cb,text="OK",command=OnOK)
    tkgrid(OK.but)

    tkwait.window(ttClinData_cb)

}