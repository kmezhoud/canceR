# getGenProfs <- function(){
#     
#     #     require(tcltk)
#     #     require(cgdsr)
#     #source("loadAllStudies.R")
#     #source("loadMatchStudies.R")
#     #source("getProfilesData.R")
#     
#     
#     #   tg<-tktoplevel()
#     #   #tkwm.geometry(tg, "580x400")
#     #   tkwm.title(tg,"The list of Genetic Profiles")
#     #   yscr <- tkscrollbar(tg, repeatinterval=5,
#     #                       command=function(...)tkyview(tl,...))
#     #   xscr <- tkscrollbar(tg, repeatinterval=5,orient="horizontal",
#     #                       command=function(...)tkxview(tl,...))
#     #   
#     #   tl<-tklistbox(tg,height=16, width= 68 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),background="white")
#     #   tkgrid(tklabel(tg,text= "The list of Genetic Profiles in selected Studies."))
#     #   
#     #   tkgrid(tl,yscr)
#     #   tkgrid.configure(yscr,rowspan=20,sticky="nsw")
#     #   tkgrid(xscr)
#     #   tkgrid.configure(xscr,rowspan=1,sticky="ew")
#     #   ##listBox Information of selected Genetic Profile
#     #   xscrInfo <- tkscrollbar(tg, repeatinterval=1,orient="horizontal",
#     #                           command=function(...)tkxview(tInfoG,...))
#     #   
#     #   tInfoG<-tklistbox(tg,height=1, width= 68,selectmode="multiple",xscrollcommand=function(...)tkset(xscrInfo,...),background="white")
#     #   
#     
#     
#     
#     ###Get Studies
#     #cgds=CGDS("http://www.cbioportal.org/public-portal/")
#     #Studies = getCancerStudies(cgds)[,2]
#     
#     #get Study Index 
#     StudiesRef <- getCancerStudies(myGlobalEnv$cgds)[,1]
#     #checked_StudyIndex
#     
#     ## and we need the cases list of every study
#     myGlobalEnv$checked_Studies_forGenProf <- myGlobalEnv$checked_Studies
#     myGlobalEnv$lchecked_Studies_forGenProf <- length(myGlobalEnv$checked_Studies_forGenProf)
#     
#     #Verify if the checked studies are the same to get Cases and Genetic profil
#     # compare checked_Studies_forCases == checked_Studies_forGenProf
#     
#     #   if(exists("checked_Studies_forCases") &&(checked_Studies_forCases != checked_Studies_forGenProf))
#     #   {
#     #     msgNOST_Case_GeneProf1="\nYou need to select the same list of Studies which selected to get the Case:\n "
#     #     msgNOSt_Case_GeneProf2=paste(msgNOST_Case_GeneProf1,getCancerStudies(cgds)[checked_StudyIndex_forCases,2], sep="\n")
#     #     tkmessageBox(message=msgNOSt_Case_GeneProf2)
#     #     tkdestroy(tg)
#     #   } else
#     
#     
#     
#     police <- tkfont.create(family="arial", size=11)
#     tkconfigure(tl, foreground="black", font=police)
#     
#     
#     GenProfsStudies = 0
#     GenProfsRefStudies =0
#     LGenProfs=0
#     for (i in 1:(myGlobalEnv$lchecked_Studies_forGenProf)){
#         
#         Si = myGlobalEnv$checked_StudyIndex[i]
#         #tkinsert(tl, "end",paste("                                        ","--------------------","Study" ,i,"--------------------", sep= " " ))
#         tkinsert(tl,"end",paste("***** Study ", Si ," : ", myGlobalEnv$Studies[Si],"******"))
#         #tkinsert(tl,"end", paste("\n index : Reference : Description"))
#         LGenProfs[i]<- length(getGeneticProfiles.CGDS(myGlobalEnv$cgds, StudiesRef[Si])[,1])
#         #LGenProfs <<- append(LGenProfs,  LGenProfs[i])
#         #LGenProfs <<- head(LGenProfs)
#         myGlobalEnv$LGenProfs[i] <- LGenProfs[i]
#         print(paste("There are:",myGlobalEnv$LGenProfs[i],"Genetics Profiles in", myGlobalEnv$Studies[Si],sep=" "))
#         
#         # create progress bar
#         progressBar_GenProfs <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
#                                               max = LGenProfs[i], width = 400)
#         
#         
#         GenProfRefStudy =0    
#         GenProfsStudy = 0
#         GenProfsRefStudy =0  
#         j=0
#         for (j in 1:LGenProfs[i]){
#             
#             Sys.sleep(0.1)
#             setTkProgressBar(progressBar_GenProfs, j, 
#                              label=paste( round(j/LGenProfs[i]*100, 0),"% of Genetic Profiles"))
#             
#             
#             #tkfocus(progressBar_GenProfs)
#             GenProfStudy <- getGeneticProfiles.CGDS(myGlobalEnv$cgds, myGlobalEnv$checked_Studies_forGenProf[i])[,2][j]
#             GenProfRefStudy <- getGeneticProfiles.CGDS(myGlobalEnv$cgds, myGlobalEnv$checked_Studies_forGenProf[i])[,1][j]
#             #tkinsert(tl,"end",c(j,":",GenProfRefStudy,":" ,GenProfStudy))
#             tkinsert(tl,"end",paste(j,":",GenProfStudy))
#             GenProfsStudy <- cbind(GenProfsStudy, GenProfStudy)
#             GenProfsRefStudy <- cbind(GenProfsRefStudy, GenProfRefStudy)
#         }
#         close(progressBar_GenProfs)
#         
#         GenProfsStudies <- cbind(GenProfsStudies, GenProfsStudy)
#         GenProfsRefStudies <- cbind(GenProfsRefStudies, GenProfsRefStudy)
#     }
#     
#     #GenProfsStudy = GenProfsStudy[-1]
#     #GenProfsStudy <<- GenProfsStudy
#     GenProfsRefStudies <- GenProfsRefStudies[-1]
#     myGlobalEnv$GenProfsRefStudies <- GenProfsRefStudies
#     myGlobalEnv$GenProfsStudies <-GenProfsStudies
#     
#     ##Count the nomber of Studies and return the number.
#     #nbrStudiesGenProf <- paste(lchecked_Studies_forGenProf, " Studies were selecded. Select Genetic Profil or more to get features.\nDO NOT RESELECT STUDIES")
#     #tkgrid(tklabel(tg,text= nbrStudiesGenProf ))
#     
#     # Default selection.  Indexing starts at zero.
#     #tkselection.set(tl,2)  
#     
#     
#     
#     #   loadSelectedGenProfs <- function(){
#     #     
#     #     curselectGenProfs = as.numeric(tkcurselection(tl))+1
#     #     lcurselectGenProfs = length(curselectGenProfs)
#     #     
#     #     GenProfChoice <<- GenProfsStudies[curselectGenProfs+1]
#     #     #GenProfIndex <- as.numeric(tkcurselection(tl))+2
#     #     
#     #     if (lcurselectGenProfs ==0){
#     #       msgSelectCase="Select at least one Genetic Profil"; tkmessageBox(message=msgSelectCase)
#     #     } else {
#     #     
#     #     ## loop to correspond each case to its study
#     #     StudyRefGenProf = 0
#     #     for( k in 1: lcurselectGenProfs){
#     #       h=1
#     #       while (curselectGenProfs[k] >  LGenProfs[h]+1){
#     #         
#     #         curselectGenProfs[k] = curselectGenProfs[k] - LGenProfs[h] - 1
#     #         h= h+1
#     #       }  
#     #       StudyRefGenProf[k] =  checked_Studies_forGenProf[h]
#     #     }
#     #     
#     #     #redefine curselectGenProfs
#     #     curselectGenProfs <<- as.numeric(tkcurselection(tl))+1
#     #     lcurselectGenProfs = length(curselectGenProfs)
#     #     
#     #     ##loop converting curselectGenProfs value of Genetic profile to index value of the same genetic profile in cgds
#     #     
#     #     for( j in 1: lcurselectGenProfs){
#     #       for (i in 1:lchecked_Studies_forGenProf){
#     #         if (curselectGenProfs[j] > LGenProfs[i]+1){
#     #           curselectGenProfs[j] = curselectGenProfs[j] - LGenProfs[i] - 1
#     #         } else if (curselectGenProfs[j] < LGenProfs[i]+1){
#     #           curselectGenProfs[j] = curselectGenProfs[j] - 1
#     #           break
#     #         } else if (curselectGenProfs[j]== LGenProfs[i]+1){
#     #           curselectGenProfs[j] = LGenProfs[i]
#     #           break
#     #         } else if (curselectGenProfs[j] == 0){
#     #           curselectGenProfs[j] = LGenProfs[i]
#     #           break
#     #         }
#     #         
#     #       }
#     #       
#     #     }
#     #     curselectGenProfs_forStudy <<- curselectGenProfs
#     #     ###output: Selected Study
#     #     #msgSelectGenProfs <- paste("Study: ", StudyRefGenProf ,"Gen. Prof.: ",GenProfChoice," Index: ",curselectGenProfs,sep=" ")
#     #     #tkmessageBox(message=msgSelectGenProfs)
#     #     #msgSelectGenProfsRef =paste(GenProfsRefStudies[as.numeric(tkcurselection(tl))+1])
#     #     #tkmessageBox(message=msgSelectGenProfsRef)
#     #     #print(StudyRefGenProf);print(GenProfChoice); print(curselectGenProfs)
#     #     #tkfocus(tg)
#     #     msgSelectedGenProfs= paste("The follwing Genetic Profiles are loaded:\n", GenProfChoice,"\n\n Load Cases if not yet...")
#     #     tkmessageBox(message= msgSelectedGenProfs)
#     #     
#     #     
#     #     tkdelete(tInfoG,0)
#     #     tkinsert(tInfoG,"end",GenProfChoice)
#     #     }
#     #   }
#     
#     
#     
#     #   LoadGenProf.but <-tkbutton(tg,text="   Load selected Genetic Profiles   ",command= loadSelectedGenProfs)
#     #   tkgrid(LoadGenProf.but)
#     #   tkfocus(tg)
#     #   tkgrid(tInfoG)
#     #   tkgrid(xscrInfo)
#     #   tkgrid.configure(xscrInfo,rowspan=1,sticky="ew")
#     
#     #   OnCancel <- function(){
#     #     
#     #     tkdestroy(tg)
#     #   }
#     #   
#     #   Cancel.but <- tkbutton(tg, text="Cancel", command=OnCancel )
#     #   tkgrid(Cancel.but)
#     #   tkfocus(tg)
# }