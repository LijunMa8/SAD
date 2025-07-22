# SAD
SVM code is used to classify the interventions of the three therapies.
library(readxl)
library(writexl)
library(this.path)

currdir = dirname(this.path())
setwd(currdir)

pred.method='svm'
randbasemat=seq(100,900,200)
# randbasemat=seq(100,500,200)
hyper_paramat=-7:7     # grid search for C in svm
# hyper_paramat=-7:2     # grid search for C in svm
multiHyper=1
multiSeed=1            # 是否计算多个随机种子点

# pred.method='rf'
# randbasemat=100
# hyper_paramat=0
# multiHyper=1
# multiSeed=1            # 是否计算多个随机种子点

datapath='./res_server'  # 文件路径，自行修改

minFreq=0.5

outfname=paste0(pred.method,'_summary_minFreq',minFreq,'.xlsx')    # 输出文件

sheetmat=c('ALFF','mALFF','zALFF','fALFF','mfALFF','zfALFF','ReHo','mReHo','zReHo')

# ================= sub function ========================
resinfo=function(acclogallnew,infname) {
  load(infname)
  tmpdata=acclogallnew[acclogallnew$validFreq>=minFreq,]
  
  # tmpdf1:
  # #1: sheetname                     [tmpdf2]
  # #2: number of roi                 [tmpdf2]
  # #3: accuracy in test data set     [tmpdf2]
  # #4: valid frequency               [tmpdf2]
  # #5: roisel list (index)
  # #6: roisel list (names)
  
  if (dim(tmpdata)[1]==0) {
    print('no available feature set')
    roiind=NA
    tmpdf1=data.frame(cbind(tmpsheet,NA,NA,NA,NA,
                            NA,NA))
    tmpdf2=data.frame(cbind(tmpsheet,NA,NA,NA,NA))
  } else {
    indsel=which(tmpdata$Test==max(tmpdata$Test))
    roiind=indsel[1]
    
    tmpdf1=data.frame(cbind(tmpsheet,roiind,tmpdata[roiind,2],tmpdata[roiind,3],
                           roisellog[[roiind]],names(as.data.frame(dataX0))[roisellog[[roiind]]]))
    tmpdf2=data.frame(cbind(tmpsheet,roiind,tmpdata[roiind,2],tmpdata[roiind,3]))
  }
  return(list(tmpdf1,tmpdf2,roiind))
  
  # returning res:
  # #1: detailed result
  # #2: brief result
  # #3: number of roi
}

# ================= main function ========================

for (randbase in randbasemat) {
  
  for (hyperind in hyper_paramat) {
    datadir=paste0(datapath,'/',pred.method,'_res_HyperPara',hyperind,'_randseed',randbase,'-',randbase+100)

    reslog_acc=data.frame()                 # detail result
    resloginfo_acc=data.frame()             # brief result
    
    # tmpdf1:
    # #1: sheetname                     [tmpdf2]
    # #2: number of roi                 [tmpdf2]
    # #3: accuracy in test data set     [tmpdf2]
    # #4: valid frequency               [tmpdf2]
    # #5: roisel list (index)
    # #6: roisel list (names)
    
    namemat_log=c('Measure','FeatureCount','TestACC','ValidFreq','FeatureIndex','FeatureName')
    namemat_loginfo=c('Measure','FeatureCount','TestACC','ValidFreq')
    for (tmpsheet in sheetmat) {
      
      print(paste0('====================== ',tmpsheet,' ======================'))
      infname=paste0(datadir,'/',pred.method,'_',tmpsheet,'_ROI_imp.RData')
      # load(infname)
      
      resdf=resinfo(acclogallnew,infname)
      names(resdf[[1]])=namemat_log
      reslog_acc=rbind(reslog_acc,resdf[[1]])
      # names(reslog_acc)=namemat_log
      
      names(resdf[[2]])=namemat_loginfo
      resloginfo_acc=rbind(resloginfo_acc,resdf[[2]])
      
      roiind=resdf[[3]]
    }
    
    # names(reslog_acc)=namemat_log
    # names(reslog_auc)=namemat_log
    
    write_xlsx(list(ACC_summary=resloginfo_acc,ACC_summary_feature=reslog_acc),
               paste0(datadir,'/',outfname),col_names=TRUE)
    
    if (multiHyper==0) {
      break
    }
  }
  
  if (multiSeed==0) {
    break
  }
}

for (hyperind in hyper_paramat) {
  # tmpdataall_acc=data.frame()
  for (randbase in randbasemat) {
    summaryfile=paste0(datapath,'/',pred.method,'_res_HyperPara',hyperind,'_randseed',randbase,'-',randbase+100,'/',outfname)
    tmpdata=read_xlsx(summaryfile,sheet='ACC_summary')
    if (randbase == 100) {
      tmpdataall_acc=tmpdata
    } else {
      tmpdataall_acc=cbind(tmpdataall_acc,tmpdata)
    }
  }
  write_xlsx(list(ACC_summary_all=tmpdataall_acc),paste0(datapath,'/',outfname,'_HyperPara',hyperind,'.xlsx'))
}
