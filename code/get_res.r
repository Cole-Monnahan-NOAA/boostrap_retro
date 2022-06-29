
#Required packages
library(here)
library(devtools)
library(r4ss)
library(dplyr)

#retro subfolders - will want to set this up 
#sf<-list.files(pattern="retro")

#create files to store results  
write.table(data.frame(assess_yr='assess_yr', name='name', boot='boot', peel='peel',year='year',value='value'),file="timeseries_res.csv",sep=",",row.names=FALSE,col.names=FALSE,append=FALSE)
write.table(data.frame(asess_yr="assess_yr",par='par', boot='boot', peel='peel', value='value'),file="parameter_res.csv",sep=",",row.names=FALSE,col.names=FALSE,append=FALSE)

#FUNCTION to get results from Report.sso
get_res<-function(dir="foo")
{
  Models<-SSgetoutput(dirvec=dir)
  
  lapply(Models,function(Models)
  {
    rep=Models
    
    #Get timeseries results
    #ssb and apical F
    ssb_f_ts<-rep$sprseries[rep$sprseries$Yr<=rep$endyr,]
    
    #recruitment
    recr_ts=rep$recruit[rep$recruit$Yr<=rep$endyr,]
    
    #Create data frames
    ssb<-data.frame(assess_yr=rep$endyr, name='ssb', boot=0, peel=1, year=ssb_f_ts$Yr, value=ssb_f_ts$SSBfished)     #NEED TO UPDATE BOOT AND PEEL TO BE AUTOMATED
    ap_f<-data.frame(assess_yr=rep$endyr, name='f', boot=0, peel=1, year=ssb_f_ts$Yr, value=ssb_f_ts$sum_Apical_F)   #NEED TO UPDATE BOOT AND PEEL TO BE AUTOMATED
    recr<-data.frame(assess_yr=rep$endyr, name='recr', boot=0, peel=1,year=recr_ts$Yr,value=recr_ts$pred_recr)       #NEED TO UPDATE BOOT AND PEEL TO BE AUTOMATED
    
    #Combine all timeseries results
    ts_res<-bind_rows(ssb,ap_f,recr)
    write.table(ts_res,file="timeseries_res.csv",sep=",",row.names=FALSE,col.names=FALSE,append=TRUE)
    
    #Get parameters
    pars<-rep$parameters
    pars_res<-data.frame(assess_yr=rep$endyr, par=pars$Label, boot=0, peel=1, value=pars$Value) #NEED TO UPDATE BOOT AND PEEL TO BE AUTOMATED, ALSO NEED TO THEN CALC YEAR
    write.table(pars_res,file="parameter_res.csv",sep=",",row.names=FALSE,col.names=FALSE,append=TRUE)   
  })

}

#sf=c(getwd(),getwd())
lapply(sf,get_res(sf))
