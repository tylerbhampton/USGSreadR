# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @title loadpackages
#' @return Loads multiple packages
#' @param packages [character] character vector of packages to call with library()
#' @md
#' @export

loadpackages=function(packages){for(p in packages){library(p,character.only=T,quietly=T)}}

#' @title fixSiteVect
#' @return Fixes 8-digit USGS Site Numbers
#' @param vect vector of site numbers
#' @md
#' @export

fixSiteVect=function(vect){
  vect=as.character(vect)
  vect[nchar(vect)==7]=paste0("0",vect[nchar(vect)==7])
  return(vect)
}

#' @title fixSiteDF
#' @return Takes a dataframe and Fixes a Column of USGS 8-digit Site Numbers
#' @param d dataframe to be returned
#' @param site string indicating the name of the site column to fix
#' @md
#' @export

fixSiteDF=function(d,site="SITE_NO"){
  d[,site]=fixSiteVect(d[,site])
  return(d)
}

#' @title fixSiteDFread
#' @return Reads in a dataframe and Fixes a Column of USGS 8-digit Site Numbers
#' @param file file path to read in the dataframe
#' @param site string indicating the name of the site column to fix
#' @md
#' @export

fixSiteDFread=function(file,site="SITE_NO"){
  d=read.csv(file,stringsAsFactors = FALSE)
  d[,site]=fixSiteVect(d[,site])
  return(d)
}


#' @title posixtodec
#' @return Takes a Date Column and Returns Decimal-Year
#' @param x [vector] vector of date strings
#' @md
#' @export

posixtodec=function(x){
  return(as.POSIXlt(x)$year+1900+as.POSIXlt(x)$yday/365)
}


#' @title DatetoWY
#' @return Takes a Date Column and Returns The October-start Water Year for the Dates
#' @param x [vector] vector of date strings
#' @md
#' @export

DatetoWY=function(x){
  year=as.POSIXlt(x)$year+1900
  month=as.POSIXlt(x)$mo+1
  return(ifelse(month<10,year,year+1))
}

#' @title readQfile
#' @return Finds a stored USGS Flow file or Downloads the Data if Not Found
#' @param site 8-digit USGS site number
#' @param Qdir file directory where Q data is stored
#' @md
#' @export

readQfile=function(site,Qdir){
  file=paste0("NWISdata",site,"_","00060",".csv")
  if(!file.exists(Qdir)){dir.create(file.path(Qdir))}
  if(!file.exists(file.path(Qdir,"WholeQseries"))){dir.create(file.path(Qdir,"WholeQseries"))}
  if(file %in% list.files(path = file.path(Qdir,"WholeQseries"))){
    Qdata=try(read.csv(file = file.path(Qdir,"WholeQseries",file),stringsAsFactors = FALSE))
  }else{
    Qdata=try(dataRetrieval::readNWISdv(siteNumbers=site,parameterCd="00060"))
    if(!("try-error" %in% class(Qdata))){
      if(nrow(Qdata!=0)){
        Qdata=dataRetrieval::renameNWISColumns(Qdata)
        Qdata=subset(Qdata,select=-c(agency_cd,site_no))
        write.csv(x = Qdata,
                  file=file.path(Qdir,"WholeQseries",file),
                  row.names = FALSE)
      }
    }
  }
  if("try-error" %in% class(Qdata)){
    return("try-error")
  }else{
    if(nrow(Qdata)==0){
      return("try-error")
    }else{
      Qdata=Qdata[!is.na(Qdata$Date),]
      Qdata[Qdata==-999999]=NA
      Qdata=Qdata[Qdata$Flow>=0,]
      Qdata$Date=as.POSIXct(Qdata$Date,format="%Y-%m-%d")
      Qdata$Flow_L.d=Qdata$Flow*2446575.55
      Qdata_year=as.POSIXlt(Qdata$Date)$year+1900
      Qdata_mo=as.POSIXlt(Qdata$Date)$mo+1
      Qdata$WY=DatetoWY(Qdata$Date)
      Qdata$WYmo=sapply(1:nrow(Qdata),function(i){
        month=Qdata_mo[i]-1-8
        month=ifelse(month<=0,month+12,month)
        month=as.character(ifelse(nchar(as.character(month))==1,paste0("0",month),month))
        return(paste0(Qdata$WY[i],month))
      })
      Qdata=Qdata[!is.na(Qdata$WY),]
      Qdata_decDate=posixtodec(Qdata$Date)
      Qdata_WYdecday1=posixtodec(paste0(Qdata$WY-1,"-10-01"))
      Qdata$WYdec=Qdata$WY+Qdata_decDate-Qdata_WYdecday1

      return(Qdata)
    }
  }
}

#' @title readwqualfile
#' @return Finds a stored USGS Water Quality data file or Downloads the Data if Not Found
#' @param site 8-digit USGS site number
#' @param param 5-digit USGS parameter code
#' @param dir file directory where data is stored
#' @md
#' @export

readwqualfile=function(site,param,dir){
  file=paste0("NWISdata",site,"_",param,".csv")
  if(file %in% list.files(path = dir)){
    data=try(read.csv(file = paste0(dir,file),stringsAsFactors = FALSE))
  }else{
    data=try(dataRetrieval::readNWISqw(siteNumbers=site,parameterCd=param))
    if(!("try-error" %in% class(data))){
      if(nrow(data!=0)){
        data=data[,c("sample_dt","result_va")]
        write.csv(x = data,
                  file=paste0(dir,file),
                  row.names = FALSE)
      }
    }
  }
  if("try-error" %in% class(data)){
    return("try-error")
  }else{
    if(nrow(data)==0){
      return("try-error")
    }else{
      data=data[!is.na(data$sample_dt),]
      data$sample_dt=as.POSIXct(data$sample_dt,format="%Y-%m-%d")
      data$WY=DatetoWY(data$sample_dt)
      data=data[!is.na(data$WY),]
      data_decDate=posixtodec(data$sample_dt)
      data_WYdecday1=posixtodec(paste0(data$WY-1,"-10-01"))
      data$WYdec=data$WY+data_decDate-data_WYdecday1
      data=data[,c("sample_dt","WY","WYdec","result_va")]
      return(data)
    }
  }
}


#' @title readQfile_year
#' @return Finds an annually summed stored USGS Flow file or Downloads the Data if Not Found
#' @param site 8-digit USGS site number
#' @param Qdir file directory where Q data is stored
#' @md
#' @export

readQfile_year=function(site,Qdir,retrieve=TRUE){
  file=paste0("DischargeSummary_",site,".csv")
  QYfileexists=(file %in% list.files(path = file.path(Qdir,"DischargeAnnualSummaries")))
  if(!QYfileexists){
    if(retrieve){
      retrieveQdatayear(site,Qdir=Qdir)
    }else{
      writeLines("FILE NOT FOUND")
      Qdata_yearly="try-error"
    }
  }
  Qdata_yearly=try(read.csv(file = file.path(Qdir,"DischargeAnnualSummaries",file),stringsAsFactors = FALSE))
  if("try-error" %in% class(Qdata_yearly)){Qdata_yearly="try-error"}
  return(Qdata_yearly)
}


#' @title retrieveQdatayear
#' @return Calculates Annually Summed USGS Flow Data and Downloads Data If Needed
#' @param site 8-digit USGS site number
#' @param Qdir file directory where Q data is stored, inherited from readQfile_year
#' @md
#' @export

retrieveQdatayear=function(site,Qdir,runclimate=TRUE,climatepath){

  basininfo=EGRET::readNWISInfo(siteNumber = site,parameterCd = "00060",interactive = FALSE)
  basin_area_m2=basininfo$drainSqKm*(1000^2)
  Qdata=readQfile(site)
  if(!("try-error" %in% class(Qdata) | "try-error" %in% Qdata)){

    years=sort(plyr::count(Qdata$WY)$x[plyr::count(Qdata$WY)$freq>350])
    if(length(years)>=5 & sum(1982:2017 %in% years)>0){
      Qdata_year=data.frame(WYear=years)
      Qdata_year$FlowYear_m3yr=NA
      Qdata_year$FlowYear_mmyr=NA
      Qdata_year$precip_mm=NA
      Qdata_year$pet_mm=NA
      Qdata_year$W2pct=NA
      Qdata_year$RBi=NA
      Qdata_year$pctQP=NA


      for(y in years){
        yearflow=Qdata$Flow_L.d[Qdata$WY==y]
        Qdata_year$FlowYear_m3yr[which(years==y)]=(mean(yearflow,na.rm=TRUE)*365*(1/1000))
        Qdata_year$FlowYear_mmyr[which(years==y)]=round(((mean(yearflow,na.rm=TRUE)*365*(1/1000))/basin_area_m2)*1000,2)
        Qdata_year$W2pct[which(years==y)]=round(sum(sort(yearflow,decreasing = TRUE)[1:floor(length(yearflow)*0.02)],na.rm = TRUE)/sum(yearflow,na.rm=TRUE),4)
        Qdata_year$RBi[which(years==y)]=round(sum(abs(yearflow[2:length(yearflow)]-yearflow[1:(length(yearflow)-1)]))/sum(yearflow[2:length(yearflow)]),4)

      }

      if(runclimate){
        climatefile=paste0("PRISMppet_bySite_",site,".csv")

        if(climatefile %in% list.files(path=climatepath)){
          climatedata_site=read.csv(file.path(climatepath,climatefile))
        }else{
          writeLines("FILE NOT FOUND")
        }

        yearsmatch=Qdata_year$WYear[Qdata_year$WYear %in% 1982:2017]
        Qdata_year$precip_mm[Qdata_year$WYear %in% yearsmatch]=round(climatedata_site$precip_mm[1982:2017 %in% yearsmatch],2)
        Qdata_year$pctQP=round(Qdata_year$FlowYear_mmyr/Qdata_year$precip_mm,4)
        Qdata_year$pet_mm[Qdata_year$WYear %in% yearsmatch]=round(climatedata_site$pet_mm[1982:2017 %in% yearsmatch],2)
        Qdata_year$AET.P=(Qdata_year$precip_mm-Qdata_year$FlowYear_mmyr)/Qdata_year$precip_mm
        Qdata_year$PET.P=Qdata_year$pet_mm/Qdata_year$precip_mm

      }

      Qdata_year$maxflowdate=NA
      Qdata_year$centroidflowdate=NA
      for(Y in Qdata_year$WYear){
        row=which(Qdata_year$WYear==Y)
        Qdata_year$maxflowdate[row]=round(mean(Qdata$WYdec[Qdata$WY==Y][which(Qdata$Flow_L.d[Qdata$WY==Y]==max(Qdata$Flow_L.d[Qdata$WY==Y]))])-Y,3)
        Qdata_year$centroidflowdate[row]=round(Qdata$WYdec[Qdata$WY==Y][which(sapply(1:sum(Qdata$WY==Y),function(i){sum(Qdata$Flow_L.d[Qdata$WY==Y][1:i])})>sum(Qdata$Flow_L.d[Qdata$WY==Y])/2)[1]]-Y,3)
      }
      write.csv(x = Qdata_year,
                file = paste0(Qdir,"DischargeAnnualSummaries/","DischargeSummary_",site,".csv"),
                row.names = FALSE)
    }
  }

}
