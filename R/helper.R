# add clIndx for the purpose of assigning parallel jobs
addClIndx = function(dt,indx,nCl){

    # indx assumed to be sorted
    if(!is.null(dt$clIndx)){dt$clIndx=NULL};

    # initialization
    thre = as.matrix(quantile(indx,(0:(nCl))/nCl));
    count = hist(indx, breaks=unique(thre),plot=FALSE)$counts;
    dt$clIndx = rep(1:length(count),times=count);

    # output
    return(dt);
}
updateCurrEra = function(curr,newEra){
    return(c(min(curr[1],newEra[1]),max(curr[2],newEra[2])));
}
mergeEraSingle = function(start,end,pw){

    # pw should be at least 1
    currEra = NULL;
    currLoc = 1;
    era = matrix(,nrow=length(start),ncol=2);
    while(currLoc <= length(start)){
        currEra = updateCurrEra(currEra,c(start[currLoc],end[currLoc]));
        # see if the next record is connected
        if(currLoc+1 <= length(start) &
                start[currLoc+1]-end[currLoc]<=pw){
            currEra = updateCurrEra(currEra,c(start[currLoc+1],end[currLoc+1]));
        }else{
            # update era
            era[currLoc,] = currEra;
            # clear currEra
            currEra = NULL;
        }
        currLoc = currLoc+1;
    }
    era = era[complete.cases(era),];
    if(!is.matrix(era)){era = matrix(era,nrow=1);}
    return(list(startAge = era[,1], endAge = era[,2]));
}
mergeEra = function(x,drugEra,pw){
    currDrugEra = subset(drugEra,clIndx==x);
    currDrugEra = currDrugEra[,mergeEraSingle(startAge,endAge,pw),by="indx"];
    return(currDrugEra);
}
getIndx = function(age,interval){
    currInterval = as.data.table(t(matrix(age,nrow=2)));
    setnames(currInterval,c("startAge","endAge"));
    currInterval$startIndx =
        subset(interval,startAge %in% currInterval$startAge)$indx;
    currInterval$endIndx =
        subset(interval,endAge %in% currInterval$endAge)$indx;
    return(unlist(mapply(seq,currInterval$startIndx,currInterval$endIndx)));
}
getIntervalSingle = function(startAge,endAge,id){
    # create interval from drug eras

    # endPoint
    endPoint = data.table(
        age = as.numeric(c(startAge,endAge)),
        enterLeaveMark = rep(c(FALSE,TRUE),each=length(startAge)),
        id = rep(id,2));
    setkey(endPoint,age,enterLeaveMark); setkey(endPoint,NULL);
    age = unique(endPoint$age);

    # interval
    interval = data.table(
        startAge = age[-length(age)],endAge = age[-1]);
    interval$indx = 1:nrow(interval);
    setcolorder(interval,c("indx","startAge","endAge"));

    # interval-drug indx
    indx = endPoint[,getIndx(age,interval),by="id"];
    setnames(indx,"V1","indx");
    interval = merge(interval,indx,by="indx",all.x=TRUE);
    setkey(interval,NULL);
    return(interval);
}
getInterval = function(x,era){
    curr = subset(era,clIndx==x);
    curr = curr[,getIntervalSingle(startAge,endAge,id),by="patientId"];
    return(curr);
}

parseConfig = function(x,configBr){

    # read
    currConfig = subset(configBr,indx==x);

    # dxIdWanted
    dxIdWanted = currConfig$dxId;

    # expose
    if(currConfig$expose=="era"){
        expose = "";
    }else if(currConfig$expose=="lasting"){
        expose = "Lasting";
    }else if(currConfig$expose=="rwOneMonth"){
        expose = "RwOneMonth";
    }

    # admit
    if(currConfig$admit=="start"){
        admit = "";
    }else if(currConfig$admit=="threeMonth"){
        admit = "ThreeMonth";
    }else if(currConfig$admit=="halfYear"){
        admit = "HalfYear";
    }else{
        admit = "";
    }

    # results
    return(paste("data",expose,admit,dxIdWanted,".RData",sep=""));

}
