

getCompPlotForMetric <- function(resampleObject, metric = "Accuracy", packType = "caret", title = "No Title", expName = "expName"){
  
  
  if(packType == "caret") {
    
    metricForGrep <- paste(c("*", metric), collapse = "")
    
    sampleResults<-resampleObject$values[,grep(metricForGrep,colnames(resampleObject$values))]
    
    plot<-sampleResults[,row.names(as.matrix(sort(sapply(sampleResults,mean),decreasing = F)))]
    plot$Datasets<-c("RFE(GAP)")
    accuracy_plot2<-melt(plot, id="Datasets")
    colnames(accuracy_plot2)<-c("GeneLists","Algorithm",metric)
    
    
    ggplot(accuracy_plot2, aes(x=Algorithm,y=Accuracy, colour = Algorithm)) +
      geom_point() +
      geom_jitter()+
      #geom_boxplot()+
      coord_flip()+
      ggtitle(title)
    
    picPrefix <- expName
    
    ggsave(paste(getwd(),"/diagrams/",picPrefix,"_",title,".png",sep=""))
    
    #all points
  } else {
    ggplot(plot, aes(x=metric,y="Algorithm")) + geom_point()
  }
}

getAndSaveEvalPlots <- function(resultList = NULL, title = "titleeser",expName = "expName", metric = "Accuracy"){
  
  if(metric == "Accuracy") {
  ggplot(resultList, aes(x=Model, y=Accuracy)) +
    #geom_bar +
    geom_bar(stat="identity") +
    #geom_boxplot()+
    #coord_flip()+
    ggtitle(title)
    
  } else if (metric == "Cindex") {
    ggplot(resultList, aes(x=Model, y=Cindex)) +
      #geom_bar +
      geom_bar(stat="identity") +
    #geom_boxplot()+
    #coord_flip()+
    ggtitle(title)
  }
  
  picPrefix <- expName
  ggsave(paste(getwd(),"/diagrams/",picPrefix,"_",title,".png",sep=""))
}