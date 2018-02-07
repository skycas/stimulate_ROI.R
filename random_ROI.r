random_ROI <- function(dmr_data){
  require(parallel)
  cl.cores <- detectCores()
  cl <- makeCluster(cl.cores)
  
  
  dmr_dir <- as.data.frame(dmr_data)
  chr.select <- rownames(dmr_dir)
  chr <- c(rep(chr.select,dmr_dir$dmr_num))
  
  clusterExport(cl,"dmr_dir")
  
  start <-  unlist(parLapply(cl,chr.select, function(x){temp=dmr_dir[x,]; start=sort(sample(1:temp$chr_length,temp$dmr_num))}))
  wid <- unlist(parLapply(cl,chr.select, function(x){
    temp=dmr_dir[x,]
    k = runif(temp$dmr_num,temp$dmr_min,temp$dmr_max)
    kk = ceiling(k*temp$sum_width/sum(k))
  }))
  end <- start + wid
  return(as.data.frame(cbind(chr,start,end)))
  stopCluster(cl)
}
