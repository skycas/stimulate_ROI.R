dmr_DIS <- function(dmr, BSgenome){
  dmr <- as.data.frame(dmr)
  
  chr_db <- seqnames(BSgenome)
  
  chr.select <- intersect(unique(dmr$seqnames),chr_db)
  chr.select.length <- seqlengths(BSgenome)[chr.select]
  
  chr_dis <- table(dmr$seqnames)[chr.select]
  dis <- lapply(chr.select,function(x){
    dmr_chr <- dmr[dmr$seqnames==x,]
    min_wid <- min(dmr_chr$width)
    max_wid <- max(dmr_chr$width)
    sum_wid <- sum(dmr_chr$width)
    return(c(min_wid,max_wid,sum_wid))
  })
  dis <- do.call(rbind,dis)
  dis <- cbind(chr_dis,dis,chr.select.length)
  colnames(dis) <- c("dmr_num","dmr_min","dmr_max","sum_width","chr_length")
  return(dis)
}
