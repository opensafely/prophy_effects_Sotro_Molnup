
## Function written by Qing Wen ## 
## Date last updated: 30/04/2024 ## 
freq_single<-function(idx_colname){ta_feq<-table(idx_colname)
		total<-sum(as.numeric(ta_feq))
		ft2<-cbind(t(ta_feq),total)
		tb_prop0<-round((ft2/total*100),digits=2)
		fre_tb0<-rbind(ft2,tb_prop0)
		fre_tb<-t(fre_tb0)
		return(fre_tb)
}