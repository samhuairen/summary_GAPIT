#!/usr/bin/Rscript
library(optparse)
library(ggplot2)
library(hexbin)
option_list = list(make_option(c("-i","--input"), help="the raw GAPIT result file,multiple files must be in a vector"),
                   make_option(c("-o","--output"), help = "the output picture name"),
                   make_option(c("-w","--width"), help="the width of out picture",default = 8.95),
                   make_option(c("-l","--height"), help = "the height of out picture",default = 3.34),
                   make_option(c("-d","--device"), help="output picture generator, for example:pdf,jpeg,png",default = "pdf")
)

opts = parse_args(OptionParser(option_list=option_list))
files = unlist(strsplit(opts$input,split = ","))
plot_maf=function(gwas_result_fn=NULL,out=NULL,out.width=NULL, out.height=NULL,device = NULL){
  fn_names = as.vector(gwas_result_fn)
  fn_num = length(fn_names)
  plot_data= data.frame()
  for (i in 1:fn_num){
    df=read.csv(file=fn_names[i],header = T)
    model_name = strsplit(fn_names[i],".",fixed = T)
    df$model = model_name[[1]][2]
    plot_data =rbind(plot_data,df[,c("SNP","P.value","maf","model")])
  }
  plot_data$model=factor(plot_data$model,levels = c("FarmCPU","MLM","GLM"))
  head(plot_data)
  p<-ggplot(plot_data,aes(maf,-log10(P.value),colour=model))+stat_binhex()+facet_grid(.~model)+ scale_fill_gradientn(colours=c("gold4","white"))+facet_grid(.~model)+xlab("MAF")+ylab(expression(-log[10](italic(p))))
  ggsave(filename = out, p, device = device, width = out.width, height = out.height, units = "in")
}
plot_maf(gwas_result_fn = files, out=opts$output, out.width = opts$width, out.height = opts$height,device = opts$device)