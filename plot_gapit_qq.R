#!/usr/bin/Rscript
library(optparse)
library(ggplot2)
library(hexbin)
option_list = list(make_option(c("-i","--input"), help="the raw GAPIT result file,multiple files must be separated by comma"),
                   make_option(c("-o","--output"), help = "the output picture name"),
                   make_option(c("-w","--width"), help="the width of out picture",default = 8.95),
                   make_option(c("-l","--height"), help = "the height of out picture",default = 3.34),
                   make_option(c("-d","--device"), help="output picture generator,such as  pdf,png,jpeg",default = "jpeg")
)

opts = parse_args(OptionParser(option_list=option_list))
files = unlist(strsplit(opts$input,split = ","))
plot_qq<-function(gwas_result_fn=NULL, out=NULL,out.width=NULL, out.height=NULL,device = NULL){
  fn_names = as.vector(gwas_result_fn)
  fn_num = length(fn_names)
  plot_data= data.frame()
  for (i in 1:fn_num){
    df=read.csv(file=fn_names[i],header = T)
    model_name = strsplit(fn_names[i],".",fixed = T)
    model = model_name[[1]][2]
    exp=-log10(ppoints(length(df$P.value)))
    obs=-log10(sort(df$P.value,decreasing = FALSE))
    qq_data = data.frame(model=model,obs=obs,exp=exp)
    plot_data =rbind(plot_data,qq_data)
  }  
  plot_data$model<-factor(plot_data$model,levels = c("FarmCPU","MLM","GLM"))
  p<-ggplot(plot_data,aes(exp,obs,colour=model))+geom_point()+facet_grid(.~model)+ geom_abline(slope = 1,intercept = 0,colour="red")+xlab(expression(Expected ~ ~-log[10](italic(p))))+ylab(expression(Observed ~ ~-log[10](italic(p))))
  ggsave(filename = out, p,device =device, width =out.width,height = out.height,units = "in")
}
plot_qq(gwas_result_fn = files, out=opts$output, out.width = opts$width, out.height = opts$height,device = opts$device)