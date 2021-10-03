#Check Usage -----
checks=checkPackageUsage()
runTests(package="SDT")
#Dismiss Usage Warnings -----
#  suppressUsageWarnings(checks)
###Dev Setup -----
## INSTALL: CTRL + SHIFT + B
sDevTools::clearEnv() ## CTRL + SHIFT + R
library(sDevTools)
sDevTools::loadUtils()
#Dev -----


library(ggplot2)
dat<-newDT(data.table(diamonds))

dt<-dat[,.(sum(price),sum(carat)),by=cut+clarity~color]$data
id='sd'
colsize='1fr'
rowsize='1fr'
#' @export
grid_headers=function(id,data,colsize,rowsize,...){
  ns=NS(id)
#df<-get_casted_identity_groups(data)
#mg<-get_casted_measures(data)
cm=get_column_names(data)
dsf<-get_identity_names(data)
cm2<-as.data.table(t(sapply(dsf,function(x)c(rep('.',ncol(cm)-1),x))))
tagList(build_column_header(id=id,name='colnames',data=cm,colsize=colsize ,rowsize=rowsize),
build_column_header(id=id,name='rownames',data=cm2,colsize=colsize ,rowsize=rowsize))
}
#' @export
build_column_header=function(id,name,data,colsize,rowsize,...){
  ns<-NS(id)
  ncols=nrow(data)
  nrows=ncol(data)
  header<-names(data)[1:l(names(data))]
  header=header[1:l(names(data))]
  headerP=paste0(header,"P")
  data[,c(paste0(header,"P")):=map2(.SD,header,function(x,y){rleid(x,prefix=paste0(y,'grp'))})]


  fd<-melt(data,measure.vars = list(header,headerP),id.vars=NULL)
  fd<-unique(fd)
  fd[,variable:=as.numeric(variable)]
  fd[,class:='table-header']
  fd[variable>min(variable),class:='sticky']

  fd<-fd[value1!="."]
  inner=glue_data(fd,"<div class='cell border-top {class}' style='grid-area:{value2};'>{value1}</div>")%sep%""
  grid_tmp_areas<-expr_label(data[,sapply(.SD,paste,collapse=" "),.SDcols=paste0(header,"P")])%sep%"\n"

  style=css(display= 'grid',
            grid.template.columns = rep(colsize,ncols),
            grid.template.rows = glue('repeat({nrows},{rowsize})'),
            grid.template.areas = grid_tmp_areas)
  tagOut<-div(...,HTML(inner))
  tagOut %>% tagAppendAttributes(style=style,id=ns(name),style=css(grid.area=name))
}
#' @export
grid_rows=function(id,data,colsize,rowsize,...){
  cm<-data%get%get_identity_names(data)
  colnams<-paste0('header',1:ncol(cm))
  colnamsP<-paste0(colnams,"P")
  setnames(cm,colnams)
  cm[,c(colnamsP):=map2(.SD,colnams,function(x,y){rleid(x,prefix=paste0(y,'grp'))})]
  build_grid_area(id,name='rowgroups',dt=cm,colnames=colnams,gridnames= colnamsP,type='header',rowsize=rowsize,colsize=rowsize,...)
}
#' @export
grid_main=function(id,data,colsize,rowsize,...){
  colnames=get_measure_names(data)
  gridnames<-paste0( colnames,"P")
datas=data%get% colnames
dims<-dim(datas)
#melt(datas,measure.vars=get_casted_measures(data))

dfs<-DT(sapply(glue('row{{1:dims[1]}}col{1:dims[2]}'),function(x,dims)glue(x),dims=dims))
setnames(dfs,gridnames)

datas<-datas[,lapply(.SD,function(x)as.character(signif(x)))]
dt<-cbind(datas,dfs)

build_grid_area(id=id,name='main',dt=dt,colnames=colnames,gridnames=gridnames,type='cell',rowsize=rowsize,colsize=colsize,...)
}
#' @export
build_grid_area=function(id,name,dt,colnames,gridnames,type,rowsize='12px',colsize='1fr',...){
  ns<-NS(id)
  rows=nrow(dt)
  ncol=l(colnames)


fd<-melt.data.table(dt,measure.vars = list(colnames,gridnames),id.vars=NULL)
fd<-unique(fd)
fd[,variable:=as.numeric(variable)]
fd[,]
fd[,class:='']
if(type=='header')
   fd[variable<max(variable),class:=' sticky']

fd<-fd[value1!="."]
inner=glue_data(fd,"<div class='cell border-top{class}' style='grid-area:{value2};'>{value1}</div>")%sep%""

grid_tmp_areas<-expr_label(dt[,reduce(.SD,function(x,y)paste(x,y)),.SDcols=gridnames])%sep%"\n"
style=css(display= 'grid',
    grid.template.columns = rep(colsize,ncol),
    grid.template.rows = glue('repeat({rows},{rowsize})'),
    grid.template.areas = grid_tmp_areas)
tagOut<-div(...,HTML(inner))
class=NULL

tagOut %>% tagAppendAttributes(style=style,id=ns(name),style=css(grid.area=name,class=class))
}
#' @export
grid_area<-function(...,id,data,colsize='1fr',rowsize='12px'){
  tagOut=div(
    grid_headers(id=id,data,colsize = colsize,rowsize = rowsize,class='sticky' ),
    grid_rows(id=id,data,colsize = colsize,rowsize = rowsize,class='sticky' ),
    grid_main(id=id,data,colsize = colsize,rowsize = rowsize ),
    ...)
  grid_tmp_areas=expr_label(c('rownames colnames','rowgroup main'))%sep%"\n"
  style=css(display= 'grid',
            grid.template.columns = c("auto",'1fr'),
            grid.template.rows =    c("auto",'1fr'),
            grid.template.areas = grid_tmp_areas)
  tagOut %>%
    tagAppendAttributes(style=style,id=id)
}


rowsize='12px'
colsize='1fr'
id='gridtest'



grid_area(id=id,data=data,colsize = colsize,rowsize = rowsize )
