#' @export
DT=function(...){
  data.table::data.table(...)
}
#' @export
insert_data_expr=function(x,pre){

  if(is.name(x))
    return(pre)
  if(is_call(x,name="$"))
    return(pre)
  if(is_call(x,name="[")){
    x[[2]]<-insert_data_expr(x=x[[2]],pre=pre)
    return(x)
  }

}
zero_fill=function(x){
  if(length(x)==0)return(0)
  x
}
na_fill=function(x){
  if(length(x)==0)return(x[1])
  x
}
#' @export
`%get%`<-function(x,names)UseMethod("%get%")
#' @export
`%get%.default`<-function(x,names){
  assert_names(names(x))
  assert_character(names)
  x[names(x)%in%names]
}
#' @export
`%get%.data.table`<-function(x,names){
  assert_names(names(x))
  assert_character(names)
  sdcols=names(x)[names(x)%in%names]
  x[,..sdcols]
}
#' @export
`%nget%`<-function(x,names)UseMethod("%nget%")
#' @export
`%nget%.default`<-function(x,names){
  assert_names(names(x))
  assert_character(names)
  x[names(x)%nin%names]
}
#' @export
`%nget%.data.table`<-function(x,names){
  assert_names(names(x))
  assert_character(names)
  sdcols=names(x)[names(x)%nin%names]
  x[,..sdcols]
}
#' @export
`%gget%`<-function(x,patterns)UseMethod("%gget%")
#' @export
`%gget%.default`<-function(x,patterns){
  assert_names(names(x))
  assert_character(patterns)
  x[grepl(patterns%sep%"|",names(x))]
}
#' @export
`%gget%.data.table`<-function(x,patterns){
  assert_names(names(x))
  assert_character(patterns)
  sdcols=names(x)[grepl(patterns%sep%"|",names(x))]

}



#' @export
is_list=rlang::is_list
get_node=function(x,indices,env=caller_env()){
  x<-enexpr(x)
  next_level<-function(exp_in,index){

    tmp=expr(ex[[!!index]])
    tmp[[2]]<-exp_in
    tmp
  }
  for(i in indices){
    x<-next_level(x,i)
  }
  # x<- try(eval(!!x,envir = env),silent=T)
  out<-expr(try(!!x,silent=T))
  x<-eval(out,envir = env)
  if(missing(x))return(missing_arg())
  if(is(x,'try-error'))return(missing_arg())
  x
}
gen_html_table=function(data,class='table table-hover'){
  data[,rowid:=1:nrow(data)]
  tmp=DTapply(data,names(data),td)

  body=reduce(tmp,paste0)%>% tr() %>% tbody()
  HTML(htable(thead(names(data))%P% body,class=class))
}
parse_list=function(x){
  switch_expr(x,
              # Base cases
              constant =x,
              symbol ={
                tmp<-self$replace_symbols_with_formula_pointers(x)()
                tmp<-list(tmp)
                names(tmp)<-deparse(x)
                tmp
              },
              pairlist = {
                tmp<- self$replace_symbols_with_formula_pointers(x)()
                tmp<-list(tmp)
                names(tmp)<-deparse(x)
                tmp
              },
              call = {
                tmp<- self$replace_symbols_with_formula_pointers(x)()
                tmp<-list(tmp)
                names(tmp)<-deparse(x)
                tmp
              })
}

parse_name_list=function(x){
  switch_expr(x,
              # Base cases
              constant =NULL,
              symbol ={
                id<-self$get_variable_id(as.character(x))
                var_names_fn<-self$get_attrs(attrs='name_fn',vars=id)
              },
              pairlist = {
                tmp<- self$replace_symbols_with_name_pointers(x)
                evalp=self$eval_pointers
                id=create_unique_id(10)
                name_fn<-list(
                  function(){
                    deparse(evalp(tmp()))
                  })
                names(name_fn)<-id
                list(var_id=id,
                     name=deparse(x),
                     name_fn=name_fn)

              },
              call = {
                tmp<- self$replace_symbols_with_name_pointers(x)
                evalp=self$eval_pointers
                id=create_unique_id(10)
                name_fn<-list(
                  function(){
                    deparse(evalp(tmp()))
                  })
                names(name_fn)<-id
                list(var_id=id,
                     name=deparse(x),
                     name_fn=name_fn)

              })
}
remove_parenthesis<- function(x){
  if(is_call(x)&&
     x[[1]]==sym("(")){
    x=x[[2]]
    return(  remove_parenthesis(x))
  }

  return(x)
}
is_fixed_arg<- function(x){
  if(is_call(x)&&
     x[[1]]==sym("(")){
    x=x[[2]]
    return(is_fixed_arg(x))
  }
  x_test<-is_call(x)&&
    x[[1]]==sym("[")&&
    grepl('.D',as.character(x[[2]]),fixed=TRUE)
  return( x_test)
}
has_fixed_arg<-function(x){
  x2=x[[2]]
  x3=x[[3]]

  is_fixed_arg(x[[2]])|is_fixed_arg(x[[3]])
}
get_by=function(x,env){
  if(is_call(x)&&grepl('\\.D_[[:alnum:]]{8}_\\.D',as.character(x[[2]]))){
    tmp<- expr(tmp[0,1:(l(!!x[[2]])-1)])
    tmp[[2]]<-x[[2]]
    out<-expr(names(!!tmp)) %>% eval(envir=env)
    out2<-syms(out)
    names(out2)=out
    return(expr(.(!!!out2)))
  }
  if(is_call(x)&&as.character(x[[2]])=='.D'){
    if(expr_list_len(x$by)==0)return(NULL)
    return(x$by)
  }
  return(NULL)
}

dt_expr2env<-function(x,env=caller_env()){
  x<-enexpr(x)
  x5<-try(x[[5]],silent=TRUE)
  if(is_missing(x5)||is_error(x5))x5<-NULL
  aname=NULL
  x4<-try(x[[4]],silent=TRUE)
  if(is_error(x4))x4<-NULL
  # if(is_assignment(x4))
  #   aname=as.character(x4[[2]])
  tmp<-list(by=x5,i=x[[3]],j=x4)
  list2env(tmp,envir=env)
}
dyn_expr2env<-function(x,env=caller_env()){
  x<-enexpr(x)
  xargs=call_args(x)
  aname=NULL
  by<-NULL
  include_LOD=NULL
  exclude_LOD=NULL
  if(is_assignment(x[[4]])){
    aname=as.character(x[[4]][[2]])
    J=deparse(x[[4]][[3]])%sep%"\n"

    if(!is.null(xargs$by)){
      by= expr_unlist(xargs$by) %>% exprs_deparse()
    }
    if(!is.null(xargs$ include_LOD)){
      exclude_LOD= expr_unlist(xargs$exclude_LOD) %>% exprs_deparse()
    }
    if(!is.null(xargs$ include_LOD)){
      include_LOD= expr_unlist(xargs$ include_LODD) %>% exprs_deparse()
    }
    I=x[[3]]%or%NULL
    if(!is.null(I)){
      I=deparse(I)%sep%"\n"
    }
  }else{
    I=x[[3]]%or%NULL
    J=x[[4]]
    if(!is.null(xargs$by)){
      by<-xargs$by
    }
  }

  tmp<-list(I=I,name=aname,J=J,by=by,include_LOD=include_LOD,exclude_LOD=exclude_LOD,from_source=F,replace_old=T)
  list2env(tmp,envir=env)
}

subsetDT = R6::R6Class(
  'subsetDT',
  public = list(
    SDcol=NULL,
    grps=NULL,
    key=NULL,
    initialize = function(DT,SDcol,by) {
      setkeyv(DT,by)
      if(missing(SDcol))
        self$SDcol<-J(DT)
      if(!missing(SDcol))
        self$SDcol<-DT[[SDcol]]
      self$key=by
      by<-syms(by)
      DT[,i:=.I]
      self$grps<-expr(DT[,.(grp=list(i)),by=.(!!!by)]$grp) %>% eval()
      DT[,i:=NULL]
    },
    SD=function(.GRP){
      self$SDcol[self$grps[[.GRP]]]
    }
  ),
  private = list(),
  active = list()
)

#' @export
`[.subsetDT` <- function(self,.GRP) {

  self$SD(.GRP)


}



