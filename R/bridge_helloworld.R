#' Title
#'
#' @param model_input
#'
#' @return Returns a list of results
#' @export
#'
#' @examples
model_run<-function(model_input = NULL)
{

  input<-unflatten_list(model_input)

  results <- helloworld        (a                        =model_input$a,
                                b                       =model_input$b)

  return(as.list(results))
}


get_default_input <- function() {
  model_input <- list(a                                  =c(1:100),
                      b                                  =c(1:100))
  return((flatten_list(model_input)))
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}



#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
