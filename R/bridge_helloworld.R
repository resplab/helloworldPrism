#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
model_run<-function(x=1,y=2)
{
  plot(x,y)
  results <- x * y
  return(as.list(results))
}
