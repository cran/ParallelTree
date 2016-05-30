#' Parallel_Tree
#'
#'
#' @param x A data frame to be used for plotting. If other arguments are left blank the entire data frame will be plotted.
#' @param use A vector of strings naming the variables, in order, in x to be used for plotting. Default is names(x). Note: Variables can be repeated if desired.
#' @param standardize A logical operator asking if variables used should be converted to z-scores prior to plotting. Default is FALSE.
#' @param full_plot A logical operator, if set to FALSE, output will be a ggplot2 object with no geoms. Default is TRUE.
#' @param append A vector strings naming variables in x that should be appended to the data frame used for plotting. Default is NULL.
#' @param a The alpha to be used by geom_path() when making the plot. Default is 1, but should be set lower if overplotting is likely to be an issue.
#' @param type allows for two choices: "negative" Sets geom_path() color to "white", and plots on a black background. Second option is "print_friendly" which will produce a plot with plack background only behind observations and the rest of the plot will be white. Designed to retain benefits of plotting white lines on black background while minimizing ink usage, this option may also be useful for simultaneous density observation and outlier detection. Default is NULL.
#'
#'
#'
#' @return The function returns a ggplot with scores on the Y axis and variable names, corresponding to levels, on the X axis. Default uses geom_path to create an individual path for each level 1 score.
#' @export
#'
#' @examples
#' #the msleep data can be found in the ggplot2 package
#' #This plots the sleep totals for a number of different mammals, as well as the grand,
#' # order and genus mean levels of sleep.
#' Means_sleep<-Group_function(msleep,"sleep_total",c("order","genus"))
#' Parallel_Tree(Means_sleep)
#'
#' #the ChickWeight data is from base R
#' #nested is set to false because Chick and Time are crossed
#' Means_Chick<-Group_function(data=ChickWeight,x="weight", levels =c("Diet","Chick","Time"),
#'  nested = FALSE, append=TRUE)
#' #Here all values not plotted are appended to the temp data frame
#' #created in the Parallel_Tree function
#' Y<-Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Level Means weight",
#' "Chick Level Means weight","Time Level Means weight", "weight"),
#' append = c("Diet","weight","Time","Chick"))
#' Y
#'
#' #color can be added using an appended variable
#' Y+geom_path(aes(color=Diet))
#'
#' #altering the alpha, and plotting using the negative setting,
#' #may be useful in cases of overplotting.
#' Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Level Means weight",
#' "Chick Level Means weight","Time Level Means weight", "weight"),
#' append = c("Diet","weight","Time","Chick"), a=.2, type="negative")
#'
#' #geom_path is the default, although other geoms may be useful
#' Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Level Means weight",
#' "Chick Level Means weight","Time Level Means weight", "weight"),
#' append = c("Diet","weight","Time","Chick"), full_plot=FALSE) +
#' geom_boxplot(aes(group=levels))+scale_x_continuous(breaks=c(1:5),
#' labels=c("Grand Mean","Diet Mean", "Chick Mean", "Age Mean", "Scores"))
#'
#'#Note that if facets are used the means are not recalculated.
#'#Such plots should be interpreted with caution.
#'Y+geom_path(aes(color=Diet))+facet_wrap(~Time)
Parallel_Tree<-function(x, use=names(x), standardize=FALSE, full_plot=TRUE, append=NULL, a=1, type=NULL){
  values<-ID<-NULL
  if(!is.null(append)){
    append_vars<-as.data.frame(x[,append])
    names(append_vars)<-append
  }
  if(!is.data.frame(x)){
    x<-as.data.frame(x)
  }
  if(standardize){
    i<-1
    while(i<length(use)+1){
      if(var(na.omit(x[,use[i]]))==0){
        x[,use[i]]<-x[,use[i]]-mean(na.omit(x[,use[i]]))
        i<-i+1
      } else{
        x[,use[i]]<-(x[,use[i]]-mean(na.omit(x[,use[i]])))/sqrt(var(na.omit(x[,use[i]])))
        i<-i+1
      }
    }
  } else{
    x<-x
  }
  i<-1
  temp<-c()
  while(i<length(use)+1){
    K<-cbind(x[,use[i]],i,c(1:nrow(x)))
    temp<-rbind(temp,K)
    i<-i+1
  }
  temp<-as.data.frame(temp)
  names(temp)<-c("values", "levels", "ID")
  if(!is.null(append)){
    temp<-cbind(temp,append_vars)
  }
  plot<-ggplot(data=temp,aes(x=levels, y=values, group=ID))
  if(full_plot==TRUE){
    if(!is.null(type)){
      if(type=="negative"){
        plot<-plot+geom_path(color="white", alpha=a)+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)+theme(panel.background = element_rect(fill="black"),  panel.grid.minor = element_blank())
        }else if(type=="print_friendly"){
          plot<-plot+geom_path(color="black",size=10)+geom_path(color="white", alpha=a)+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)+theme(panel.background = element_rect(fill="white"),  panel.grid.minor = element_blank())
        }
    }
      else{
      plot<-plot+geom_path( alpha=a)+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)
    }
  }
  if(standardize){
    plot<-plot+ylab("Standard Deviations")
  }
  return(plot)
}


