#' Parallel_Tree
#'
#'
#' @param x A data frame to be used for plotting. If other arguments are left blank the entire data frame will be plotted.
#' @param use A vector of strings naming the variables, in order, in x to be used for plotting. Default is names(x). Note: Variables can be repeated if desired.
#' @param standardize Asksif variables used should be converted to z-scores ("Z"), or if scales should be set to have identical minimums and maximums ("MM") prior to plotting. Useful when plotted variables are on different scales. Default is "NONE".
#' @param full_plot A logical operator, if set to FALSE, output will be a ggplot2 object with no geoms. Default is TRUE.
#' @param append A vector strings naming variables in x that should be appended to the data frame used for plotting. Default is NULL.
#' @param a The alpha to be used by geom_path() when making the plot. Default is 1, but should be set lower if overplotting is likely to be an issue.
#' @param type allows for two choices: "negative" Sets geom_path() color to "white", and plots on a black background. Second option is "print_friendly" which will produce a plot with plack background only behind observations and the rest of the plot will be white. Designed to retain benefits of plotting white lines on black background while minimizing ink usage, this option may also be useful for simultaneous density observation and outlier detection. Default is NULL.
#' @param linesize allows choice of line size used for geom_path() default is 0.1.
#' @param color takes a string variable to be passed to geom_path() color aesthetic.
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
#' Y<-Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Mean weight",
#' "Chick Mean weight","Time Mean weight", "weight"),
#' append = c("Diet","weight","Time","Chick"))
#' Y
#'
#' #color can be added using the color functionality
#' Z<-Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Mean weight",
#' "Chick Mean weight","Time Mean weight", "weight"),
#' color="Diet")
#' Z
#'
#' #altering the alpha, and plotting using the negative setting,
#' #may be useful in cases of overplotting.
#' Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Mean weight",
#' "Chick Mean weight","Time Mean weight", "weight"),
#' append = c("Diet","weight","Time","Chick"), a=.2, type="negative")
#'
#' #geom_path is the default, although other geoms may be useful
#' Parallel_Tree(Means_Chick, use = c("Grand Mean weight", "Diet Mean weight",
#' "Chick Mean weight","Time Mean weight", "weight"),
#' append = c("Diet","weight","Time","Chick"), full_plot=FALSE) +
#' geom_boxplot(aes(group=levels))+scale_x_continuous(breaks=c(1:5),
#' labels=c("Grand Mean","Diet Mean", "Chick Mean", "Age Mean", "Scores"))
#'
#'#Note that if facets are used the means are not recalculated.
#'#Such plots should be interpreted with caution.
#'Y+geom_path(aes(color=Diet))+facet_wrap(~Time)
#'
#'
Parallel_Tree<-function(x, use=names(x), standardize="NONE", full_plot=TRUE, append=NULL, a=1, type=NULL, linesize=.1, color=NULL){
  #ID and values is set to null initially to avoid a note
  ID<-values<-NULL
  #this gathers the variables to be appended and color variables to be attached to the subsequent data frame
  if((!is.null(append)) | (!is.null(color))){
    append<-c(append,color)
    append_vars<-as.data.frame(x[,append])
    names(append_vars)<-append
    if(!is.null(color)){
      names(append_vars)[length(append_vars)]<-"color"
    }
  }
  #this takes x and converts it to a dataframe if it is not already. ggplot generally requires a dataframe for plotting
  if(!is.data.frame(x)){
    x<-as.data.frame(x)
  }
  #this standardizes the variables using Z scores. If there is no variability it will set the variable to 0
  if(standardize=="Z"){
    i<-1
    while(i<length(use)+1){
      if(var(na.omit(x[,use[i]]))==0){
        x[,use[i]]<-x[,use[i]]-mean(na.omit(x[,use[i]]))
        i<-i+1
      } else{
        x[,use[i]]<-(x[,use[i]]-mean(na.omit(x[,use[i]])))/sqrt(var(na.omit(x[,use[i]])))
        i<-i+1
      }
    } #this next standardization sets the minimum and maximum of all variables to be 0 and 1. If there is no variability the variable is set to .5
  } else if (standardize == "MM"){
    i<-1
    while(i<length(use)+1){
      if(var(na.omit(x[,use[i]]))==0){
        x[,use[i]]<-.5
        i<-i+1
      } else{
        x[,use[i]]<-(x[,use[i]]-min(x[,use[i]]))
        x[,use[i]]<-x[,use[i]]/max(x[,use[i]])
        i<-i+1
      }
    }
  }
  #This section creates the data frame to be used in the plotting. A long form dataset is used. Each observation is given an ID number, then the variables are appended end to end and the ID variable repeated as necessary. The levels variable numbers each variable in the order that they will be plotted.
  i<-1
  temp<-c()
  while(i<length(use)+1){
    K<-cbind(x[,use[i]],i,c(1:nrow(x)))
    temp<-rbind(temp,K)
    i<-i+1
  }
  temp<-as.data.frame(temp)
  names(temp)<-c("values", "levels", "ID")
  #If variables were to be appendend or passed to an aesthetic (e.g., color) they are attached to the dataframe here
  if(!is.null(append)){
    temp<-cbind(temp,append_vars)
  }
  #The base ggplot plots the x axis using the levels variable, and the y axis as the values varaible. Each individual observation is plotted seperately via its ID number.
  plot<-ggplot(data=temp,aes(x=levels, y=values, group=ID))
  if(full_plot==TRUE){
    #If a specific kind of plot was selected it is plotted here.
    if(!is.null(type)){
      if(type=="negative"){
        plot<-plot+geom_path(color="white", alpha=a, size=linesize)+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)+theme(panel.background = element_rect(fill="black"),  panel.grid.minor = element_blank())
        }else if(type=="print_friendly"){
          plot<-plot+geom_path(color="black",size=10)+geom_path(color="white", alpha=a, size=linesize)+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)+theme(panel.background = element_rect(fill="white"),  panel.grid.minor = element_blank())
        }
      #if color was to be used it is plotted here
    } else if (!is.null(color)){
      plot<-plot+geom_path(alpha=a, size=linesize, aes(color=color))+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)
      #if a "normal" plot with no additional aesthetics was to be plotted it is plotted here
    } else{
      plot<-plot+geom_path(alpha=a, size=linesize)+scale_x_continuous(breaks=c(1:(length(use))), labels=c(use))+xlab(NULL)+ylab(NULL)
    }
  }
  #The final plot is returned
  return(plot)
}


