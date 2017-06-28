
##
#
# Here are the Arguments:
# 
# data - data.frame or matrix
# data.row - row of data to plot (if NULL uses row 1)
# y.cols - columns of interest (if NULL it selects all numeric columns)
# main - title of plot (if NULL then rowname of data)
# add - whether the plot should be added to an existing plot
# col - color of the data line
# lty - lty of the data line
#
##
plotHelper.webplot = function(data, data.row = NULL, y.cols = NULL, main = NULL, add = F, 
                   col = "red", lty = 1, scale = T) {
  if (!is.matrix(data) & !is.data.frame(data)) 
    stop("Requires matrix or data.frame")
  if (is.null(y.cols)) 
    y.cols = colnames(data)[sapply(data, is.numeric)]
  if (sum(!sapply(data[, y.cols], is.numeric)) > 0) {
    out = paste0("\"", colnames(data)[!sapply(data, is.numeric)], "\"", 
                 collapse = ", ")
    stop(paste0("All y.cols must be numeric\n", out, " are not numeric"))
  }
  if (is.null(data.row)) 
    data.row = 1
  if (is.character(data.row)) 
    if (data.row %in% rownames(data)) {
      data.row = which(rownames(data) == data.row)
    } else {
      stop("Invalid value for data.row:\nMust be a valid rownames(data) or row-index value")
    }
  if (is.null(main)) 
    main = rownames(data)[data.row]
  if (scale == T) {
    data = scale(data[, y.cols])
    data = apply(data, 2, function(x) x/max(abs(x)))
  }
  data = as.data.frame(data)
  n.y = length(y.cols)
  min.rad = 360/n.y
  polar.vals = (90 + seq(0, 360, length.out = n.y + 1)) * pi/180
  
  # 
  if (add == F) {
    plot(0, xlim = c(-2.2, 2.2), ylim = c(-2.2, 2.2), type = "n", axes = F, 
         xlab = "", ylab = "")
    title(main)
    lapply(polar.vals, function(x) lines(c(0, 2 * cos(x)), c(0, 2 * sin(x))))
    lapply(1:n.y, function(x) text(2.15 * cos(polar.vals[x]), 2.15 * sin(polar.vals[x]), 
                                   y.cols[x], cex = 0.8))
    
    lapply(seq(0.5, 2, 0.5), function(x) lines(x * cos(seq(0, 2 * pi, length.out = 100)), 
                                               x * sin(seq(0, 2 * pi, length.out = 100)), lwd = 0.5, lty = 2, col = "gray60"))
    lines(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 2 * pi, length.out = 100)), 
          lwd = 1.2, col = "gray50")
  }
  
  
  r = 1 + data[data.row, y.cols]
  xs = r * cos(polar.vals)
  ys = r * sin(polar.vals)
  xs = c(xs, xs[1])
  ys = c(ys, ys[1])
  
  lines(xs, ys, col = col, lwd = 2, lty = lty)
  
}

##
#
##
plotHelper.radar <- function (x, labels = NULL, center = FALSE, connect = FALSE, 
                              scale = 1, ncolors = 31, fill = FALSE, add = FALSE, linetyp = "solid", 
                              main = "Radar Plot", cex = 1, colour = "red",lwd=1, ...) 
{
  #Parameters
  labelOffset_x <- 1.2
  labelOffset_y <- 1.2
  
  nvar <- length(x)
  if (is.null(labels)) 
    labels <- paste("V", 1:nvar, sep = "")
  SEGMENTS <- 48
  if (ncolors < 2) {
    colors <- FALSE
  }
  else {
    colors <- TRUE
  }
  angles <- (0:SEGMENTS) * 2 * pi/SEGMENTS
  unit.circle <- cbind(cos(angles), sin(angles))
  if (!add) {
    plot(unit.circle, typ = "l", asp = 1, axes = FALSE, xlab = "", 
         ylab = "", main = main,cex.main=cex)
    lines(unit.circle * 0.25, typ = "l", lty = "dotted")
    lines(unit.circle * 0.5, typ = "l", lty = "dotted")
    lines(unit.circle * 0.75, typ = "l", lty = "dotted")
  }
  if (colors) {
    gr <- colorRampPalette(c("red", "white", "blue"))
    colramp <- gr(ncolors)
  }
  else {
    colramp <- grey((ncolors:0)/ncolors)
  }
  for (c in 1:nvar) {
    nx <- (c - 1) * SEGMENTS/nvar + 1
    #Print the line of the axis var
    if (!add) {
      lines(c(0,unit.circle[nx,1]),c(0,unit.circle[nx,2]),lty = "dotted")
    }
    if (center) {
      x0 <- unit.circle[nx, 1] * 0.5
      y0 <- unit.circle[nx, 2] * 0.5
    }
    else {
      x0 <- 0
      y0 <- 0
    }
    scaler <- (x[c] * scale)
    x1 <- unit.circle[nx, 1]
    y1 <- unit.circle[nx, 2]
    Lx <- c(x0, x1) * scaler
    Ly <- c(y0, y1) * scaler
    if (c == 1) {
      Oldx <- unit.circle[(nvar - 1) * SEGMENTS/nvar + 
                            1, 1] * (x[nvar] * scale)
      Oldy <- unit.circle[(nvar - 1) * SEGMENTS/nvar + 
                            1, 2] * (x[nvar] * scale)
    }
    if (colors) {
      if (scaler < 0.5) {
        col = "red"
      }
      else {
        col = "blue"
      }
      lines(Lx, Ly, col = col, ...)
    }
    else {
      lines(Lx, Ly, ...)
    }
    if (connect) {
      lines(c(Oldx, x1 * scaler), c(Oldy, y1 * scaler), 
            lty = linetyp,
            col = colour,
            lwd=lwd)
    }
    if (fill) {
      polygon(c(0, Oldx, x1 * scaler, 0), c(0, Oldy, y1 * 
                                              scaler, 0), col = colour, 
              ...)
    }
    Oldx <- x1 * scaler
    Oldy <- y1 * scaler
    text(x1 * labelOffset_x, y1 * labelOffset_y, labels[c],cex=cex*1.3)
  }
}

##
#
##
plotHelper.spider <- function (y, x, data, labels = NULL, rescale = FALSE, center = FALSE, 
                    connect = TRUE, overlay = FALSE, scale = 1, ncolors = 31, 
                    fill = FALSE, main = NULL,cex=1,col="red",lwd=1, ...) 
{
  coloursVector <- c("red","blue","green","black")
  
  
  if (is.null(labels)) 
    labels <- colnames(data)[x]
  if (rescale) {
    data <- scale(data)/3
  }
  if (length(y) == 1) {
    if (!is.null(main)) {
      main = main
    }
    else {
      main <- colnames(data)[y]
    }
    plotHelper.radar(data[y, x], labels = labels, center = center, connect = connect, 
          scale = scale, ncolors = ncolors, fill = fill, main = main,cex=cex,lwd=lwd,col=col, 
          ...)
  }
  else {
    nvar <- length(y)
    for (i in 1:nvar) {
      if (!is.null(main)) {
        title = main[y[i]]
      }
      else {
        title <- colnames(data)[y[i]]
      }
      if (overlay) {
        if (i == 1) {
          plotHelper.radar(data[y[i], x], labels = labels, center = center, 
                connect = connect, scale = scale, ncolors = ncolors, 
                fill = fill, main = title,cex=cex,colour=col,lwd=lwd, ...)
        }
        else {
          plotHelper.radar(data[y[i], x], labels = labels, center = center, 
                connect = connect, scale = scale, ncolors = ncolors, 
                fill = fill, add = TRUE, linetyp = nvar%%6 + 
                  2, main = title,cex=cex,colour=coloursVector[nvar%%(length(coloursVector)+1)],lwd=lwd, ...)
        }
      }
      else {
        plotHelper.radar(data[y[i], x], labels = labels, center = center, 
              connect = connect, scale = scale, ncolors = ncolors, 
              fill = fill, main = title,cex=cex,colour=col,lwd=lwd, ...)
      }
    }
  }
}

# CREATE RADIAL PLOT
#
# Function parameters
# 
# The function has been heavily paramterised, as detailed below, to allow the user to closely manage most aspects of the resulting plot.
# 
# The one aspect of plot appearance that I have been unable to control satisfactorily is the colour assigned to each plot path. All suggestions welcome.
# 
# Input data
# plot.data - dataframe comprising one row per group (cluster); col1 = group name; cols 2-n = variable values
# axis.labels - names of axis labels if other than column names supplied via plot.data [Default = colnames(plot.data)[-1]
#                                                                                       
#   Grid lines
#   grid.min - value at which mininum grid line is plotted [Default = -0.5]
#   grid.mid - value at which 'average' grid line is plotted [Default = 0]
#   grid.max - value at which 'average' grid line is plotted [Default = 0.5]
#   
#   Plot centre
#   centre.y - value of y at centre of plot [default < grid.min]
#   label.centre.y - whether value of y at centre of plot should be labelled [Default=FALSE]
#   
#   Plot extent
#   #Parameters to rescale the extent of the plot vertically and horizontally, in order to
#   #allow for ggplot default settings placing parts of axis text labels outside of plot area.
#   #Scaling factor is defined relative to the circle diameter (grid.max-centre.y).
#   
#   plot.extent.x.sf - controls relative size of plot horizontally [Default 1.2]
#   plot.extent.y.sf - controls relative size of plot vertically [Default 1.2]
#   
#   Grid lines
#   #includes separate controls for the appearance of some aspects the 'minimum', 'average' and 'maximum' grid lines.
#   
#   grid.line.width [Default=0.5]
#   gridline.min.linetype [Default=“longdash”]
#   gridline.mid.linetype [Default=“longdash”]
#   gridline.max.linetype [Default=“longdash”]
#   gridline.min.colour [Default=“grey”]
#   gridline.mid.colour [Default=“blue”]
#   gridline.max.colour [Default=“grey”]
#   
#   Grid labels
#   grid.label.size - text size [Default=4]
#   gridline.label.offset - displacement to left/right of central vertical axis [Default=-0.02(grid.max-centre.y)]
#   *label.gridline.min - whether or not to label the mininum gridline [Default=TRUE]
#   
#   Axis and Axis label
#   axis.line.colour - line colour [Default=“grey”]
#   axis.label.size - text size [Default=3]
#   axis.label.offset - vertical displacement of axis labels from maximum grid line, measured relative to circle diameter [Default=1.15]
#   
#   x.centre.range - controls axis label alignment. Default behaviour is to left-align axis labels on left hand side of
#   plot (x < -x.centre.range); right-align labels on right hand side of plot (x > +x.centre.range); and centre align
#   those labels for which -x.centre.range < x < +x.centre.range
#   
#   Cluster plot lines
#   group.line.width [Default=1]
#   group.point.size [Default=4]
#   
#   Background circle
#   background.circle.colour [Default=“yellow”]
#   background.circle.transparency [Default=0.2]
#   
#   Plot legend
#   plot.legend - whether to include a plot legend [Default = FALSE for one cluster; TRUE for 2+ clusters]
#   legend.title [Default=“Cluster”]
#   legend.text.size [Default=grid.label.size=4]
#   
plotHelper.createRadialPlot <- function(plot.data,
                             axis.labels=colnames(plot.data)[-1],                             
                             grid.min=-0.5,  #10,
                             grid.mid=0,  #50,
                             grid.max=0.5,  #100,
                             centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                             plot.extent.x.sf=1.2,
                             plot.extent.y.sf=1.2,
                             x.centre.range=0.02*(grid.max-centre.y),
                             label.centre.y=FALSE,
                             grid.line.width=0.5,
                             gridline.min.linetype="longdash",
                             gridline.mid.linetype="longdash",
                             gridline.max.linetype="longdash",
                             gridline.min.colour="grey",
                             gridline.mid.colour="blue",
                             gridline.max.colour="grey",
                             grid.label.size=4,
                             gridline.label.offset=-0.02*(grid.max-centre.y),
                             label.gridline.min=TRUE,
                             axis.label.offset=1.15,
                             axis.label.size=3,
                             axis.line.colour="grey",
                             group.line.width=1,
                             group.point.size=4,
                             background.circle.colour="yellow",
                             background.circle.transparency=0.2,
                             plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                             legend.title="Cluster",
                             legend.text.size=grid.label.size ) {
  
  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  
  #Declare required internal functions
  
  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    
    path <- as.factor(as.character(df[,1]))
    
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      
      pathData = subset(df, df[,1]==i)
      
      for(j in c(2:ncol(df))){
        
        #pathData[,j]= pathData[,j]
        
        graphData=rbind(graphData, data.frame(group=i, 
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
      
    }
    
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    
    graphData #data frame returned by function
    
  }
  
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  
  
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  ### Convert supplied data into plottable format
  
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  #print(group$path)
  
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  
  # (d) Create file containing axis labels + associated plotting coordinates
  
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  
  # (e) Create Circular grid-lines + labels
  
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)
  
  
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw() + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
    scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)
  
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)
  
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)
  
  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
  
  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,face="bold",size=grid.label.size, hjust=1) }
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,face="bold",size=grid.label.size, hjust=1)
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,face="bold",size=grid.label.size, hjust=1)
  
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,face="bold",size=grid.label.size, hjust=0.5) }
  
  return(base)
  
}


plot.grid_arrange_shared_legend <- function(plotList,customTheme,...) {
  plots <- plotList
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position="bottom") + customTheme)$grobs
  
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  #lheight <- sum(legend$height)
  #lheight <- sum(legend$height) - unit(0.07,"npc")
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    #heights = c(rep(1+(0.1),length(plotList)),0.1),
    ...)
}

plotHelper.hmm.emissionDistributionGrid <- function (emissionParms,obsSymbols) {
  nstates <- length(emissionParms)
  nObs <- length(emissionParms[[1]])
  epDf <- data.frame(emissionParms)
  
  #Set rownames and colnames
  if (missing(obsSymbols)) {
    rownames(epDf) <- laply(1:nObs,function (i) paste("Obs.Symbol",i))
  }
  else {
    rownames(epDf) <- obsSymbols
  }
  colnames(epDf) <- laply(1:nstates, function (i) paste("state",i))
  
  #Melt the matrix with variables rownames and colnames
  epDfLf <- melt(as.matrix(epDf))
  
  p <- ggplot(data = epDfLf, mapping = aes(x = Var1, y = value, group=1))
  p <- p + facet_grid(Var2~., scale="free")
  
  for (i in 1:nstates) {
    p <- p + layer(data= epDfLf %>% filter(Var2==paste("state",i)), 
                   geom = c("line"), 
                   stat = "identity") + ylim(0,1)
  }
  
  p <- p + labs(x="", y="probability")
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}

plotHelper.simulationTS <- function(simSeries,
                                    simulationID,
                                    measureResolution,
                                    measures = "ALL",
                                    incidentsDrawing=FALSE,
                                    markExecutionStartTime=FALSE) {
  
  metric_labeller <- function (variable, value) {
    metric_names <- list(
      'scoreTS' = "S",
      'agilityTS' = "A",
      'attentionTS'= "At",
      'cooperationTS' = "C",
      'aggressivenessTS' = "Ag",
      'precisionTS' = "P"
    )
    
    return(metric_names[value])
  }
  
  TSList <- simSeries[[simulationID]]
  
  aux <- melt(as.matrix(data.frame(TSList)))
  aux$Var1 <- ((aux$Var1 -1)*measureResolution) #From time steps to seconds
  
  #Incidents data
  if (incidentsDrawing) {
    incidents <- dataHelper.getSimulationIncidentSnapshots(simulationID)
  }
  
  #Simulation monitoring real start time
  if (markExecutionStartTime) {
    firstSimulationSpeedUp <- dataHelper.getMonitoringStartTime(simulationID)
  }
  
  p <- ggplot(data = aux, mapping = aes(x = Var1, y = value, group=1))
  if (measures == "ALL" || length(measures) > 2) {
    p <- p + facet_grid(Var2~., scales="free_x", labeller = metric_labeller)
  }
  
  for (i in 1:length(TSList)) {
    
    if (measures == "ALL" || names(TSList[i]) %in% measures) {
      p <- p + layer(data= aux %>% filter(Var2==names(TSList)[i]),
                     geom = c("line"),
                     stat = "identity",
                     position = "identity") + 
        ylim(c(0,1))
      
      
      if (incidentsDrawing) {
        p <- p + geom_vline(xintercept=incidents$elapsedRealTime, colour="red",linetype="longdash")
      }
      
      #Draw a vertical line where the simulation time is accelerated (simulation start)
      if (markExecutionStartTime) {
        p <- p + geom_vline(xintercept=firstSimulationSpeedUp, colour="green",linetype="longdash")
      }
    }
  }
  
  p <- p + labs(x="Time (ms)")
  if (measures == "ALL" || length(measures) > 1) {
    p <- p + labs(y="Performance Measure [0,1]")
  } else {
    p <- p + labs(y=paste(measures[[1]],"[0,1]"))
  }
  # p <- ifelse(measures == "ALL" || length(measures) > 2,
  #             p + labs(y=paste(measures[[1]],"[0,1]"))
  # )
  p <- p + theme(axis.text = element_text(size=24),
                 axis.title=element_text(size=27), 
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 strip.text = element_text(size=27))
  
  p <- p + scale_y_continuous(breaks=c(0,0.5,1), minor_breaks=c(0.25,0.75))
  p
}