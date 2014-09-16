library(shiny)

# Stable vars to use when adding one sample at a time
means <- NULL
means_offset <- 0

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n) # draw n samples
  })
    
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    # Plot parameters...
    tcol="orange"      # fill colors
    acol="orangered"   # color for added samples
    tscale=2;          # label rescaling factor
    
    dist <- input$dist
    n <- input$n
    reps <- input$reps
    
    # generate a(nother) typical sample
    x<-switch(dist,
              norm = rnorm(n),
              unif = runif(n),
              lnorm = rlnorm(n),
              exp = rexp(n),
              rnorm(n))
    
    # Add to list of sample means or repeat sampling reps times depending on checkbox
    if (input$checkbox) {
      # Reset list of means if necessary
      if (length(means)==reps) {
        means<<-NULL
        means_offset<<-input$resample
      }
      means[input$resample-means_offset]<<-mean(x)
      }
    else {
      means<<-1:reps
      for (i in 1:reps) {
        means[i] <<-mean(switch(dist,
                               norm = rnorm(n),
                               unif = runif(n),
                               lnorm = rlnorm(n),
                               exp = rexp(n),
                               rnorm(n)))
      }
    }    
      
    
    # set plot range
    xmin = switch(dist, norm = -3, unif = 0, lnorm = 0, exp = 0, -3)
    xmax = switch(dist, norm =  3, unif = 1, lnorm = 4, exp = 4,  3)
    
    # do not plot outliers
    x[x>xmax]<-NA
    x[x<xmin]<-NA
    means[means>xmax]<<-NA
    means[means<xmin]<<-NA
    
    par(mfrow=c(3,1),mar=c(8,6,2,2)) 
    
    # plot true distribution
    x0 = seq(xmin,xmax,length.out=512);
    y0 = switch(dist,
                norm = dnorm(x0),
                unif = dunif(x0),
                lnorm = dlnorm(x0),
                exp = dexp(x0),
                dnorm(x0))
    y0=y0/sum(y0);
    plot(x0,y0,type="l",lwd=0,col=NULL,main="Population",xlab="",ylab="Probability",frame=F,cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale) 
    polygon(c(xmin,x0,xmax),c(0,y0,0),col=tcol,border=NA)
    
    
    # plot typical sample
    hist(x, 
         breaks=seq(xmin,xmax,length.out=50),
         main="Typical Sample",
         warn.unused = FALSE,
         col=tcol,
         border=tcol,
         xlab="",
         cex.lab=tscale,
         cex.axis=tscale,
         cex.main=tscale,
         cex.sub=tscale)

    # plot list of sample means with the latest sample highlighted and N(mu,sigma^2/n)
    breaks_mh=seq(xmin,xmax,length.out=100);
    y0 = dnorm(x0,switch(dist,
                         norm = 0,
                         unif = 0.5,
                         lnorm = exp(1/2),
                         exp = 1,
                         0),
               switch(dist,
                      norm = 1/sqrt(n),
                      unif = 1/sqrt(12)/sqrt(n),
                      lnorm = sqrt((exp(1)-1)*(exp(1)))/sqrt(n),
                      exp = 1/sqrt(n),
                      0))
    y0=y0/sum(y0)*length(means)*mean(diff(breaks_mh))/mean(diff(x0))
    
    nh<-hist(means,
             breaks=breaks_mh,
             main="Sample Means",
             warn.unused = FALSE,
             col=tcol,
             border=tcol,
             xlab="",
             cex.lab=tscale,
             cex.axis=tscale,
             cex.main=tscale,
             cex.sub=tscale)
    hist(mean(x,na.rm=TRUE),
         breaks=breaks_mh,
         col=acol,
         border=acol,
         add=TRUE,
         ylim=c(0,max(y0,max(nh$counts))))
    points(x0,y0,type="l",lwd=2)
    print(input$resample)
  },width=600,height=600)
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
})
