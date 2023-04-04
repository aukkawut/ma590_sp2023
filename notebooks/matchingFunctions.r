plotPS <- function(mod=NULL,ps=NULL,md=NULL,treatment=NULL,caliper,labels=FALSE,Zfuzz,...){
    oldPar <- par(mar=c(5.1,4.1,4.1,4.1))

    if(is.null(md)){
	    Zname <- as.character(mod$formula[2])
        Covnames <- names(coef(mod))[-1]
      #	md <- mod$data[,Covnames]
	    md <- mod$data
	    md$Z <- mod$data[[Zname]]
    }
    else    if(!is.null(treatment)){
        if(is.integer(treatment)) names(md)[treatment] <- 'Z'
        if(is.character(treatment)) names(md)[which(names(md)==treatment)] <- 'Z'
    }

    if(missing(Zfuzz)){
        treatFuzz <- sample(seq(-0.19,0.19,length=sum(md$Z)))
        Zfuzz <- md$Z+(1-md$Z)*runif(nrow(md),-0.2,0.2)
        Zfuzz[md$Z==1] <- 1+treatFuzz
    }
    if(is.null(mod)){
      md$logOdds <- ps
    } else md$logOdds <- mod$linear

    ## md$Z <- as.factor(md$Z)

    ## p <- ggplot(md,aes(Z,logOdds,group=Z))+
    ##   geom_boxplot()+
    ##   geom_jitter(width=0.25)+
    ##   ylab('Estimated Log Odds of Adoption')

    plot(Zfuzz,md$logOdds,xlim=c(-0.5,1.5),xaxt='n',
         ylab='Estimated Log Odds of Adoption',xlab=NA,pch=16,...)
    if(!missing(caliper)) errbar(Zfuzz[md$Z==1],md$logOdds[md$Z==1],
           md$logOdds[md$Z==1]+caliper*sd(md$logOdds),
           md$logOdds[md$Z==1]-caliper*sd(md$logOdds),add=TRUE)
    if(labels) text(Zfuzz[md$Z==1],md$logOdds[md$Z==1],rownames(md)[md$Z==1])
    axis(side=1,at=c(0,1),labels=c('Control','Treatment'))
    rtLbls <- seq(min(md$logOdds),max(md$logOdds),length=5)
    axis(side=4,at=rtLbls,
         labels=round((rtLbls-mean(md$logOdds))/sd(md$logOdds),1))
    mtext('Standardized Log Odds',side=4,line=3)
    par(oldPar)
        invisible(Zfuzz)
#    p
}

plotMatch <- function(match,mod=NULL,md=NULL,ps=NULL,treatment=NULL,Zfuzz){
    if(missing(Zfuzz))
        Zfuzz <- plotPS(mod,ps,md,treatment)
    else plotPS(mod,ps,md,treatment,Zfuzz=Zfuzz)
    if(is.null(md)&is.null(treatment)){
	    Zname <- as.character(mod$formula[2])
      Covnames <- names(coef(mod))[-1]
	#	data <- mod$data[,Covnames]
	    md <- mod$data
	    md$Z <- md[[Zname]]
    }
    else if(!is.null(treatment)){
        if(is.integer(treatment)) names(md)[treatment] <- 'Z'
        if(is.character(treatment)) names(md)[which(names(md)==treatment)] <- 'Z'
    }


    require(graphics)
    Zfuzz <- Zfuzz[!is.na(match)]
    md <- md[!is.na(match),]
    if(is.null(ps)) ps <- mod$linear
    ps <- ps[!is.na(match)]
    match <- match[!is.na(match)]
    colors <- 1:length(unique(match))
    drawSegs <- function(matchNum){
        m <- unique(match)[matchNum]
        x0 <- Zfuzz[match==m & md$Z==1]
        x1 <- Zfuzz[match==m & md$Z==0]
        y1 <- ps[match==m &md$Z==0]
        y0 <- ps[match==m &md$Z==1]
        segments(x0,y0,x1,y1,col=colors[matchNum])
        points(c(x0,x1),c(y0,y1),col=colors[matchNum])
        return(0)
    }
    aintNoThang <- vapply(1:length(unique(match)),drawSegs,0)

}

wilcox.pairmatch <- function(Y,Z,match,...){
    Y <- Y[!is.na(match)]
    Z <- Z[!is.na(match)]
    match <- match[!is.na(match)]

    matchLevs <- unique(match)
    treated <- sapply(matchLevs, function(m) Y[match==m & Z==1])
    control <- sapply(matchLevs,function(m) Y[match==m & Z==0])

    wilcox.test(treated, control,paired=TRUE,conf.int=TRUE,...)
}
