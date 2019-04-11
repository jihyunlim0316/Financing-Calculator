########################################################################
#################### GROUP 5 - FINANCING CALCULATOR ####################
########################################################################
rm(list = ls())
install.packages("stargazer")
library(stargazer)
library(plyr)
library(xtable)

## To make the Financing Calculator
F.Cal <- function() {
  
  ########################################################################
  ############################### INPUT ##################################
  ########################################################################
  ## (1) Loan Amount (loan.amt)
  loan.amt <- as.numeric(readline(prompt="Enter the Loan Amount (Unit:Won): "))
  ## (2) Repayment Period
  ## (per=year / com.p=num per year / com.n=total num / com.f=for function / com.i=inform)
  per=as.numeric(readline(prompt="Enter the Repayment Period (Year(s)): "))
  ## (3) Compounding Information
  cat("Choose the compounding period : 
      1=Monthly / 2=Quarterly / 3=Semi-annually / 4=Annually ")
  com.p=scan(n=1, quiet=TRUE)
  if (com.p==1) {
    com.n=12; com.f="months"; com.i="Monthly"
  } else if (com.p==2) {
    com.n=4; com.f="quarters"; com.i="Quarterly"
  } else if (com.p==3) {
    com.n=2; com.f="6 months"; com.i="Semi-Anually"
  } else {
    com.n=1; com.f="years"; com.i="Annually"
  }
  ## (4) APR (APR)
  APR=as.numeric(readline(prompt="Enter the Annual Percentage Rate (%): "))
  APR=APR/100
  ## (5) Repayment Type (p.typ)
  cat("Choose the repayment types: 
      1=Even Total Payment / 2=Even Principal Payment / 3=Balloon Payment") # Balloon : See the report
  p.typ=scan(n=1, quiet=TRUE)
  
  ####################### Form of Values #################################
  form <- function(x) {format(x, big.mark=",", scientific=FALSE)}
  c.form <- function(x) {paste(format(x, big.mark=",", scientific=FALSE), "Won")}
  ########################################################################
  
  ########################################################################
  ###################### (1) EVEN TOTAL PAYMENT ##########################
  ########################################################################  
  if (p.typ==1) {
    ## (6) Interest-Only Period (intonly.p= year)
    intonly.p <- as.numeric(readline(prompt=
                                       "If necessary, enter the Interest-Only Period (Year(s)): "))
    ## Number of Payments (num=Total comp # / intonly.n=comp # of Intonly / pay.n=comp # of Pay)
    if (com.p==1) {
      num=(intonly.p+per)*12; intonly.n=intonly.p*12; pay.n=per*12
    } else if (com.p==2) {
      num=(intonly.p+per)*4; intonly.n=intonly.p*4; pay.n=per*4
    } else if (com.p==3) {
      num=(intonly.p+per)*2; intonly.n=intonly.p*2; pay.n=per*2
    } else {
      num=intonly.p+per; intonly.n=intonly.p; pay.n=per
    }
    
    ## for Payment Schedule
    typ.1 <- function(loan.amt, num, com.n, APR) {
      r=APR/com.n
      ## Total Pay, P, Int, Unpaid B
      table.r=data.frame(matrix(0,num+1,4))
      table.r[1,4]=loan.amt
      if (intonly.p==0) {     # When int only period = 0
        for (i in 1:num+1) {
          table.r[i,1]=ceiling(loan.amt / (((1+r)^num-1) / (r*(1+r)^num)))
          table.r[i,3]=round(table.r[i-1,4]*r)
          table.r[i,2]=table.r[i,1] - table.r[i,3]
          table.r[i,4]=table.r[i-1,4] - table.r[i,2]
        }} else {             # When int only period > 0
          table.r[2:(intonly.n+1),1]=round(loan.amt*r)
          table.r[2:(intonly.n+1),3]=round(loan.amt*r)
          table.r[2:(intonly.n+1),4]=loan.amt
          for (i in 1:pay.n) {
            table.r[(intonly.n+1+i),1]=ceiling(loan.amt / (((1+r)^pay.n-1) / (r*(1+r)^pay.n)))
            table.r[(intonly.n+1+i),3]=round(table.r[(intonly.n+i),4]*r)
            table.r[(intonly.n+1+i),2]=table.r[(intonly.n+1+i),1] - table.r[(intonly.n+1+i),3]
            table.r[(intonly.n+1+i),4]=table.r[(intonly.n+i),4] - table.r[(intonly.n+1+i),2]
          }}
      # to make the last un.B zero
      table.r[num+1,2]=table.r[num,4]
      table.r[num+1,3]=table.r[num+1,1]-table.r[num+1,2]
      table.r[num+1,4]=0
      return(table.r)
    }
    table.r=typ.1(loan.amt, num, com.n, APR)
    ## Num, Dates
    table.l=data.frame(matrix(0,num+1,2))
    table.l[,1]=seq(0,num)
    table.l[,2]=seq(Sys.Date(),by=com.f,length=num+1)
    ## Total
    tt=c(c.form(sum(table.r[,1])), c.form(sum(table.r[,2])), c.form(sum(table.r[,3])))

    ## for Unpaid B Graph
    un.B=cbind(table.l[,1], round(table.r[,4]/10000))
    ## for Payment Graph
    graph=cbind(table.l[2:(num+1),1],round(table.r[2:(num+1),]/10000))
    
    ## Schedule Table
    table.r=data.frame(sapply(table.r,form))
    table1=cbind(table.l, table.r)
    colnames(table1)=c("Num", "Payment Dates", "Total Payment", "Principal", "Interest", "Unpaid Balance")
    
    ## Loan Data
    data=t(data.frame(c.form(loan.amt), paste(APR*100,"%"), 
                      paste(per," Year(s)"), paste(intonly.p, " Year(s)"), com.i, Sys.Date()))
    rownames(data)=c("Loan Amount", "Annual Percentage Rate", "Repayment Period",
                     "Interest-only Period", "Frequency of Payment", "Start Date")
    ## Summary
    summ=t(data.frame(paste((per+intonly.p), " Year(s)"), num, paste("(approx.) ",round(APR/com.n*100,3),"%"), 
                      paste(table1[2,3],"Won"), tt[3], tt[1]))
    rownames(summ)=c("Total Loan Period", "Number of Payments", "Rate(per period)", 
                     "Payment(per period)", "Total Interest Paid", "Total Payments")
    
    ## Plot(1) : Unpaid B
    plot(un.B[,2]~un.B[,1],data=un.B,type="s",
         main="Unpaid Balance over time", xlab="Number of Payments", ylab="Unit : 10,000 Won")
    ## Plot(2) : Payments
    plot(graph[,1], graph[,2], type="l", ylim=c(0, max(graph[,2])*1.1),
         main="Even Total Payment", xlab="Number of Payments", ylab="Unit :10,000 Won")
    mtext(paste("Loan amount=", c.form(loan.amt), "/ APR=", paste(APR*100,"% / "),
                "Loan Period=", per,"years;",com.i), side=3)
    lines(graph[,1], graph[, 3], lty=2, col="blue")
    lines(graph[,1], graph[, 4], lty=3, col="red")
    legend("right", legend=c("Total Payment","Principal","Interest"), 
           lty = c(1,2,3), col=c("black","blue","red"), cex=0.8)
    
    ###################### for Latex OUTPUT #########################
    ## Export to txt file
    sink("table1")
    ## Loan Data
    cat("Even Total Payment","\n")
    cat("----------------------------------------------","\n")
    cat("<Loan Data>", "\n")
    stargazer(data, summary=F, rownames=T)
    ## Summary
    cat("----------------------------------------------","\n")
    cat("<Summary>", "\n")
    stargazer(summ, summary=F, rownames=T)
    ## Schedule Table
    cat("----------------------------------------------","\n")
    cat("<Schedule>", "\n")
    stargazer(table1, summary=F, rownames=F)
    sink()
    
    ########################################################################
    ################### (2) EVEN PRINCIPAL PAYMENT #########################
    ######################################################################## 
  } else if (p.typ==2) {
    ## (6) Interest-Only Period (intonly.p= year)
    intonly.p <- as.numeric(readline(prompt=
                                       "If necessary, enter the Interest-Only Period (Year(s)): "))
    ## Number of Payments (num=Total comp # / intonly.n=comp # of Intonly / pay.n=comp # of Pay)
    if (com.p==1) {
      num=(intonly.p+per)*12; intonly.n=intonly.p*12; pay.n=per*12
    } else if (com.p==2) {
      num=(intonly.p+per)*4; intonly.n=intonly.p*4; pay.n=per*4
    } else if (com.p==3) {
      num=(intonly.p+per)*2; intonly.n=intonly.p*2; pay.n=per*2
    } else {
      num=intonly.p+per; intonly.n=intonly.p; pay.n=per
    }
    
    ## for Payment Schedule
    typ.2 <- function(loan.amt, num, com.n, APR) {
      r=APR/com.n
      ## Total Pay, P, Int, Unpaid B
      table.r=data.frame(matrix(0,num+1,4))
      table.r[1,4]=loan.amt
      if (intonly.p==0) {         # When int only period = 0
        for (i in 1:num+1) {
          table.r[i,2]=ceiling(loan.amt/num)
          table.r[i,3]=round(table.r[i-1,4]*r)
          table.r[i,4]=table.r[i-1,4]-table.r[i,2]
          table.r[i,1]=table.r[i,2]+table.r[i,3]
        }} else {               # When int only period > 0
          table.r[2:(intonly.n+1),1]=round(loan.amt*r)
          table.r[2:(intonly.n+1),3]=round(loan.amt*r)
          table.r[2:(intonly.n+1),4]=loan.amt
          for (i in 1:pay.n) {
            table.r[(intonly.n+1+i),2]=ceiling(loan.amt/pay.n)
            table.r[(intonly.n+1+i),3]=round(table.r[(intonly.n+i),4]*r)
            table.r[(intonly.n+1+i),4]=table.r[(intonly.n+i),4]-table.r[intonly.n+1+i,2]
            table.r[(intonly.n+1+i),1]=table.r[(intonly.n+1+i),2]+table.r[(intonly.n+1+i),3]
          }}
      # to make the last un.B zero
      table.r[num+1,2]=table.r[num,4]
      table.r[num+1,1]=table.r[num+1,2]+table.r[num+1,3]
      table.r[num+1,4]=0
      return(table.r)
    }
    table.r=typ.2(loan.amt, num, com.n, APR)
    ## Num, Dates
    table.l=data.frame(matrix(0,num+1,2))
    table.l[,1]=seq(0,num)
    table.l[,2]=seq(Sys.Date(),by=com.f,length=num+1)
    ## Total
    tt=c(c.form(sum(table.r[,1])), c.form(sum(table.r[,2])), c.form(sum(table.r[,3])))
    
    ## for Unpaid B Graph
    un.B=cbind(table.l[,1], round(table.r[,4]/10000))
    ## for Payment Graph
    graph=cbind(table.l[2:(num+1),1],round(table.r[2:(num+1),]/10000))
    
    ## Schedule Table
    table.r=data.frame(sapply(table.r,form))
    table2=cbind(table.l, table.r)
    colnames(table2)=c("Num", "Payment Dates", "Total Payment", "Principal", "Interest", "Unpaid Balance")
    
    ## Loan Data
    data=t(data.frame(c.form(loan.amt),paste(APR*100,"%"),
                      paste(per," Year(s)"),paste(intonly.p," Year(s)"),com.i,Sys.Date()))
    rownames(data)=c("Loan Amount", "Annual Percentage Rate", "Repayment Period",
                     "Interest-only Period", "Frequency of Payment", "Start Date")
    ## Summary
    summ=t(data.frame(paste((per+intonly.p)," Year(s)"), num, paste("(approx.) ", round(APR/com.n*100,3), "%"), 
                      paste(table2[2,3],"Won"), paste(table2[num+1,3],"Won"), tt[3], tt[1]))
    rownames(summ)=c("Total Loan Period", "Number of Payments", "Rate(per period)", "First Payment",
                     "Last Payment", "Total Interest Paid", "Total Payments")
    
    ## Plot(1) : Unpaid B
    plot(un.B[,2]~un.B[,1],data=un.B,type="s",
         main="Unpaid Balance over time", xlab="Numbers of Payments", ylab="Unit : 10,000 Won")
    ## Plot(1) : Payments
    plot(graph[,1], graph[,2], type="l", ylim=c(0, max(graph[,2])*1.1), pch=10,
         main="Even Principal Payment", xlab="Number of Payments", ylab="Unit :10,000 Won")
    mtext(paste("Loan amount=", c.form(loan.amt), "/ APR=", paste(APR*100,"% / "),
                "Loan Period=", per,"years;",com.i), side=3)
    lines(graph[,1], graph[, 3], lty=2, col="blue")
    lines(graph[,1], graph[, 4], lty=3, col="red")
    legend("right", legend=c("Total Payment","Principal","Interest"), 
           lty = c(1,2,3), col=c("black","blue","red"), cex=0.8)
    
    ###################### for Latex OUTPUT #########################
    ## Export to txt file
    sink("table2")
    ## Loan Data
    cat("Even Principal Payment","\n")
    cat("----------------------------------------------","\n")
    cat("<Loan Data>", "\n")
    stargazer(data, summary=F, rownames=T)
    ## Summary
    cat("----------------------------------------------","\n")
    cat("<Summary>", "\n")
    stargazer(summ, summary=F, rownames=T)
    ## Schedule Table
    cat("----------------------------------------------","\n")
    cat("<Schedule>", "\n")
    stargazer(table2, summary=F, rownames=F)
    sink()
    
    ########################################################################
    ######################### (3) BALLOON PAYMENT ###########################
    ########################################################################
  } else {
    ## Number of Payments (num=Total comp #)
    if (com.p==1) {
      num=per*12
    } else if (com.p==2) {
      num=per*4
    } else if (com.p==3) {
      num=per*2
    } else {
      num=per
    }
    ## for Payment Schedule
    typ.3 <- function(loan.amt, num, com.n, APR) {
      r=APR/com.n
      ## Total Pay, P, Int, Unpaid B
      table.r=data.frame(matrix(0,num+1,4))
      table.r[1,4]=loan.amt
      table.r[2:num,1]=loan.amt*r
      table.r[2:num,3]=loan.amt*r
      table.r[2:num,4]=loan.amt
      table.r[num+1,1:4]=c(loan.amt*(1+r), loan.amt, loan.amt*r, 0)
      return(table.r)
    }
    table.r=typ.3(loan.amt, num, com.n, APR)
    ## Num, Dates
    table.l=data.frame(matrix(0,num+1,2))
    table.l[,1]=seq(0,num)
    table.l[,2]=seq(Sys.Date(),by=com.f,length=num+1)
    ## Total
    tt=c(c.form(round(sum(table.r[,1]))), 
         c.form(round(sum(table.r[,2]))), c.form(round(sum(table.r[,3]))))
    
    ## Schedule Table
    table.r=data.frame(sapply(table.r,function(x){form(ceiling(x))}))
    table3=cbind(table.l, table.r)
    colnames(table3)=c("Num", "Payment Dates", "Total Payment", "Principal", "Interest", "Unpaid Balance")
    
    ## Loan Data
    data=t(data.frame(c.form(loan.amt), paste(APR*100,"%"), paste(per," Year(s)"), com.i, Sys.Date()))
    rownames(data)=c("Loan Amount", "Annual Percentage Rate", "Repayment Period", 
                     "Frequency of Payment", "Start Date")
    ## Summary
    summ=t(data.frame(num, paste("(approx.) ", round(APR/com.n*100,3), "%"), tt[3], tt[1]))
    rownames(summ)=c("Number of Payments", "Rate(per period)", "Total Interest Paid",
                     "Total Payments")
    
    ###################### for Latex OUTPUT #########################
    ## Export to txt file
    sink("table3")
    ## Loan Data
    cat("Balloon Payment","\n")
    cat("----------------------------------------------","\n")
    cat("<Loan Data>", "\n")
    stargazer(data, summary=F, rownames=T)
    ## Summary
    cat("----------------------------------------------","\n")
    cat("<Summary>", "\n")
    stargazer(summ, summary=F, rownames=T)
    ## Schedule Table
    cat("----------------------------------------------","\n")
    cat("<Schedule>", "\n")
    stargazer(table3, summary=F, rownames=F)
    sink()
  }
}

