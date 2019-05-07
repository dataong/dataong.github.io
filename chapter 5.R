#CHAPTER5

#177p

option(digit=5)

cor <- c(0.4196,0.4172,0.4237,0.4182,0.4324,0.4365,0.4354,0.4156,0.4172,0.4414)

m <- mean(cor)
dev <- cor - m
num <- sum(dev^2)
denom <- length(cor)
denom2 <- length(cor) - 1

(var.p <- num/denom) #모분산
(var.s <- num/denom2) #표본분산
var(cor) #표본분산




#179p - 모분산 구하는 사용자 정의 합수

option(digits=4)

var.p <- function(x) { #모분산 구하는 함수
	n <- length(x)
	m <- mean(x)
	num <- sum((x-m)^2)
	denom <- n
	var <- num/denom
	#return(c(m,var)) 평균과 분산을 함께 return
	return(print(paste("평균은",m,"분산은",var))) #만약에 cat쓰면 "\n"으로 엔터 해주기
	}

radius <- c(234,234,234,233,233,233,233,231,232,231)
weight <- c(146.3,146.4,144.1,146.7,145.2,144.1,143.3,147.3,146.7,147.3)

var.p(radius) #모분산
var(radius) #표본분산
var.p(weight) #모분산
var(weight) #표본분산


var.n <- function(x){
	m <- mean(x)
	l <- length(x)
	mvar <- sum((x-m)^2)/l
	msd <- sqrt(mvar)
	mcv <- msd/m
	return(cat("모평균:",m,"\n","모분산:",mvar,"\n","모표준편차:",msd,"\n","모변동계수:",mcv,"\n")) #"\n"이 줄바꿈
	}

var.n(radius)


#183p-결측값 처리하는 사용자정의 함수

options(digits=4)

var.p2 <- function(x, na.rm = FALSE) { #결측값 제거 하지 않는 것 기본값으로 정의
	if(na.rm == TRUE) { #결측값 제거해주세요
	x <- x[!is.na(x)] #결측값이 아닌 값들만 x에 저장
	}
	n <- length(x)
	m <- mean(x, na.rm=na.rm) #na.rm에 사용자가 정의한 na.rm(TRUE OR FALSE)를 전달
	num <- sum((x-m)^2,na.rm = na.rm) #na.rm에 사용자가 정의한 na.rm(TRUE OR FALSE)를 전달
	denom <- n
	var <- num/denom
	return(var)
	}


radius <- c(234,234,234,233,233,233,NA,231,232,231)
var.p2(radius)
var.p2(radius,na.rm=TRUE)

#예제 5-1,5-2하기

#201p 모비율 점추정
install.packages("prob")
library(prob)

n <- 3
smp.all <- rolldie(n) #주사위 세번 굴린 것을 smp.all에 저장
str(smp.all) 
head(smp.all, n=3)

is.even <- function(x) return(!x%%2) #짝수이면 !0 TRUE, 홀수이면 !0 FALSE

p.even <- function(x,s.size=3) { #각 표본별 표본비율 구하기
	return(sum(is.even(x))/s.size)
	}

var.p <- function(x) { #모분산 구하는 공식
	return(sum(x-mean(x))^2/length(x))
	}

phat <- apply(smp.all,1,p.even) #각 행별 짝수의 비율을 phat에 저장,1:가로방향/2:세로방향 
phat #6^3개의 표본비율 생김 

mean(phat)
(p.p <- 0.5)
var.p(phat)
(p.p*(1-p.p)/3)
sqrt(var.p(phat))

#207p 모평균에 대한 95%신뢰구간
set.seed(9)

n <- 10 #표본크기
x <- 1:100 #표본추출순서
y <- seq(-3,3,by=0.01) #y지정

smps <- matrix(rnorm(n*length(x)),ncol=n) #1000개의 난수 생성, 열개수가 10
smps

xbar <- apply(smps, 1, mean) #각 행별 mean 적용한 변수를 xbar에 저장
xbar

se <- 1/sqrt(10) #표준오차 구하기 
alpha <- 0.05 #유의수준 지정
z <- qnorm(1-alpha/2) #임계값 구하기 
ll <- xbar - z*se #하한
ul <- xbar + z*se #상한

plot(y,type="n",xlab="trial",ylab="z",main="95% confidence interval for population mean",xlim=c(1,100),ylim=c(-1.5,1.5),cex.lab=1.8)
abline(h=0, col="red", lty=2) #모집단 평균인 0을 붉은색으로
l.c <- rep(NA, length(x)) 
l.c <- ifelse(ll * ul >0, "red","black") #신뢰구간이 평균을 포함하면 검은색, 아니면 빨간색
arrows(1:length(x),ll,1:length(x),ul,code=3,angle=90,length=0.02,col=l.c,lwd=1.5) #각도 90, 화살표 시작과 끝점에 머리 생기도록 code=3 





	
