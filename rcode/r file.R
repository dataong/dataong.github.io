#CHAPTER5

#177p

options(digits=5)

cor <- c(0.4196,0.4172,0.4237,0.4182,0.4324,0.4365,0.4354,0.4156,0.4172,0.4414)

m <- mean(cor)
dev <- cor - m #편차
num <- sum(dev^2) #편차제곱
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
var(radius) #표본분산(r내장함수)
var.p(weight) #모분산
var(weight) #표본분산(r내장함수)


var.n <- function(x){
	m <- mean(x)
	l <- length(x)
	mvar <- sum((x-m)^2)/l
	msd <- sqrt(mvar)
	mcv <- msd/m #변동계수 표준편차/평균
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


#192p-유효성비교 
x <- seq(-3,3,by=0.01)
y <- dnorm(x) #정규분포 확률값지정(P(X=x))
y.1 <- dnorm(x,sd=sqrt(1/3))
y.2 <- dnorm(x,sd=sqrt(7/18))

pnorm(0.1,sd=sqrt(1/3))-pnorm(-0.1,sd=sqrt(1/3)) #-0.1과 0.1사이에서 발생할 확률
pnorm(0.1,sd=sqrt(7/18))-pnorm(-0.1,sd=sqrt(7/18)) #-0.1과 0.1사이에서 발생할 확률

plot(x,y,type="l",ylim=c(0,0.8),axes=F,ylab="",lwd=3,col="yellow") #모집단 분포모양을 축 없이 작성(axes=F)
lines(x,y.1,col="red",lwd=3) #y.1분포를 빨간색으로 그림
lines(x,y.2,col="green",lty=2,lwd=3) #y.2분포를 초록색으로 그림
axis(1) #x축 생성

#194p
options(digits=3)
set.seed(1)
mean.seq <- function(x) { #y2평균 = (1*x1 + 2*x2 + 3*x3)/6 구하는 함수
	n <- length(x)
	sum <- 0
	n2 <- 0
	for(i in 1:n){
		newx <- i * x[i]
		sum <- sum + newx
		n2 <- n2 + i }
	return(sum/n2)}
y1 <- rep(NA,1000)
y2 <- rep(NA,1000)

for(i in 1:1000) { #표본추출 각각 1000번(표본평균1000개)
	smp <- rnorm(3)
	y1[i] <- mean(smp)
	y2[i] <- mean.seq(smp)
	}
n1 <- length(y1[(y1 > -0.1) & (y1 < 0.1)]) # 표본평균1000개 중 -0.1보다 크고 0.1보다 작은 갯수 
n2 <- length(y2[(y2 > -0.1) & (y2 < 0.1)]) 
data.frame(mean=mean(y1),var=var(y1),n=n1)
data.frame(mean=mean(y2),var=var(y2),n=n2)

par(mfrow=c(1,2))#화면나누기
hist(y1,probability=T,xlim=c(-2,2),ylim=c(0,0.65),main="(x1+x2+x3)/3",xlab="",col="orange",border="red") #히스토그램
hist(y2,probability=T,xlim=c(-2,2),ylim=c(0,0.65),main="(1*x1+2*x2+3*x3)/6",xlab="",col="orange",border="red")


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

#207p 모평균에 대한 95%신뢰구간(모평균 알 때)
set.seed(9)

n <- 10 #표본크기
x <- 1:100 #표본추출순서
y <- seq(-3,3,by=0.01) #y지정

smps <- matrix(rnorm(n*length(x)),ncol=n) #1000개의 난수 생성, 열개수가 10
smps

xbar <- apply(smps, 1, mean) #각 행별 mean 적용한 변수를 xbar에 저장, 1은 행/2는 열
xbar

se <- 1/sqrt(10) #표준오차 구하기, 표준정규분포에선 표준오차가 1
alpha <- 0.05 #유의수준 지정
z <- qnorm(1-alpha/2) #임계값 구하기 
ll <- xbar - z*se #하한100개
ul <- xbar + z*se #상한100게

plot(y,type="n",xlab="trial",ylab="z",main="95% confidence interval for population mean",xlim=c(1,100),ylim=c(-1.5,1.5),cex.lab=1.8)#빈영역(type="n")그리기, cex.lab은 기호의 크기
abline(h=0, col="red", lty=2) #abline직선, h=0가로선
l.c <- rep(NA, length(x)) 
l.c <- ifelse(ll * ul >0, "red","black") #신뢰구간이 평균을 포함하면 검은색, 아니면 빨간색
arrows(1:length(x),ll,1:length(x),ul,code=3,angle=90,length=0.02,col=l.c,lwd=1.5) #각도 90, code-arrow의 모양

#211p 모평균에 대한 95% 신뢰구간(모평균 모를 때)
ci.t <- function(x, alpha = 0.05) {
	n <- length(smp)
	m <- mean(x)
	s <- sd(x)
	t <- qt(1-(alpha/2), df = n-1) #t분포 이용
	ll <- m - t*(s/sqrt(n)) #하한
	ul <- m + t*(s/sqrt(n)) #상한
	ci <- c(1-alpha,ll,m,ul)
	names(ci) <- c("confidence level", "lower limit","mean","upper limit") #ci벡터값에 이름 붙여주기
	return(ci)}

smp <- c(520,498,481,512,515,542,518,527,526)
ci.t(smp)
ci.t(smp,0.1)


#모비율 추정하는 사용자 정의 함수

ci.p <- function(x, alpha=0.05) {
	n <- length(x)
	p <- sum(x)/n
	se <- sqrt(p*(1-p)/n)#표준오차 구하기
	t <- qt(1-(alpha/2), df = n-1) #t분포 이용
	ll <- p - t*se #하한
	ul <- p + t*se #상한
	ci <- c(1-alpha,ll,p,ul,se) 
	names(ci) <- c("confidence level", "lower limit","p hat","upper limit","se") #ci벡터값에 이름 붙여주기
	return(ci) }

smp <- c(1,0,1,1,0)
ci.p(smp)


#217p - subset으로 데이터 추출하기 

data <- read.table("C:/Users/Administrator/Desktop/chapter2.txt",sep=",",na.strings=".")
tmp <- subset(data, data$V1 == 2 & data$V4 == 6) #성별2, 학력6인 값들 추출

#217p - subset으로 데이터 추출하기 

data <- read.table("C:/Users/Administrator/Desktop/chapter2.txt",sep=",",na.strings=".")
tmp <- subset(data, data$V1 == 2 & data$V4 == 6) #성별2, 학력6인 값들 추출


#220p 남자 어린이 키의 평균 검정

height <- c(1196,1340,1232,1184,1295,1247,1201,1182,1192,1287,1159,1160,1243,1264,1276)

mean(height)
sd(height)
h <- 1220
t <- (mean(height)-h)/(sd(height)/sqrt(length(height))) #검정통계량

#양측검정
#기각역구하기
a1 <- qt(0.975,14)#임계값
a2 <- qt(0.025,14)#임계값 
pvalue <- (1-pt(t,df=14))*2 #p값구하기(유의확률)

#우측단측검정
a3 <- qt(0.95,14) #임계값
pvalue2 <- 1-pt(t,14) #p값(유의확률)

#좌측단측검정
a3 <- qt(0.025,14) #임계값
pvalue2 <- 1-pt(t,14) #p값(유의확률)


#231p
weight <- c(3837,3334,2208,1745,2576,3208,37 46,3523,3430,3480,3116,3428,2184,2383,3500,3866,3542,3278)
h <- 2800
t <- (mean(weight)-h)/(sd(weight)/sqrt(length(weight))) #검정통계량
#양측검정
#기각역구하기
a1 <- qt(0.975,17)#임계값
a2 <- qt(0.025,17)#임계값 
pvalue <- (1-pt(t,df=17))*2 #p값구하기(유의확률)

t.test(weight, mu = 2800, alternative = "two.sided") #t-test

#우측단측검정
a3 <- qt(0.95,14) #임계값
pvalue2 <- 1-pt(t,14) #p값(유의확률)

t.test(weight, mu = 2800, alternative = "greater") #t-test

#좌측단측검정
a3 <- qt(0.025,14) #임계값
pvalue2 <- 1-pt(t,14) #p값(유의확률)

t.test(weight, mu = 2800, alternative = "less") #t-test 

#234p - 단일 모집단 가설검정
data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",header=F)
str(data)

names(data) <- c("time","gender","weight","minutes")
tmp <- subset(data, gender ==1)
weight <- tmp[[3]] #세번째 열을 weight에 저장

barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
(t.t <- (barx-h0)/(s/sqrt(n)))

alpha <- 0.05
(c.u <- qt(1-alpha, df=n-1))
(p.value <- 1 - pt(t.t, df=n-1))

t.test(weight, mu=2800, alternative="greater")

#240p - 모비율 검정
tmp <- read.table("C:/Users/user/Desktop/source/Chapter06/data/restitution.txt",header=T)
rel <- ifelse(tmp$rst < 0.4134|tmp$rst >0.4374,1,0)

n <- length(rel) #자료개수
nos <- sum(rel) 
sp <- nos / n
hp <- 0.1
(z <- (sp-hp)/sqrt(hp*(1-hp)/n)) #검정통계량

alpha <- 0.05
(c.u <- qnorm(1-alpha)) #기각역
(p.value <- 1- pnorm(z))

prop.test(nos,n,p=0.1,alternative="greater",correct=FALSE)

#데이터 프레임 다루기

data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",header=F)

nrow(data) #행개수
ncol(data) #열개수
str(data) #지료의 구조 확인

head(data)
tail(data)

names(data) #열의 이름 확인
names(data) <- c("time","gender","weight","minutes") #데이터 열 이름 바꾸기
names(data)
names(data)[1] <- "time.24hrs" #데이터 첫번째 열 이름 바꾸기
names(data)

row.names(data) #행번호를 관리

#열추출하는 다양한 방법
g1 <- data$gender #$을 이용한 열 지정
str(g1) 
g2 <- data[,2] #[]을 이용한 열 지정
str(g2)
g3 <- data["gender"] #["변수명"]을 이용한 열 지정
str(g3)
g4 <- data[[2]] #[[]]을 이용한 열 지정
str(g4)
g5 <- data[["gender"]] #[[]]을 이용한 열 지정
str(g5)
#복수의 열 추출
gg1 <- data[,c(2,4)]
str(gg1)
gg2<- data[c("gender","minutes")]
str(gg2)

#조건에 맞는 행 선택하기
#gender==2의 자료 가져오기
data[data$gender==2,]
str(data[data$gender==2,])

subset(data,gender==2)
str(subset(data,gender==2))

#gender == 2이고 평균체중보다 큰 자료만 가져오기
male.m <- mean(data$weight)
data[data$gender==2 & data$weight > male.m,]
str(data[data$gender==2 & data$weight > male.m,])
subset(data, (gender ==2) & (weight > male.m))
str(subset(data, (gender ==2) & (weight > male.m)))

#조건에 맞는 행과 열 선택
str(data[data$gender==2 & data$weight > male.m, c(2,4)])
str(subset(data, (gender==2)&(weight>male.m),select=c(2,4)))

#데이터 프레임 저장
chapter7 <- data[,c(2,3)]
write.table(chapter7,"C:/Users/Administrator/Desktop/chapter7.txt",row.names=FALSE)

#데이터 정제하기
ad <- read.csv("C:/Users/Administrator/Desktop/age.data.csv",header=T)
str(ad)
head(ad)
tail(ad)
summary(ad)

#결측값 처리
ad$score <- ifelse(ad$score==99,NA,ad$score) #99값을 결측값으로 바꾸기
summary(ad)

ad2<-read.csv("C:/Users/Administrator/Desktop/age.data.csv",header=T,na.strings=c("99")) #데이터 불러올 떄 문자열 결측처리
summary(ad2)

mean(ad$score)
mean(ad$score,na.rm=T) #결측값 제거하고 평균구하기

#결측여부 확인
is.na(c(1,NA,3,NA,5))

nonna.sum <- sum(ad$score[!is.na(ad$score)]) #score가 결측값이 아닌 것들의 합
nonna.length <- length(ad$score([!is.na(ad$score)]) #결측값들이 아닌 것들의 원소 개수 
nonna.sum / nonna.length


#factor변환
ad$scale <- factor(ad$scale) #범주형 자료로 바꿔줌
ad$sex <- factor(ad$sex) #범주형 자료로 바꿔줌
str(ad)
summary(ad)

#범주의 값이 1일 때 통계량
length(ad$age[ad$scale=="1"]) #scale이 1일 때 age자료의 길이 
mean(ad$age[ad$scale=="1"]) 
sd(ad$age[ad$scale=="1"]) 

#summaryby함수 
install.packages("doBy")
library(doBy)
summaryBy(age~scale, data=ad, FUN=c(length))
summaryBy(age~scale, data=ad, FUN=c(mean,sd),na.rm=TRUE)


#266p
data <- read.table("C:/Users/Administrator/Desktop/chapter7.txt", header=T)
var.test(data$weight~data$gender) #등분산성분석(성별에 따른 몸무게 비교)

#269p-두 모집단의 평균비교
t.test(data$weight ~ data$gender, mu=0, alternative="less", var.equal=TRUE)

#274p
data <- read.csv("C:/Users/Administrator/Desktop/01.anorexia.csv", header=T)
str( data )

#차이에 대한 검정통계량(직접구하는 방법)
n <- length(data$Prior - data$Post)
m <- mean( data$Prior - data$Post )
s <- sd (data$Prior - data$Post)
( t.t <- m/(s / sqrt(n)) )

#두 모집단 평균비교(r함수)
t.test(data$Prior, data$Post, paired=T, alternative="less")


#282p-지역구모별로 응답자의 나이 평균비교
ad <-read.csv("C:/Users/Administrator/Desktop/age.data.csv",header=T,na.strings=c("99"))
ad$scale <- factor(ad$scale) #범주형 자료로 바꿔줌
ad$sex <- factor(ad$sex) #범주형 자료로 바꿔줌
str(ad)
summary(ad)

y1 <- ad$age[ad$scale=="1"]#지역규모별 나이저장
y2 <- ad$age[ad$sclae=="2"]
y3 <- ad$age[ad$scale=="3"]

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum((y1-y1.mean)^2) #각각의 오차제곱합(SSE)
sse.2 <- sum((y2-y2.mean)^2)
sse.3 <- sum((y3-y3.mean)^2)

(sse <- sse.1+sse.2+sse.3) #최종 오차제곱합
(dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1)) #SSE의 자유도(n-k)

#283p-SSt구하기
y <- mean(ad$age)

sst.1 <- sum((y1.mean-y)^2)*length(y1)
sst.2 <- sum((y2.mean-y)^2)*length(y2)
sst.3 <- sum((y3.mean-y)^2)*length(y3)

(sst <- sst.1 + sst.2 + sst.3)
(dft <- length(levels(ad$scale))-1) #SSt자유도(k-1)


#284p-SSR구하기
(tsq <- sum((ad$age-y)^2) ) #SST
(ss <- sst + sse) #SST

#285p
mst <- sst/dft
mse <- sse/dfe
(f.t <- mst/mse) #검정통계량
#286p
(f.t <- mst/mse) #검정통계량
#287p
alpha <- 0.05 #유의수준설정
(tol <- qf(1-alpha,2,147)) #임계값구하기

#286p
(p.value <- 1-pf(f.t,2,147)) #p-value구하기

#287p-r함수로 평균비교
ow <- lm(age~scale,data=ad) #scale에 따른 age
anova(ow) 


names <- c("고길동","둘리","영희")
gender <- c("2","2","1")

(names <- c(names,"희동이")) #문자열 추가
(gender <- c(gender,"남자"))

gender
str(gender)
gender[5] <- "여자"
gender
f.gender <- factor(gender)
f.gender
str(f.gender)
levels(f.gender) #factor형 자료의 수준 확인
f.gender[6] <- "여" #factor형으로 변수 수준 정해지면 추가안됨
f.gender

answer <- c(2,2,3,2,3,4,4,4,3,4)
f.answer <- factor(answer)
str(f.answer)

f.answer <- factor(answer,levels=c(1,2,3,4,5)) #level직접 지정
str(f.answer)

o.f.answer <- factor(answer, levels=c(1,2,3,4,5), ordered = TRUE)
str(o.f.answer)
o.f.answer

o.f.answer <- factor(answer, levels=c(1,2,3,4,5), ordered = TRUE, labels=c("매우만족","불만족","보통이다","만족","매우만족"))
str(o.f.answer)
o.f.answer

names <- c("고길동", "둘리", "영희")
gender <- c("2", "2", "1")

characters <- data.frame(name=names, gender=gender)
str(characters)

characters <- data.frame(name=names, gender=gender, 
                         stringsAsFactors=FALSE) #factor형으로 저장할 지
str(characters)
characters <- transform(
                characters, 
                f.gender=factor(gender, 
                         levels=c("1", "2"), labels=c("여자", "남자")))
str(characters)
characters

sns <- read.csv("E:/통계프로그래밍/source/Chapter07/data/snsbyage.csv", header=T)
str( sns )

sns.c <- read.csv("E:/통계프로그래밍/source/Chapter07/data/snsbyage.csv", header=T, stringsAsFactors=FALSE)
str( sns.c )

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1, 2, 3), 
                            labels=c("20대", "30대", "40대")))

sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F", "T", "K", "C", "E"), 
                            ordered=TRUE))

str(sns.c)

age.c.tab <- table(sns.c$age.c)
str(age.c.tab)
age.c.tab
margin.table(age.c.tab)
addmargins(age.c.tab)
prop.table(age.c.tab)

prop.table( age.c.tab)


c.tab <- table(sns.c$age.c, sns.c$service.c)
str(c.tab)
c.tab

margin.table(c.tab)
margin.table(c.tab, margin=1)
margin.table(c.tab, margin=2)

addmargins(c.tab)
addmargins(c.tab, margin=1)
addmargins(c.tab, margin=2)

apply(c.tab, 1, mean)
apply(c.tab, 2, mean)

prop.table(c.tab)
prop.table(c.tab, margin=1)
prop.table(c.tab, margin=2)


## xtabs()
xt.age <- xtabs(~age.c, data=sns.c)
str(xt.age)
xt.age
xt.sns <- xtabs(~age.c+service.c, data=sns.c)
xt.sns

s.data <- read.csv("E:/통계프로그래밍/source/Chapter07/data/xtab.count.csv", header=T)
s.data
xt.s.data <- xtabs(count~group+result, data=s.data)
xt.s.data


#306p-적합도 검정 
x <- c(315,101,108,32)
chisq.test(x,p=c(9,3,3,1)/16)

#309p-동질성 검정

c.tab <- table(sns.c$age.c, sns.c$service.c)
(a.n <- margin.table(c.tab, margin=1)) #연령별 사용자 수 margin
(s.n <- margin.table(c.tab, margin=2)) #서비스별 사용자 수 margin
(s.p <- s.n/margin.table(c.tab)) #서비스별 이용자 수 / 전체 이용자
expected <- a.n %*% t(s.p) #s.p를 transpose하고 a.n과 행렬곱셈
(t.t <- sum((c.tab - expected)^2/expected)) #검정통계량
qchisq(0.95,df=8) #기각값
1-pchisq(t.t,df=8) #유의확률

chisq.test(c.tab)

#319p-독립성 검정
ucba.tab <- apply(UCBAdmissions,c(1,2),sum)
ucba.tab

(a.n <- margin.table(ucba.tab, margin=1))
(g.n <- margin.table(ucba.tab, margin=2))

(a.p <- a.n / margin.table(ucba.tab))
(g.p <- g.n / margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p))) #기대도수

(t.t <- sum((ucba.tab - expected)^2/expected))#검정통계량

qchisq(0.95,df=1)
1-pchisq(t.t,df=1)

chisq.test(ucba.tab)


hf <- read.table("http://www.randomservices.org/random/data/Galton.txt",header=TRUE,stringsAsFactors=FALSE)
hf$Gender <- factor(hf$Gender,levels=c("M","F"))
hf.son <- subset(hf,Gender=="M")
hf.son <- hf.son[c("Father","Height")]

#333p

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father-f.mean)*(hf.son$Height-s.mean))#표본공분산 직접 구하기 
(cov.xy <- cov.num /(nrow(hf.son)-1))
cov(hf.son$Height,hf.son$Father) #표본공분산

(r.xy <- cov.xy/(sd(hf.son$Father)*sd(hf.son$Height))) #표본상관계수
cor(hf.son$Father,hf.son$Height) #표본상관계수


#339p - 회귀계수 추정

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Height - mean.y)*(hf.son$Father-mean.x))
sxx <- sum((hf.son$Father-mean.x)^2)

(b1 <- sxy/sxx)
(b0 <- mean.y - b1*mean.x)

lm(Height ~ Father, data=hf.son)

#344p - 회귀모형의 유의성 검정
out <- lm(Height ~ Father, data=hf.son)
anova(out)

qf(0.95,1,463) #5%의 기각값
1-pf(83.719,1,463) #유의확률 

#346p - 회귀계수의 유의성 검정
summary(out)

#독립변수-잔차 산점도(등분산성 확인)
plot( hf.son$Father, residuals(out), xlab="residuals", ylab="")
abline(h=0, col="red", lty=2)

#잔차의 정규확률그림
qqnorm(residuals(out), main="")
qqline(residuals(out), lty=2, col="red")

#정규성 검정
shapiro.test(residuals(out))







