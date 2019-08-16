#CHAPTER5

#177p

options(digits=5)

cor <- c(0.4196,0.4172,0.4237,0.4182,0.4324,0.4365,0.4354,0.4156,0.4172,0.4414)

m <- mean(cor)
dev <- cor - m #����
num <- sum(dev^2) #��������
denom <- length(cor)
denom2 <- length(cor) - 1

(var.p <- num/denom) #��л�
(var.s <- num/denom2) #ǥ���л�
var(cor) #ǥ���л�




#179p - ��л� ���ϴ� ����� ���� �ռ�

option(digits=4)

var.p <- function(x) { #��л� ���ϴ� �Լ�
	n <- length(x)
	m <- mean(x)
	num <- sum((x-m)^2)
	denom <- n
	var <- num/denom
	#return(c(m,var)) ��հ� �л��� �Բ� return
	return(print(paste("�����",m,"�л���",var))) #���࿡ cat���� "\n"���� ���� ���ֱ�
	}

radius <- c(234,234,234,233,233,233,233,231,232,231)
weight <- c(146.3,146.4,144.1,146.7,145.2,144.1,143.3,147.3,146.7,147.3)

var.p(radius) #��л�
var(radius) #ǥ���л�(r�����Լ�)
var.p(weight) #��л�
var(weight) #ǥ���л�(r�����Լ�)


var.n <- function(x){
	m <- mean(x)
	l <- length(x)
	mvar <- sum((x-m)^2)/l
	msd <- sqrt(mvar)
	mcv <- msd/m #������� ǥ������/���
	return(cat("�����:",m,"\n","��л�:",mvar,"\n","��ǥ������:",msd,"\n","�𺯵����:",mcv,"\n")) #"\n"�� �ٹٲ�
	}

var.n(radius)


#183p-������ ó���ϴ� ��������� �Լ�

options(digits=4)

var.p2 <- function(x, na.rm = FALSE) { #������ ���� ���� �ʴ� �� �⺻������ ����
	if(na.rm == TRUE) { #������ �������ּ���
	x <- x[!is.na(x)] #�������� �ƴ� ���鸸 x�� ����
	}
	n <- length(x)
	m <- mean(x, na.rm=na.rm) #na.rm�� ����ڰ� ������ na.rm(TRUE OR FALSE)�� ����
	num <- sum((x-m)^2,na.rm = na.rm) #na.rm�� ����ڰ� ������ na.rm(TRUE OR FALSE)�� ����
	denom <- n
	var <- num/denom
	return(var)
	}


radius <- c(234,234,234,233,233,233,NA,231,232,231)
var.p2(radius)
var.p2(radius,na.rm=TRUE)


#192p-��ȿ���� 
x <- seq(-3,3,by=0.01)
y <- dnorm(x) #���Ժ��� Ȯ��������(P(X=x))
y.1 <- dnorm(x,sd=sqrt(1/3))
y.2 <- dnorm(x,sd=sqrt(7/18))

pnorm(0.1,sd=sqrt(1/3))-pnorm(-0.1,sd=sqrt(1/3)) #-0.1�� 0.1���̿��� �߻��� Ȯ��
pnorm(0.1,sd=sqrt(7/18))-pnorm(-0.1,sd=sqrt(7/18)) #-0.1�� 0.1���̿��� �߻��� Ȯ��

plot(x,y,type="l",ylim=c(0,0.8),axes=F,ylab="",lwd=3,col="yellow") #������ ��������� �� ���� �ۼ�(axes=F)
lines(x,y.1,col="red",lwd=3) #y.1������ ���������� �׸�
lines(x,y.2,col="green",lty=2,lwd=3) #y.2������ �ʷϻ����� �׸�
axis(1) #x�� ����

#194p
options(digits=3)
set.seed(1)
mean.seq <- function(x) { #y2��� = (1*x1 + 2*x2 + 3*x3)/6 ���ϴ� �Լ�
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

for(i in 1:1000) { #ǥ������ ���� 1000��(ǥ�����1000��)
	smp <- rnorm(3)
	y1[i] <- mean(smp)
	y2[i] <- mean.seq(smp)
	}
n1 <- length(y1[(y1 > -0.1) & (y1 < 0.1)]) # ǥ�����1000�� �� -0.1���� ũ�� 0.1���� ���� ���� 
n2 <- length(y2[(y2 > -0.1) & (y2 < 0.1)]) 
data.frame(mean=mean(y1),var=var(y1),n=n1)
data.frame(mean=mean(y2),var=var(y2),n=n2)

par(mfrow=c(1,2))#ȭ�鳪����
hist(y1,probability=T,xlim=c(-2,2),ylim=c(0,0.65),main="(x1+x2+x3)/3",xlab="",col="orange",border="red") #������׷�
hist(y2,probability=T,xlim=c(-2,2),ylim=c(0,0.65),main="(1*x1+2*x2+3*x3)/6",xlab="",col="orange",border="red")


#201p ����� ������
install.packages("prob")
library(prob)

n <- 3
smp.all <- rolldie(n) #�ֻ��� ���� ���� ���� smp.all�� ����
str(smp.all) 
head(smp.all, n=3)

is.even <- function(x) return(!x%%2) #¦���̸� !0 TRUE, Ȧ���̸� !0 FALSE

p.even <- function(x,s.size=3) { #�� ǥ���� ǥ������ ���ϱ�
	return(sum(is.even(x))/s.size)
	}

var.p <- function(x) { #��л� ���ϴ� ����
	return(sum(x-mean(x))^2/length(x))
	}

phat <- apply(smp.all,1,p.even) #�� �ະ ¦���� ������ phat�� ����,1:���ι���/2:���ι��� 
phat #6^3���� ǥ������ ���� 

mean(phat)
(p.p <- 0.5)
var.p(phat)
(p.p*(1-p.p)/3)
sqrt(var.p(phat))

#207p ����տ� ���� 95%�ŷڱ���(����� �� ��)
set.seed(9)

n <- 10 #ǥ��ũ��
x <- 1:100 #ǥ���������
y <- seq(-3,3,by=0.01) #y����

smps <- matrix(rnorm(n*length(x)),ncol=n) #1000���� ���� ����, �������� 10
smps

xbar <- apply(smps, 1, mean) #�� �ະ mean ������ ������ xbar�� ����, 1�� ��/2�� ��
xbar

se <- 1/sqrt(10) #ǥ�ؿ��� ���ϱ�, ǥ�����Ժ������� ǥ�ؿ����� 1
alpha <- 0.05 #���Ǽ��� ����
z <- qnorm(1-alpha/2) #�Ӱ谪 ���ϱ� 
ll <- xbar - z*se #����100��
ul <- xbar + z*se #����100��

plot(y,type="n",xlab="trial",ylab="z",main="95% confidence interval for population mean",xlim=c(1,100),ylim=c(-1.5,1.5),cex.lab=1.8)#�󿵿�(type="n")�׸���, cex.lab�� ��ȣ�� ũ��
abline(h=0, col="red", lty=2) #abline����, h=0���μ�
l.c <- rep(NA, length(x)) 
l.c <- ifelse(ll * ul >0, "red","black") #�ŷڱ����� ����� �����ϸ� ������, �ƴϸ� ������
arrows(1:length(x),ll,1:length(x),ul,code=3,angle=90,length=0.02,col=l.c,lwd=1.5) #���� 90, code-arrow�� ���

#211p ����տ� ���� 95% �ŷڱ���(����� �� ��)
ci.t <- function(x, alpha = 0.05) {
	n <- length(smp)
	m <- mean(x)
	s <- sd(x)
	t <- qt(1-(alpha/2), df = n-1) #t���� �̿�
	ll <- m - t*(s/sqrt(n)) #����
	ul <- m + t*(s/sqrt(n)) #����
	ci <- c(1-alpha,ll,m,ul)
	names(ci) <- c("confidence level", "lower limit","mean","upper limit") #ci���Ͱ��� �̸� �ٿ��ֱ�
	return(ci)}

smp <- c(520,498,481,512,515,542,518,527,526)
ci.t(smp)
ci.t(smp,0.1)


#����� �����ϴ� ����� ���� �Լ�

ci.p <- function(x, alpha=0.05) {
	n <- length(x)
	p <- sum(x)/n
	se <- sqrt(p*(1-p)/n)#ǥ�ؿ��� ���ϱ�
	t <- qt(1-(alpha/2), df = n-1) #t���� �̿�
	ll <- p - t*se #����
	ul <- p + t*se #����
	ci <- c(1-alpha,ll,p,ul,se) 
	names(ci) <- c("confidence level", "lower limit","p hat","upper limit","se") #ci���Ͱ��� �̸� �ٿ��ֱ�
	return(ci) }

smp <- c(1,0,1,1,0)
ci.p(smp)


#217p - subset���� ������ �����ϱ� 

data <- read.table("C:/Users/Administrator/Desktop/chapter2.txt",sep=",",na.strings=".")
tmp <- subset(data, data$V1 == 2 & data$V4 == 6) #����2, �з�6�� ���� ����

#217p - subset���� ������ �����ϱ� 

data <- read.table("C:/Users/Administrator/Desktop/chapter2.txt",sep=",",na.strings=".")
tmp <- subset(data, data$V1 == 2 & data$V4 == 6) #����2, �з�6�� ���� ����


#220p ���� ��� Ű�� ��� ����

height <- c(1196,1340,1232,1184,1295,1247,1201,1182,1192,1287,1159,1160,1243,1264,1276)

mean(height)
sd(height)
h <- 1220
t <- (mean(height)-h)/(sd(height)/sqrt(length(height))) #������跮

#��������
#�Ⱒ�����ϱ�
a1 <- qt(0.975,14)#�Ӱ谪
a2 <- qt(0.025,14)#�Ӱ谪 
pvalue <- (1-pt(t,df=14))*2 #p�����ϱ�(����Ȯ��)

#������������
a3 <- qt(0.95,14) #�Ӱ谪
pvalue2 <- 1-pt(t,14) #p��(����Ȯ��)

#������������
a3 <- qt(0.025,14) #�Ӱ谪
pvalue2 <- 1-pt(t,14) #p��(����Ȯ��)


#231p
weight <- c(3837,3334,2208,1745,2576,3208,37 46,3523,3430,3480,3116,3428,2184,2383,3500,3866,3542,3278)
h <- 2800
t <- (mean(weight)-h)/(sd(weight)/sqrt(length(weight))) #������跮
#��������
#�Ⱒ�����ϱ�
a1 <- qt(0.975,17)#�Ӱ谪
a2 <- qt(0.025,17)#�Ӱ谪 
pvalue <- (1-pt(t,df=17))*2 #p�����ϱ�(����Ȯ��)

t.test(weight, mu = 2800, alternative = "two.sided") #t-test

#������������
a3 <- qt(0.95,14) #�Ӱ谪
pvalue2 <- 1-pt(t,14) #p��(����Ȯ��)

t.test(weight, mu = 2800, alternative = "greater") #t-test

#������������
a3 <- qt(0.025,14) #�Ӱ谪
pvalue2 <- 1-pt(t,14) #p��(����Ȯ��)

t.test(weight, mu = 2800, alternative = "less") #t-test 

#234p - ���� ������ ��������
data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",header=F)
str(data)

names(data) <- c("time","gender","weight","minutes")
tmp <- subset(data, gender ==1)
weight <- tmp[[3]] #����° ���� weight�� ����

barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
(t.t <- (barx-h0)/(s/sqrt(n)))

alpha <- 0.05
(c.u <- qt(1-alpha, df=n-1))
(p.value <- 1 - pt(t.t, df=n-1))

t.test(weight, mu=2800, alternative="greater")

#240p - ����� ����
tmp <- read.table("C:/Users/user/Desktop/source/Chapter06/data/restitution.txt",header=T)
rel <- ifelse(tmp$rst < 0.4134|tmp$rst >0.4374,1,0)

n <- length(rel) #�ڷᰳ��
nos <- sum(rel) 
sp <- nos / n
hp <- 0.1
(z <- (sp-hp)/sqrt(hp*(1-hp)/n)) #������跮

alpha <- 0.05
(c.u <- qnorm(1-alpha)) #�Ⱒ��
(p.value <- 1- pnorm(z))

prop.test(nos,n,p=0.1,alternative="greater",correct=FALSE)

#������ ������ �ٷ��

data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",header=F)

nrow(data) #�ళ��
ncol(data) #������
str(data) #������ ���� Ȯ��

head(data)
tail(data)

names(data) #���� �̸� Ȯ��
names(data) <- c("time","gender","weight","minutes") #������ �� �̸� �ٲٱ�
names(data)
names(data)[1] <- "time.24hrs" #������ ù��° �� �̸� �ٲٱ�
names(data)

row.names(data) #���ȣ�� ����

#�������ϴ� �پ��� ���
g1 <- data$gender #$�� �̿��� �� ����
str(g1) 
g2 <- data[,2] #[]�� �̿��� �� ����
str(g2)
g3 <- data["gender"] #["������"]�� �̿��� �� ����
str(g3)
g4 <- data[[2]] #[[]]�� �̿��� �� ����
str(g4)
g5 <- data[["gender"]] #[[]]�� �̿��� �� ����
str(g5)
#������ �� ����
gg1 <- data[,c(2,4)]
str(gg1)
gg2<- data[c("gender","minutes")]
str(gg2)

#���ǿ� �´� �� �����ϱ�
#gender==2�� �ڷ� ��������
data[data$gender==2,]
str(data[data$gender==2,])

subset(data,gender==2)
str(subset(data,gender==2))

#gender == 2�̰� ���ü�ߺ��� ū �ڷḸ ��������
male.m <- mean(data$weight)
data[data$gender==2 & data$weight > male.m,]
str(data[data$gender==2 & data$weight > male.m,])
subset(data, (gender ==2) & (weight > male.m))
str(subset(data, (gender ==2) & (weight > male.m)))

#���ǿ� �´� ��� �� ����
str(data[data$gender==2 & data$weight > male.m, c(2,4)])
str(subset(data, (gender==2)&(weight>male.m),select=c(2,4)))

#������ ������ ����
chapter7 <- data[,c(2,3)]
write.table(chapter7,"C:/Users/Administrator/Desktop/chapter7.txt",row.names=FALSE)

#������ �����ϱ�
ad <- read.csv("C:/Users/Administrator/Desktop/age.data.csv",header=T)
str(ad)
head(ad)
tail(ad)
summary(ad)

#������ ó��
ad$score <- ifelse(ad$score==99,NA,ad$score) #99���� ���������� �ٲٱ�
summary(ad)

ad2<-read.csv("C:/Users/Administrator/Desktop/age.data.csv",header=T,na.strings=c("99")) #������ �ҷ��� �� ���ڿ� ����ó��
summary(ad2)

mean(ad$score)
mean(ad$score,na.rm=T) #������ �����ϰ� ��ձ��ϱ�

#�������� Ȯ��
is.na(c(1,NA,3,NA,5))

nonna.sum <- sum(ad$score[!is.na(ad$score)]) #score�� �������� �ƴ� �͵��� ��
nonna.length <- length(ad$score([!is.na(ad$score)]) #���������� �ƴ� �͵��� ���� ���� 
nonna.sum / nonna.length


#factor��ȯ
ad$scale <- factor(ad$scale) #������ �ڷ�� �ٲ���
ad$sex <- factor(ad$sex) #������ �ڷ�� �ٲ���
str(ad)
summary(ad)

#������ ���� 1�� �� ��跮
length(ad$age[ad$scale=="1"]) #scale�� 1�� �� age�ڷ��� ���� 
mean(ad$age[ad$scale=="1"]) 
sd(ad$age[ad$scale=="1"]) 

#summaryby�Լ� 
install.packages("doBy")
library(doBy)
summaryBy(age~scale, data=ad, FUN=c(length))
summaryBy(age~scale, data=ad, FUN=c(mean,sd),na.rm=TRUE)


#266p
data <- read.table("C:/Users/Administrator/Desktop/chapter7.txt", header=T)
var.test(data$weight~data$gender) #��л꼺�м�(������ ���� ������ ��)

#269p-�� �������� ��պ�
t.test(data$weight ~ data$gender, mu=0, alternative="less", var.equal=TRUE)

#274p
data <- read.csv("C:/Users/Administrator/Desktop/01.anorexia.csv", header=T)
str( data )

#���̿� ���� ������跮(�������ϴ� ���)
n <- length(data$Prior - data$Post)
m <- mean( data$Prior - data$Post )
s <- sd (data$Prior - data$Post)
( t.t <- m/(s / sqrt(n)) )

#�� ������ ��պ�(r�Լ�)
t.test(data$Prior, data$Post, paired=T, alternative="less")


#282p-�������𺰷� �������� ���� ��պ�
ad <-read.csv("C:/Users/Administrator/Desktop/age.data.csv",header=T,na.strings=c("99"))
ad$scale <- factor(ad$scale) #������ �ڷ�� �ٲ���
ad$sex <- factor(ad$sex) #������ �ڷ�� �ٲ���
str(ad)
summary(ad)

y1 <- ad$age[ad$scale=="1"]#�����Ը� ��������
y2 <- ad$age[ad$sclae=="2"]
y3 <- ad$age[ad$scale=="3"]

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum((y1-y1.mean)^2) #������ ����������(SSE)
sse.2 <- sum((y2-y2.mean)^2)
sse.3 <- sum((y3-y3.mean)^2)

(sse <- sse.1+sse.2+sse.3) #���� ����������
(dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1)) #SSE�� ������(n-k)

#283p-SSt���ϱ�
y <- mean(ad$age)

sst.1 <- sum((y1.mean-y)^2)*length(y1)
sst.2 <- sum((y2.mean-y)^2)*length(y2)
sst.3 <- sum((y3.mean-y)^2)*length(y3)

(sst <- sst.1 + sst.2 + sst.3)
(dft <- length(levels(ad$scale))-1) #SSt������(k-1)


#284p-SSR���ϱ�
(tsq <- sum((ad$age-y)^2) ) #SST
(ss <- sst + sse) #SST

#285p
mst <- sst/dft
mse <- sse/dfe
(f.t <- mst/mse) #������跮
#286p
(f.t <- mst/mse) #������跮
#287p
alpha <- 0.05 #���Ǽ��ؼ���
(tol <- qf(1-alpha,2,147)) #�Ӱ谪���ϱ�

#286p
(p.value <- 1-pf(f.t,2,147)) #p-value���ϱ�

#287p-r�Լ��� ��պ�
ow <- lm(age~scale,data=ad) #scale�� ���� age
anova(ow) 


names <- c("���浿","�Ѹ�","����")
gender <- c("2","2","1")

(names <- c(names,"����")) #���ڿ� �߰�
(gender <- c(gender,"����"))

gender
str(gender)
gender[5] <- "����"
gender
f.gender <- factor(gender)
f.gender
str(f.gender)
levels(f.gender) #factor�� �ڷ��� ���� Ȯ��
f.gender[6] <- "��" #factor������ ���� ���� �������� �߰��ȵ�
f.gender

answer <- c(2,2,3,2,3,4,4,4,3,4)
f.answer <- factor(answer)
str(f.answer)

f.answer <- factor(answer,levels=c(1,2,3,4,5)) #level���� ����
str(f.answer)

o.f.answer <- factor(answer, levels=c(1,2,3,4,5), ordered = TRUE)
str(o.f.answer)
o.f.answer

o.f.answer <- factor(answer, levels=c(1,2,3,4,5), ordered = TRUE, labels=c("�ſ츸��","�Ҹ���","�����̴�","����","�ſ츸��"))
str(o.f.answer)
o.f.answer

names <- c("���浿", "�Ѹ�", "����")
gender <- c("2", "2", "1")

characters <- data.frame(name=names, gender=gender)
str(characters)

characters <- data.frame(name=names, gender=gender, 
                         stringsAsFactors=FALSE) #factor������ ������ ��
str(characters)
characters <- transform(
                characters, 
                f.gender=factor(gender, 
                         levels=c("1", "2"), labels=c("����", "����")))
str(characters)
characters

sns <- read.csv("E:/������α׷���/source/Chapter07/data/snsbyage.csv", header=T)
str( sns )

sns.c <- read.csv("E:/������α׷���/source/Chapter07/data/snsbyage.csv", header=T, stringsAsFactors=FALSE)
str( sns.c )

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1, 2, 3), 
                            labels=c("20��", "30��", "40��")))

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

s.data <- read.csv("E:/������α׷���/source/Chapter07/data/xtab.count.csv", header=T)
s.data
xt.s.data <- xtabs(count~group+result, data=s.data)
xt.s.data


#306p-���յ� ���� 
x <- c(315,101,108,32)
chisq.test(x,p=c(9,3,3,1)/16)

#309p-������ ����

c.tab <- table(sns.c$age.c, sns.c$service.c)
(a.n <- margin.table(c.tab, margin=1)) #���ɺ� ����� �� margin
(s.n <- margin.table(c.tab, margin=2)) #���񽺺� ����� �� margin
(s.p <- s.n/margin.table(c.tab)) #���񽺺� �̿��� �� / ��ü �̿���
expected <- a.n %*% t(s.p) #s.p�� transpose�ϰ� a.n�� ��İ���
(t.t <- sum((c.tab - expected)^2/expected)) #������跮
qchisq(0.95,df=8) #�Ⱒ��
1-pchisq(t.t,df=8) #����Ȯ��

chisq.test(c.tab)

#319p-������ ����
ucba.tab <- apply(UCBAdmissions,c(1,2),sum)
ucba.tab

(a.n <- margin.table(ucba.tab, margin=1))
(g.n <- margin.table(ucba.tab, margin=2))

(a.p <- a.n / margin.table(ucba.tab))
(g.p <- g.n / margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p))) #��뵵��

(t.t <- sum((ucba.tab - expected)^2/expected))#������跮

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
cov.num <- sum((hf.son$Father-f.mean)*(hf.son$Height-s.mean))#ǥ�����л� ���� ���ϱ� 
(cov.xy <- cov.num /(nrow(hf.son)-1))
cov(hf.son$Height,hf.son$Father) #ǥ�����л�

(r.xy <- cov.xy/(sd(hf.son$Father)*sd(hf.son$Height))) #ǥ��������
cor(hf.son$Father,hf.son$Height) #ǥ��������


#339p - ȸ�Ͱ�� ����

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Height - mean.y)*(hf.son$Father-mean.x))
sxx <- sum((hf.son$Father-mean.x)^2)

(b1 <- sxy/sxx)
(b0 <- mean.y - b1*mean.x)

lm(Height ~ Father, data=hf.son)

#344p - ȸ�͸����� ���Ǽ� ����
out <- lm(Height ~ Father, data=hf.son)
anova(out)

qf(0.95,1,463) #5%�� �Ⱒ��
1-pf(83.719,1,463) #����Ȯ�� 

#346p - ȸ�Ͱ���� ���Ǽ� ����
summary(out)

#��������-���� ������(��л꼺 Ȯ��)
plot( hf.son$Father, residuals(out), xlab="residuals", ylab="")
abline(h=0, col="red", lty=2)

#������ ����Ȯ���׸�
qqnorm(residuals(out), main="")
qqline(residuals(out), lty=2, col="red")

#���Լ� ����
shapiro.test(residuals(out))






