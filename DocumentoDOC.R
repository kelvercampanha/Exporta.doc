#IMPORTANTE:
#Salvar um arquivo .doc, sem nome, no diretório escolhido para o output
#Esse arquivo será usado para exportar as análises

#Limpando a memória e instalando pacotes
rm(list=ls(all=T))
if(!require(ReporteRs)){install.packages("ReporteRS");require(ReporteRs)}
if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}
if(!require(gridExtra)){install.packages("gridExtra");require(gridExtra)}
if(!require(sqldf)){install.packages("sqldf");require(sqldf)}
if(!require(agricolae)){install.packages("agricolae");require(agricolae)}

setwd("C:/Users/User/Desktop/DOC")

#Simulando dados
#Os dados foram gerados através de um processo gama
Tratamento=c(rep("T1",8),rep("T2",8),rep("T3",8),rep("T4",8))
set.seed(12345)
simul_peso=function(alpha,x,beta,n,perf)
{
	alphat=alpha*x
	y=matrix(0,n,perf)

	for(i in 1:perf)
	{
  		set.seed(i)
  		y1=rgamma(n,alphat[1],beta)
  		y[,i]=cumsum(y1)
	}

	t(y)
}
T1=simul_peso(1,c(1,2,3,4),5,4,8)
T2=simul_peso(1.5,c(1,2,3,4),5,4,8)
T3=simul_peso(2,c(1,2,3,4),5,4,8)
T4=simul_peso(2.5,c(1,2,3,4),5,4,8)
dados=as.data.frame(rbind(T1,T2,T3,T4))
colnames(dados)=c("P1","P2","P3","P4")
dados$Tratamento=as.factor(Tratamento)
sapply(dados,class)

#Boxplot dos dados simulados
plot1=ggplot(dados,aes(as.factor(Tratamento),P1))+geom_boxplot(fill="white",colour="#3366FF")+labs(x="Tratamento",y="Peso 1",title="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot2=ggplot(dados,aes(as.factor(Tratamento),P2))+geom_boxplot(fill="white",colour="#3366FF")+labs(x="Tratamento",y="Peso 2",title="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot3=ggplot(dados,aes(as.factor(Tratamento),P3))+geom_boxplot(fill="white",colour="#3366FF")+labs(x="Tratamento",y="Peso 3",title="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot3c=grid.arrange(plot1, plot2, plot3, ncol=3)

#Sumarizando a variável de interesse pela média
classeX=sqldf("
select a.Tratamento, avg(a.P1) as Media_P1, avg(a.P2) as Media_P2, avg(a.P3) as Media_P3
              from dados as a
              group by a.Tratamento")
classeX

#Análise de variância (ANOVA) para os dados
desempenho=aov(P1~Tratamento,data=dados)
ps=summary(desempenho)[[1]][["Pr(>F)"]][1]
tk=HSD.test(desempenho,"Tratamento");cmt=tk$groups#Tukey;
cmt=cbind(rownames(cmt),cmt);names(cmt)=c("Trat","P1","groups")
bf=LSD.test(desempenho,"Tratamento",p.adj="bonferroni");cmb=bf$groups#Bonferroni
r=residuals(desempenho)
nt=shapiro.test(r)[[2]]

desempenho9=aov(P2~Tratamento,data=dados)
ps9=summary(desempenho9)[[1]][["Pr(>F)"]][1]
tk9=HSD.test(desempenho9,"Tratamento");cmt9=tk9$groups#Tukey
cmt9=cbind(rownames(cmt9),cmt9);names(cmt9)=c("Trat","P2","groups")
bf9=LSD.test(desempenho9,"Tratamento",p.adj="bonferroni");cmb9=bf9$groups#Bonferroni
r9=residuals(desempenho9)
nt9=shapiro.test(r9)[[2]]

desempenho18=aov(P3~Tratamento,data=dados)
ps18=summary(desempenho18)[[1]][["Pr(>F)"]][1]
tk18=HSD.test(desempenho18,"Tratamento");cmt18=tk18$groups#Tukey
cmt18=cbind(rownames(cmt18),cmt18);names(cmt18)=c("Trat","P3","groups")
bf18=LSD.test(desempenho18,"Tratamento",p.adj="bonferroni");cmb18=bf18$groups#Bonferroni
r18=residuals(desempenho18)
nt18=shapiro.test(r18)[[2]]

#Tabela resumo com a comparação entre os tratamentos
tb=sqldf("
              select distinct a.Tratamento, b.groups as G1, c.groups as G9, d.groups as G18
              from classeX as a left join cmt as b on a.Tratamento=b.Trat left join cmt9 as c on a.Tratamento=c.Trat left join cmt18 as d on a.Tratamento=d.Trat")
tb

#agrupando estatísticas de interesse com o output da comparação entre tratamentos
m_dia=sapply(dados[,2:4],mean)
dp_dia=sapply(dados[,2:4],sd)
CV=dp_dia/m_dia
pva=round(c(ps,ps9,ps18),digits=4)

mg=matrix(0,nrow(classeX),ncol(classeX)-1)
for (i in 1:(ncol(classeX)-1))
{
	for(j in 1:nrow(classeX))
	{
		a4=substr(classeX[j,i+1],1,6)
		mg[j,i]=paste(a4,"(",tb[j,i+1],")",sep="")
	}
}
result=rbind(mg,substr(as.character(m_dia),1,6),substr(as.character(CV),1,6),substr(as.character(pva),1,6))
result=as.data.frame(result)
colnames(result)=c("P1","P2","P3")
rownames(result)=c("T1","T2","T3","T4","Media","CV","P-Valor")
#h0: Todos os Tratamentoamentos produzem o mesmo efeito
#H1: Pelo menos um Tratamentoamento se difere dos demais

#P-valor para normalidade de resíduos
tn=c(substr(as.character(round(c(nt,nt9,nt18),digits=4)),1,6))
tn=matrix(tn,1,3)
rownames(tn)=c("P-Valor")
colnames(tn)=c("P1","P2","P3")

#################################
#compilando tabelas de resultados
header_cells_props <- cellProperties( text.direction = "btlr", background.color = "#00557F", padding = 3 )

header_text_props <- textProperties( color = "white", font.size = 11, font.weight = "bold" )

MyFTable <- vanilla.table(data= result,add.rownames=TRUE)
MyFTable = setFlexTableBorders(MyFTable,
inner.vertical = borderNone(),
inner.horizontal = borderNone(),
outer.vertical = borderNone(),
outer.horizontal = borderProperties( color = "black",style = "solid", width = 4 ))
MyFTable <- setFlexTableWidths( MyFTable, widths = c(1,1,1,1))
MyFTable[ to = "header" ] = parCenter()
MyFTable[] = parCenter()
MyFTable <- chprop( MyFTable, borderSolid(color = "black"), i = 4, side = "bottom") 

MyFTable2 <- vanilla.table(data= tn,add.rownames=TRUE)
MyFTable2 = setFlexTableBorders(MyFTable2,
inner.vertical = borderNone(),
inner.horizontal = borderNone(),
outer.vertical = borderNone(),
outer.horizontal = borderProperties( color = "black",style = "solid", width = 4 ))
MyFTable2[ to = "header" ] = parCenter()
MyFTable2[] = parCenter()

plot3c=function(){
plot1=ggplot(dados,aes(as.factor(Tratamento),P1))+geom_boxplot(fill="white",colour="#3366FF")+labs(x="Tratamento",y="Peso 1",title="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot2=ggplot(dados,aes(as.factor(Tratamento),P2))+geom_boxplot(fill="white",colour="#3366FF")+labs(x="Tratamento",y="Peso 2",title="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot3=ggplot(dados,aes(as.factor(Tratamento),P3))+geom_boxplot(fill="white",colour="#3366FF")+labs(x="Tratamento",y="Peso 3",title="")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot3c=grid.arrange(plot1, plot2, plot3, ncol=3)
return(plot3c)}

#Exportando tabelas para o documento final
doc <- docx()
doc <- addTitle(doc, "Efeito dos tratamentos sobre o peso vivo de aves", level=2)
doc <- addFlexTable( doc, MyFTable)
doc <- addParagraph(doc,"")

doc <- addTitle(doc, "Boxplot para comparação da evolução dos tratamentos", level=2)
doc <- addPlot( doc = doc , vector.graphic = FALSE,width = 5, height = 3,par.properties = parProperties(text.align ="left", padding = 2), fun = function(){ grid.arrange(arrangeGrob(plot1, plot2,plot3, ncol=3, nrow=1), newpage = F)} )
doc <- addParagraph(doc,"")

doc <- addTitle(doc, "Análise de resíduos", level=2)
doc <- addFlexTable( doc, MyFTable2)
writeDoc(doc, file = "word-table.docx")
