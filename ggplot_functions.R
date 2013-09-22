ggplot_function<-function(dati) {

p<-ggplot(dati,aes(x=Anno))
	for (i in 2:length(names(dati))){
	p<-p+geom_line(aes(y=dati[,i]))+geom_point(aes(y=dati[,i]))
	}
dev.new()
print(p)
}
