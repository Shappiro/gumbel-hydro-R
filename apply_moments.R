apply_moments<-function(vettore_dati,comando_plottaggio,fascia_oraria)

{


# Parametri iniziali
	eulergamma<-0.577216
	vettore_parametri<-rep(c(0),each=2)
	x<-sort(vettore_dati)
	
# Gumbel corevalues	
	mean<-mean(vettore_dati, na.rm=TRUE)
	var<-var(vettore_dati, na.rm=TRUE)	
	vettore_parametri[1]<-sqrt((6)*var)/pi					# Fattore di scala	
	vettore_parametri[2]<-(mean-vettore_parametri[1]*eulergamma)		# Fattore di localizzazione

# Plottaggio della funzione Gumbel e della ECDF

	if(comando_plottaggio=="Y")
	{
	plot(x,pgumbel(x,scale=vettore_parametri[1],loc=vettore_parametri[2]),xlab="Altezza precipitazione[mm]",ylab="Probabilità[H<h]",main=paste("ECDF a",paste(fascia_oraria), "fittata da Gumbel"),sub="Metodo dei momenti", col="red",type="l")
	plot(ecdf(x),add=T)
	}	
	else if(comando_plottaggio=="N")					# Scelta tra visualizzazione del grafico ("Y") o suo salvataggio ("N")
	
	{
	path<-file.path(paste(getwd()),"Gumbelplots","Moments/")
	dir.create(path,recursive=TRUE,showWarnings=FALSE)
	path<-paste(paste(path),"Gumbel_moments", paste(fascia_oraria), ".png",sep="_")
	png(file=path)

	plot(x,pgumbel(x,scale=vettore_parametri[1],loc=vettore_parametri[2]),xlab="Altezza precipitazione[mm]",ylab="Probabilità[H<h]",main=paste("ECDF a",paste(fascia_oraria),"fittata da Gumbel"),sub="Metodo dei momenti", col="red",type="l")
	plot(ecdf(x),add=T)

	dev.off()
	}
	
	return (vettore_parametri)						# Fattore scala in prima posizione, fattore di localizzazione in seconda
	
}



