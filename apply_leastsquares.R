apply_leastsquares<-function(vettore_dati,comando_plottaggio,fascia_oraria)

{

# Parameti iniziali
	vettore_parametri<-rep(c(0),each=2)
	fun_in<-ecdf(vettore_dati)
	prob_emp_hour<-fun_in(sort(vettore_dati))

# Eliminazione dell'ultimo valore (==1, non desiderabile)
	prob_emp_hour<-prob_emp_hour[-length(prob_emp_hour)]

# Least squares corevalues
	y = -(log(-log(prob_emp_hour)))
	x<-sort(vettore_dati)
	x<-x[-length(x)]
	fts<-lsfit(x,y)
	
	vettore_parametri[1]<- fts$coefficients[[2]]^-1			    	  # Fattore di scala
	vettore_parametri[2]<- (-fts$coefficients[[1]]*vettore_parametri[1])	  # Fattore di localizzazione


# Plottaggio della funzione Gumbel e della ECDF
	if(comando_plottaggio=="Y")
	{
	plot(x,pgumbel(x,scale=vettore_parametri[1],loc=vettore_parametri[2]),xlab="Altezza precipitazione[mm]",ylab="Probabilità[H<h]",main=paste("ECDF a",paste(fascia_oraria),"fittata con Gumbel"),sub="Metodo dei minimi quadrati", col="red",type="l")
	plot(ecdf(sort(vettore_dati)),add=T)
	}
	else if(comando_plottaggio=="N")					# Scelta tra visualizzazione del grafico ("Y") o suo salvataggio ("N")

	{

	path<-file.path(paste(getwd()),"Gumbelplots","Least_squares/")
	dir.create(path,recursive=TRUE,showWarnings=FALSE)
	path<-paste(paste(path),"Gumbel_lssquares", paste(fascia_oraria), ".png",sep="_")
	png(file=path)

	plot(x,pgumbel(x,scale=vettore_parametri[1],loc=vettore_parametri[2]),xlab="Altezza precipitazione[mm]",ylab="Probabilità[H<h]",main=paste("ECDF a",paste(fascia_oraria),"fittata con Gumbel"),sub="Metodo dei minimi quadrati", col="red",type="l")
	plot(ecdf(sort(vettore_dati)),add=T)
	
	dev.off()
	}

	return (vettore_parametri)						# Fattore scala in prima posizione, fattore di localizzazione in seconda
}	
