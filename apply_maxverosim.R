apply_maxverosim<-function(vettore_dati,comando_plottaggio,fascia_oraria)

{

# Parametri iniziali
	vettore_parametri<-rep(c(0),each=2)
	vettore_parametrifunzione<-rep(c(0),each=2)
	x<-sort(vettore_dati)

# Esiste già una funzione per eseguire questa operazione... Ma ha bisogno di   #
# valori iniziali, da cui partire con l'interpolazione.                        #
# E' pratica consolidata fornirle i valori del fattore di scala e del fattore  #
# di forma della distribuzione di Gumbel ottenuti dal metodo                   #
# dei momenti.

# Inizializzazione parametri del fit da cui partire
	source("apply_moments.R")	
	parametri<-apply_moments(vettore_dati,"NN",fascia_oraria)
	
# Attenzione: fitdistr ha bisogno di immagazzinare i dati iniziali in una lista # 
# identata, qua nell'ordine scala-loc. Agli elementi di una lista si accede con #
# la sintassi [[riga]][colonna]                                                 #

# Fittaggio
	vettore_parametri<-fitdistr(sort(vettore_dati),densfun=dgumbel,start=list(scale=parametri[1],loc=parametri[2]))

# Plottaggio della funzione Gumbel e della ECDF
	if(comando_plottaggio=="Y")
	{
        plot(x,pgumbel(x,scale=vettore_parametri[[1]][1],loc=vettore_parametri[[1]][2]),xlab="Altezza precipitazione[mm]",ylab="Probabilità[H<h]",main=paste("ECDF a", paste(fascia_oraria),"fittata con Gumbel"),sub="Metodo di massima verosimiglianza", col="red",type="l")
 	plot(ecdf(x),add=T)
	}
	else if(comando_plottaggio=="N")					#Scelta tra visualizzazione del grafico ("Y") o suo salvataggio ("N")

	{

	path<-file.path(paste(getwd()),"Gumbelplots","Maxverosim/")
	dir.create(path,recursive=TRUE,showWarnings=FALSE)
	path<-paste(paste(path),"Gumbel_maxverosim", paste(fascia_oraria), ".png",sep="_")
	png(file=path)

	plot(x,pgumbel(x,scale=vettore_parametri[[1]][1],loc=vettore_parametri[[1]][2]),xlab="Altezza precipitazione[mm]",ylab="Probabilità[H<h]",main=paste("ECDF a", paste(fascia_oraria),"fittata con Gumbel"),sub="Metodo di massima verosimiglianza", col="red",type="l")
 	plot(ecdf(x),add=T)

	dev.off()
	}
	
# Rilascio dei dati contenuti nella lista in un vettore, per più facile 
# immagazzinamento successivo (e per uniformità dei dati)	
	for (i in 1:2)
	{	
	vettore_parametrifunzione[i]<-vettore_parametri[[1]][i]			# Ok, ci piacciono i cicli
	}

	return (vettore_parametrifunzione)					# Fattore scala in prima posizione, fattore di localizzazione in seconda
}


