# Progetto Complementi di Basi di Dati - A. A. 2023-2024


# DBMS scelto: CrateDB

## ISTRUZIONI PER L'ESECUZIONE:

- Aprire Docker e seguire le istruzioni scritte nel report per mandare in esecuzione il docker-compose
- Aprire localhost:8888 su un browser qualsiasi, che restituirà la pagina JupyterLab
- A sinistra, sotto alla cartella work, trascinare ed aggiungere il file "DBcomp_template_progetto" presente nella cartella work/my-data
- Trascinare ed aggiungere anche i quattro dataset in formato JSON presenti in my_datasets
- Eseguire le celle del file Jupyter partendo dalla prima


### IN CASO DI PROBLEMI:

Alla cella 27 del file Jupyter è presente una query che esegue la DROP di tutte le tabelle inserite, resettando quindi il database. Nel caso in cui si verifichino eccezioni o errori, è consigliabile eseguire la stessa avendo cura di installare anche la libreria sqlalchemy, aprendo una nuova cella e digitando:

pip install sqlalchemy
