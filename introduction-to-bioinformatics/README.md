# BioCose_Consegna

## Quali risultati cerchiamo?
#### Numero di variazioni
#### Posizione con variazione costante
#### Posizioni con Variazioni Comuni


Componenti del gruppo:

- Bernasconi Giorgio Luigi Maria - mat. 885948
- Cambiago Silvia - mat. 879382
- Farioli Alessio - mat. 879217



# Funzioni 

1. read_fasta(fasta_sequences): legge un file in formato FASTA ed estrae sequenze e relativi nomi.

- Argomenti:
  - fasta_sequences (str): percorso del file FASTA da leggere.
  
- Ritorna:
  - sequences: lista delle sequenze estratte 
  - names: lista dei nomi delle sequenze

2. prepare_matrix_from_fasta_aligned(fasta_content): legge il contenuto di un file FASTA già 
   allineato e lo struttura in una matrice per eseguire il confronto delle sequenze.

- Argomenti:
  - fasta_content (str): contenuto di un file FASTA allineato contenente più sequenze
  
- Ritorna:
  - matrix: matrice dove ogni riga rappresenta una sequenza diversa ed ogni colonna rappresenta 
    un carattere della sequenza

3. identify_variations(base, lines): identifica le variazioni e crea una matrice che indica 
   inserzioni, delezioni e sostituzioni.

- Argomenti:
  - base (str): sequenza di riferimento
  - lines (list of str): sequenze da confrontare
  
- Ritorna:
  - matrice: matrice che indica la presenza di variazioni.

==============================================================================================================


Indicazioni per l'esecuzione dello script

- Percorso del file FASTA: lo script richiede che il file FASTA si trovi nella stessa cartella del 
  file Python e che il nome dello stesso sia "mafft-I20240427-110027-0296-29230641-p1m.aln-fasta". 
  L'indicazione del percorso si trova alla riga 124, con l'istruzione:

fasta_file_path = os.path.join(os.path.dirname(file), "mafft-I20240427-110027-0296-29230641-p1m.aln-fasta")

- Output delle variazioni: le variazioni rilevate vengono stampate sul terminale insieme a un riepilogo 
  delle variazioni comuni a tutte le sequenze.


# Link alla presentazione
##### https://www.canva.com/design/DAGIZuYDOKM/8fkx5pnZ6QhsS4Oc2mqjuw/edit?utm_content=DAGIZuYDOKM&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton
