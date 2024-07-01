import os


def read_fasta(fasta_sequences):
    """
    Legge un file in formato FASTA e estrae le sequenze e i relativi nomi.

    Args:
    fasta_sequences (str): Percorso del file FASTA da leggere.

    Returns:
    sequences: lista delle sequenze estratte
    names: lista dei nomi delle sequenze
    """
    # Inizializza una lista per contenere le sequenze e una lista per i nomi delle sequenze
    sequences = []
    seq = ""
    names = []

    # Apre il file FASTA e legge le righe
    with open(fasta_sequences, 'r') as f:
        fasta_lines = f.readlines()

        # Scorre le righe del file FASTA
        for line in fasta_lines:
            # Se una riga inizia con '>', estrae il nome della sequenza
            if line.startswith('>'):
                names.append(line.split(" ")[0][1:])  # Estrae il nome dalla riga di intestazione
                if seq:
                    sequences.append(seq)
                seq = ""  # Resetta la variabile per la prossima sequenza
            else:
                # Se la riga non è un'intestazione, aggiunge la parte della sequenza alla sequenza corrente
                line = line.strip()
                seq += line

        # Aggiunge l'ultima sequenza letta (se presente)
        if seq:
            sequences.append(seq)

    # Restituisce una tupla contenente le sequenze estratte e i relativi nomi
    return sequences, names


def prepare_matrix_from_fasta_aligned(fasta_content):
    """
    Legge il contenuto di un file FASTA già allineato e lo struttura in una matrice per il confronto delle sequenze.

    Args:
    fasta_content (str): Contenuto di un file FASTA allineato con più sequenze.

    Returns:
    matrix (list of list): matrice dove ogni riga rappresenta una sequenza diversa ed ogni colonna un carattere della sequenza.
    """
    # Dividi il contenuto per '>' e filtra le stringhe vuote
    entries = [entry for entry in fasta_content.split('>') if entry.strip()]

    # Prepara una lista per contenere tutte le sequenze
    sequences = []

    # Estrai le sequenze
    for entry in entries:
        lines = entry.split('\n')
        sequence = ''.join(line.strip() for line in lines[1:] if line.strip())  # Unisci tutte le righe di sequenza
        sequences.append(sequence)

    # Prepara la matrice dove ogni riga è una sequenza e le colonne sono allineate con le posizioni dei caratteri
    matrix = [list(seq) for seq in sequences]  # Converte ogni sequenza in una lista di caratteri

    return matrix


def identify_variations(base, lines):
    """
    Identifica le variazioni e crea una matrice booleana che indica in quella posizione la presenza o l'assenza di variazione rispetto alla sequenza di riferimento.

    Args:
    base (str): La sequenza di riferimento da confrontare.
    lines (list of str): Le sequenze da confrontare.

    Returns:
    matrice (list of list): Una matrice che indica la presenza di variazioni.
    """
    # Inizializza la matrice per registrare la presenza di variazioni
    matrice = [[False for x in range(len(base))] for y in range(len(lines))]
    nonstarted = True
    maxIndex = len(base) - 1

    i = 0
    j = 0

    for linea in lines:
        # Ignora eventuali indel alla fine della sequenza
        while linea[maxIndex] == '-':
            maxIndex -= 1

        # Analizza ogni carattere nella sequenza
        for character in linea:
            # Ignora i caratteri 'N'
            if character == 'N':
                matrice[i][j] = True
            else:
                # Ignora eventuali indel iniziali
                if nonstarted & (character == '-'):
                    matrice[i][j] = True
                else:
                    nonstarted = False
                    # Controlla se il carattere corrente corrisponde al carattere nella sequenza di riferimento (base)
                    if (character == base[j]) | (j > maxIndex):
                        matrice[i][j] = True
            j += 1

        # Resetta il flag per gli indel iniziali
        nonstarted = True
        i += 1
        j = 0
        maxIndex = len(base) - 1

    return matrice


def print_variations(base, lines, variation_matrix, names, output_file):
    """
    Individua la tipologia e stampa le variazioni rispetto alla sequenza di riferimento. In oltre si occupa di produrre il file di report.

    Args:
    base (str): La sequenza di riferimento.
    lines (list of str): Le sequenze da confrontare.
    variation_matrix (list of list): La matrice delle variazioni.
    names (list of str): I nomi delle sequenze.

    Output:
    Produce un file txt contenente il report
    """
    # Scrivi l'output in un file di testo
    with open(output_file, 'w') as f:
        # Inizializza le variabili per il conteggio delle variazioni
        i = 0
        j = 0
        count = [0 for _ in range(len(lines))]
        countpos = [0 for _ in range(len(base))]

        # Itera su ogni riga della matrice delle variazioni
        for row in variation_matrix:
            f.write("\nvariazioni nella read " + names[i + 1] + ": \n")
            for element in row:
                if not element:
                    # Se viene rilevata una variazione, aggiorna i contatori
                    count[i] += 1
                    countpos[j] += 1
                    # Stampa i dettagli delle variazioni
                    if lines[i][j] == '-':
                        f.write("posizione " + str(j + 1) + ": inserimento di " + base[j] + "\n")
                    else:
                        if base[j] == '-':
                            f.write("posizione " + str(j + 1) + ": cancellazione di " + lines[i][j] + "\n")
                        else:
                            f.write("posizione " + str(j + 1) + ": sostituzione " + base[j] + "-->" + lines[i][j] + "\n")
                j += 1
            i += 1
            j = 0

        max_index = count.index(max(count))
        min_index = count.index(min(count))

        f.write("\nIl genoma con più variazioni è " + names[max_index + 1] + ", che presenta " + str(
            count[max_index]) + " variazioni\n")
        f.write("\nIl genoma con meno variazioni è " + names[min_index + 1] + ", che presenta " + str(
            count[min_index]) + " variazioni\n\n")

        # Inizializza le variabili per la gestione delle variazioni consecutive
        start = ''
        end = ''
        w = 0
        anotherOne = False
        prevstuff = [('', '') for x in range(len(lines))]

        for i in range(len(base)):
            countx = 0
            for j in range(len(lines)):
                # Controlla se c'è una variazione nella posizione i della sequenza j
                if not variation_matrix[j][i]:
                    if anotherOne:
                        # Se siamo già nel mezzo di un altro blocco di variazioni e questa variazione è diversa dalla
                        # precedente
                        if (base[i] != start) | (end != lines[j][i]):
                            # Memorizza la variazione precedente
                            prevstuff[w] = (start, end)
                            w += 1
                            # Inizia un nuovo blocco di variazioni
                            start = base[i]
                            end = lines[j][i]
                            anotherOne = False
                    # Se è la prima variazione trovata in questa posizione
                    if countx == 0:
                        # Memorizza il carattere della sequenza base
                        start = base[i]
                        # Memorizza il carattere della sequenza j
                        end = lines[j][i]
                        countx += 1
                    else:
                        # Controlla se la variazione è uguale a quella trovata precedentemente
                        if (base[i] == start) & (lines[j][i] == end):
                            # Incrementa il conteggio
                            countx += 1
                        else:
                            foundAnother = True
                            for (prevstart, prevend) in prevstuff:
                                if prevstart == prevend:
                                    break
                                if (base[i] == prevstart) & (prevend == lines[j][i]):
                                    foundAnother = False
                            if foundAnother:
                                # Inizia un nuovo blocco di variazioni
                                anotherOne = True

            if countx != 0:
                aggiunta = " genoma" if countx == 1 else " genomi"
                if start == '-':
                    f.write("alla posizione " + str(i + 1) + " c'è una cancellazione di " + end + " in " + str(countx)
                            + aggiunta + "\n")
                else:
                    if end == '-':
                        f.write("alla posizione " + str(i + 1) + " c'è un inserimento di " + start + " in " + str(countx)
                                + aggiunta + "\n")
                    else:
                        f.write("alla posizione " + str(i + 1) + " c'è una sostituzione " + start + "-->" + end + " in " +
                                str(countx) + aggiunta + "\n")
                if anotherOne:
                    i -= 1
                else:
                    prevstuff = [('', '') for x in range(len(lines))]
                    w = 0

        letter = ''
        all_vary_positions = []
        same_vary_positions = []

        # Itera su ogni posizione nelle sequenze per identificare variazioni uguali
        for h in range(len(countpos)):
            # Controlla se tutte le sequenze hanno una variazione in questa posizione
            if countpos[h] == len(lines):
                # Verifica se tutte le variazioni sono uguali
                alleq = True
                for x in range(len(lines)):
                    if x == 0:
                        # Memorizza il primo carattere in questa posizione
                        letter = lines[x][h]
                    else:
                        if lines[x][h] != letter:
                            alleq = False
                # Se tutte le variazioni sono uguali, registra la posizione
                all_vary_positions.append(h)
                if alleq:
                    # Se le variazioni sono uguali in tutte le sequenze, registra la posizione con i dettagli della variazione
                    same_vary_positions.append((h, base[h], lines[0][h]))

        f.write("\nTutti i genomi variano rispetto al reference in " + str(len(all_vary_positions)) + " posizioni:\n")

        for pos in all_vary_positions:
            f.write("alla posizione " + str(pos + 1) + " tutti i genomi variano\n")

        f.write("\nTutti i genomi variano rispetto al reference allo stesso modo in " + str(len(same_vary_positions))
                + " posizioni:\n")

        for pos, start, end in same_vary_positions:
            if start == "-":
                f.write(
                    "alla posizione " + str(pos + 1) + " tutti i genomi variano allo stesso modo, con una cancellazione di "
                    + end + "\n")
            elif end == "-":
                f.write(
                    "alla posizione " + str(
                        pos + 1) + " tutti i genomi variano allo stesso modo, con un inserimento di " + start + "\n")
            else:
                f.write("alla posizione " + str(
                    pos + 1) + " tutti i genomi variano allo stesso modo, con una sostituzione " + start + " --> " + end + "\n")


# Percorso al file FASTA
fasta_file_path = os.path.join(os.path.dirname(__file__), "mafft-I20240427-110027-0296-29230641-p1m.aln-fasta")
output_file_path = os.path.join(os.path.dirname(__file__), "output_variations.txt")

# Lettura delle sequenze dal file
sequences, names = read_fasta(fasta_file_path)

# Preparazione della matrice delle sequenze
fasta_content = "\n".join([f">Seq{i + 1}\n{seq}" for i, seq in enumerate(sequences)])
matrix = prepare_matrix_from_fasta_aligned(fasta_content)
base = matrix[0]
lines = matrix[1:]

# Identificazione delle variazioni
variation_matrix = identify_variations(base, lines)

# Stampa delle variazioni in un file
print_variations(base, lines, variation_matrix, names, output_file_path)
