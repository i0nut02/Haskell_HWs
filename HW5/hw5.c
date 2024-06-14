#include <stdio.h>
#include <stdlib.h>

// Esercizio 1
int checkLittleEndian() {
    unsigned int i = 1;
    char *c = (char*)&i;
    return (*c == 1);
}

// Esercizio 2
int compare(const void *a, const void *b) {
    /*
        REQ: 'a' e 'b' castabili ad int
        ENS: 'int(a)' - 'int(b)', la differenza tra 
              i due "interi"
    */
    return (*(int*)a - *(int*)b);
}

// Ritorna l'indice i piu' piccolo tale che arr[i] = element
int binarySearchFirst(int *arr, int n, int element) {
    /*
        REQ: 'arr' ordinato;
             0 <= 'n' <= lunghezza 'arr';
        ENS: se 'element', è in 'arr[0:n)', ritorna il più piccolo i tale che 'arr[i]' = 'element', altrimenti se 'element' non e'
        in 'arr' ritorna -1
    */

    int left = 0;
    int right = n - 1;
    int result = -1;

    while (left <= right) {
        int mid = left + (right - left) / 2;

        if (arr[mid] == element) {
            result = mid;
            right = mid - 1; 
        } else if (arr[mid] < element) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return result;
}

int removeDups(int *arr, int n) {
    /*
        REQ: 0 <= n <= lunghezza di 'arr'
        ENS: lunghezza di {'arr[0:n)'} "set"
        MOD: 'arr' avra' tra i primi 'ENS' elementi
              0 duplicati, e l'ordine e' basato sul ordine
              di partenza di 'arr', mentre gli elementi in 'arr[ENS:n)'
              ci saranno elementi che sono in 'arr[0:ENS)'
    */
    // array con elementi ordinati
    int *sortedArray = (int*)malloc(n * sizeof(int));
    if (!sortedArray) {
        exit(EXIT_FAILURE);
    }

    // array fotocopia di quello in input
    int *copy = (int*)malloc(n * sizeof(int));
    if (!copy) {
        exit(EXIT_FAILURE);
    }

    // array per capire se e' un doppione o meno
    int *visited = (int*)malloc(n * sizeof(int));
    if (!visited) {
        exit(EXIT_FAILURE);
    }
    
    for (int i = 0; i < n; ++i) {
        sortedArray[i] = arr[i];
        copy[i] = arr[i];
        visited[i] = 0;
    }

    qsort(sortedArray, n, sizeof(int), compare);

    // dove inserisco i non doppioni
    int first = 0;

    // dove inserisco i doppioni
    int last = n-1;
    int element, index;
    
    for (int i = 0; i < n; ++i) {
        element = copy[i];
        index = binarySearchFirst(sortedArray, n, element);

        if (visited[index]) {
          // doppione
          arr[last--] = element;
        } else {
          // visto per la prima volta
          arr[first++] = element;
          visited[index] = 1;
        }
    }

    free(sortedArray);
    free(copy);
    free(visited);
    return first;
}

// Esercizo 3
typedef struct cBinTree {
    int n;
    int k;
    int value;
    struct cBinTree *left;
    struct cBinTree *right;
} cBinTree;

cBinTree* createNode(int n, int k, int value) {
    /*
        REQ: n >= k e n >= 0 e k >= 0 e value = (n scegli k)
        ENS: Un nodo che ha come valori n = 'n', k = 'k'
             value = 'value' e come figli NULL
    */
    cBinTree *newNode = (cBinTree*)malloc(sizeof(cBinTree));
    if (!newNode) {
        exit(EXIT_FAILURE);
    }
    newNode->n = n;
    newNode->k = k;
    newNode->value = value;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

// Una matrice (n+1)x(k+1) che ci serve
// per ricordarci i nodi gia' calcolati
cBinTree*** createMatrix(int n, int k) {
    /*
        REQ: n >= k e n >= 0 e k >= 0 
        ENS: una matrice (n +1)x(k+1) che puo contenere cBinTree
    */
    cBinTree ***matrix = (cBinTree***)malloc((n+1) * sizeof(cBinTree**));
    if (!matrix) {
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i <= n; i++) {
        matrix[i] = (cBinTree**)malloc((k+1) * sizeof(cBinTree*));
        if (!matrix[i]) {
            exit(EXIT_FAILURE);
        }
        for (int j = 0; j <= k; j++) {
            matrix[i][j] = NULL;
        }
    }
    return matrix;
}

cBinTree* generateTree(int n, int k, cBinTree ***matrix) {
    /*
        REQ: n >= k e n >= 0 e k >= 0 
        ENS: ritorna il cBinTree che rappresenta n scegli k
        MOD: si salva il cBinTree in matrix[n][k]
    */
    if (matrix[n][k] != NULL) {
        return matrix[n][k];
    }

    if (n == k || k == 0) {
        matrix[n][k] = createNode(n, k, 1);
        return matrix[n][k];
    }

    cBinTree *leftChild = generateTree(n-1, k-1, matrix);
    cBinTree *rightChild = generateTree(n-1, k, matrix);

    int value = leftChild->value + rightChild->value;
    matrix[n][k] = createNode(n, k, value);
    matrix[n][k]->left = leftChild;
    matrix[n][k]->right = rightChild;

    return matrix[n][k];
}

cBinTree* cbinTree(int n, int k) {
    /*
        REQ: n >= k e n >= 0 e k >= 0 
        ENS: ritorna il cBinTree che rappresenta n scegli k
    */
    cBinTree ***matrix = createMatrix(n, k);
    generateTree(n, k, matrix);
    return matrix[n][k];
}

// Versione senza matrice, ho notato che in realta
// che i nodi si ripetono perche quando vai a sinistra
// e a destra, ti ritrovi nello stesso caso di andare
// a destra e poi a sinistra, considerando che ci sia
// un valore diverso da NULL
// Inoltre non mi sembra che ci siano altri casi
// calcolandosi prima la parte sinistra dell'albero
cBinTree* generateTree2(int n, int k, cBinTree* leftRightChildOfFather) {
    /*
        REQ: n >= k e n >= 0 e k >= 0 
        ENS: ritorna il cBinTree che rappresenta n scegli k
    */
    if (n == k || k == 0) {
        return createNode(n, k, 1);
    }
    cBinTree *leftChild;
    cBinTree *rightChild;

    if (leftRightChildOfFather != NULL) {
        leftChild = leftRightChildOfFather;
    } else {
        leftChild = generateTree2(n-1, k-1, NULL);
    }

    rightChild = generateTree2(n-1, k, leftChild->right);

    int value = leftChild->value + rightChild->value;
    cBinTree *node = createNode(n, k, value);
    node->left = leftChild;
    node->right = rightChild;

    return node;
}

// Esercizio 4
typedef struct {
    int succ;
    int prec;
} Pair;

// Il costo di questo algoritmo dipende principalmente da questa riga
// delPrime[lastDelIndex++] = p * s;
// ora se riusciamo a dire quante volte un numero viene
// inserito all' interno di delPrime, riusciamo a dire il costo.
// In particolare possiamo dire che ad esempio alla prima iterazione con p = 2
// generiamo tutti i multipli di 2 o meglio tutti i numeri che
// scomposti in fattori primi hanno all'interno come fattore primo minimo 2
// ed eliminandoli sappiamo che rimangono tutti queli che contengono fattori primi > p
// e quindi i numeri non c'e' nessun modo di ricreare quel 
// numero perche ormai ci sara' un p' > p e all'interno dei
// numeri rimanti non esiste nessun numero divisibile per p
Pair* eulerSieve(int n) {
    /*
        REQ: n >= 2
        ENS: ritorna un puntatore a una lista di Pair che 
             dove 'ENS[i].succ' e' il prossimo primo e 'ENS[i].prec' e' il precedente primo
             inoltre prec e succ saranno uguali a 0
             se i e' non primo
    */
    Pair *sieve = (Pair *)malloc((n + 1) * sizeof(Pair));
    if (!sieve) {
        exit(EXIT_FAILURE);
    }

    int *delPrime = (int*)malloc((n + 1) * sizeof(int));

    if (!delPrime) {
        exit(EXIT_FAILURE);
    }

    // Inizializiamo il vettore
    for (int i = 2; i <= n; ++i) {
        sieve[i].succ = 1;
        sieve[i].prec = (i == 2) ? 0 : 1;
    }

    int lastDelIndex = 0;

    for (int p = 2; p <= n; p += sieve[p].succ) {
        lastDelIndex = 0;
        for (int s = p; p * s <= n; s += sieve[s].succ) {
            delPrime[lastDelIndex++] = p * s;
        }

        for (int i = 0; i <ß lastDelIndex; i++) {
            int actNum = delPrime[i];
            int precP = sieve[actNum].prec;
            int succP = sieve[actNum].succ;

            // aggiornamento dei puntatori tra primo 
            // precedente e successivo
            sieve[actNum - precP].succ += succP;
            if (actNum + succP <= n) {
                sieve[actNum + succP].prec += precP;
            }

            // il numero attuale come non e'
            // primo siccome e' un prodotto tra un numero
            // e un primo
            sieve[actNum].prec = 0;
            sieve[actNum].succ = 0;
        }
    }
    free(delPrime);
    return sieve;
}

int main() {
    printf("2002688\n");
    return 2002688;
}