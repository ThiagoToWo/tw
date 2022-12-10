#include <stdio.h> // printf(), FILE, fopen(), fclose(), scanf()
#include <stdlib.h> // exit()
#include <ctype.h> // isalpha(), isdigit(), isspace(), toupper()
#include <string.h> // strcmp()

#define PROGLEN 10000
#define VARLEN  26
#define STRLEN  1000
#define NUMLEN  100

FILE* file; /*program file*/
double var[VARLEN]; /*array of variables*/
char token; /*current character in program*/
int idx; /*current token index*/
char prog[PROGLEN]; /*optimized program string pointer*/
char cont[PROGLEN]; /*program content text*/
double labl[PROGLEN]; /*label characters and their values*/

void program();
void statement_seq();
void statement();
void assign();
void read();
void write();
void conditional();
void branch();
void label();
void comment();
double logical_expr();
double log_expr_term();
double log_expr_factor();
double relational_expr();
double simple_expr();
double term();
double factor();

void error(int e) {
    static char* errors[] = {
        "Error",
        "Unexpected token",
        "Factor error",
        "Invalid comparison simbol",
        "Invalid variable name",
        "Invalid command"
    };

    printf("Snippet: ");
    for (int i = -5; i <= 5; i++)
        printf("%c", prog[idx + i]);

    printf(". Current token: %c. ", token);
    printf("%s\n", errors[e]);

    exit(1);
}

void getusch() { /*get non space char*/
    while (isspace(token = prog[++idx]));
}

void scannum(double* n) { /*get number from current token in prog*/
    char temp[NUMLEN];
    int i = 0;
    int state = 1;
    
    while (state != 9) {
        switch (state) {
            case 1:
                if (token == '+' || token == '-') {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 2;                    
                } else if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 3;                    
                } else {
                    error(1);
                }
                break;
            case 2:
                if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 3;                    
                } else {
                    error(1);
                } 
                break;
            case 3:
                if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];                    
                } else if (token == '.') {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 4;                    
                } else if (token == 'E' || token == 'e') {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 6;                    
                } else {
                    token = prog[--idx];                    
                    state = 9;                    
                }
                break;
            case 4:
                if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 5;                    
                } else {
                    error(1);
                }
                break;
            case 5:
                if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];                                        
                } else if (token == 'E' || token == 'e') {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 6;                    
                } else {
                    token = prog[--idx];                    
                    state = 9;                    
                }
                break;
            case 6:
                if (token == '+' || token == '-') {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 7;                    
                } else if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 8;                    
                } else {
                    error(1);
                }
                break;
            case 7:
                if (isdigit(token)) {
                    temp[i++] = token;
                    token = prog[++idx];
                    state = 8;                    
                } else {
                    error(1);
                } 
                break;
            case 8:
                if (isalpha(token)) {
                    temp[i++] = token;
                    token = prog[++idx];                    
                } else {
                    token = prog[--idx];                    
                    state = 9;                    
                }
        }
    }

    temp[i] = '\0';

    *n = atof(temp);
}

void match(char expectedToken) {
    if (token == expectedToken) {
        getusch();
    } else {
        error(1); /*Unexpected token*/
    }
}

void readFile() {
    int i = 0;

    while ((cont[i] = fgetc(file)) != EOF) i++;
    cont[i] = '\0';
}

void optimize() {
    int p = 0;
    int c = 0;

    prog[p] = cont[c];

    while ((prog[p]) != '\0') {
        if (isspace(prog[p])) {
            while (isspace(cont[++c]));
            prog[p] = cont[c];            
        } 
        
        if (prog[p] == '#') {
            while (cont[++c] != '\n' && cont[c] != '\0');
            prog[p] = cont[c];
            continue;
        }

        prog[++p] = cont[++c];          
    }

    prog[p] = '\0';
}

/*
* PRE-CONDITION: idx = -1
* POST-CONDITION: 
*   idx = PROGLEN
*   token = last character
*/
void markLabels() {
    double temp;
    
    token = prog[++idx];

    if (isdigit(token)) {
        scannum(&temp);
        labl[idx++] = temp;
    }

    while (idx < PROGLEN) {
        token = prog[++idx];

        if (token == ';') {
            while (isspace(token = prog[++idx]));

            if (isdigit(token)) {
                scannum(&temp);
                labl[idx++] = temp;
            }
        }
    }
}

void printContent() {
    printf("CONTENT:\n");

    int i = 0;

    while (cont[i] != '\0') {
        printf("%c", cont[i]);
        i++;
    }

    printf("\n");
}

void printProgram() {
    printf("OPTIMIZED:\n");

    int i = 0;

    while (prog[i] != '\0') {
        printf("%c", prog[i]);
        i++;
    }

    printf("\n");
}

void main(int argc, char* argv[]) {
    if (argc < 2 || argc > 3) {
        printf("tw\tversion: 1.3\n");
        printf("Use: .\\tw <file_name> [<options>]\n");
        printf("Options availables:\n");
        printf("\tp:\tprint program content optimized.\n");
        printf("\tc:\tprint program content text.\n");
        printf("\tcp:\tprint text and optimized program content.\n");
        exit(1);
    }

    if (!(file = fopen(argv[1], "r"))) {
        printf("Error openning file.\n");
        exit(1);
    }   

    readFile();
    fclose(file);

    optimize();
    idx = -1;
    markLabels();

    if (argv[2]) {
        if (strcmp(argv[2], "c") == 0)
            printContent();
        else if(strcmp(argv[2], "p") == 0)
            printProgram();
        else if(strcmp(argv[2], "cp") == 0) {
            printContent();
            printProgram();
        } else {
            printf("Invalid option.\n");
        }
    }
    
    printf("--------------Start execution--------------\n");
    
    idx = -1;
    getusch();
    program();

    if (token == '\0'){        
        printf("\nFinish. Press a key to exit.");
        getchar();        
    }
}

void program() {
    statement_seq();
}

void statement_seq() {
    statement();

    while (token == ';') {
        match(';');     
        statement();
    }
}

void statement() {    
    switch (token) {
        case '>': /*read*/
            match('>');
            if (token == '>') {
                match('>');
                read();
            } else {
                error(5); /*Invalid command*/
            }
            break;
        case '<': /*write*/
            match('<');
            if (token == '<') {
                match('<');
                write();
            } else {
                error(5); /*Invalid command*/
            }
            break; 
        case '?': /*conditional*/
            conditional();
            break;
        case '-': /*branch*/
            match('-');
            if (token == '>') {
                match('>');
                branch();
            } else {
                error(5); /*Invalid command*/
            }
    }  

    if (isalpha(token)) { /*assignment*/
        assign();
    } else if (isdigit(token)) { /*lable handle*/
        label();
    }
}

void assign() {
    char variable = token;
    getusch();

    if (token == '=') {
        match('=');
        var[toupper(variable) - 'A'] = logical_expr();
    } else {
        error(4); /*Invalid variable name*/
    }
}

void read() {
    if (isalpha(token)) {
        scanf("%lf%*c", &var[toupper(token) - 'A']);
        getusch();     
    } else {
        error(1); /*Unexpected token*/
    }
}

void write() {
    double result;
    char string[STRLEN];

    if (token == '\"') {
        token = prog[++idx];
        
        int i = 0;
        while (token != '\"') {
            if (token == '_') {
                string[i++] = ' ';
            } else if (token == '\\') {
                string[i++] = '\n';
            } else {
                string[i++] = token;
            }
            
            token = prog[++idx];            
        }

        string[i] = '\0';
        token = prog[++idx];

        printf("%s", string);        
    } else if (token == '\\') {
        printf("\n");
        match('\\');        
    } else {
        result = logical_expr();
        printf("%g", result);
    }    
}

void conditional() {
    match('?');
    double expression = logical_expr();
    match('?');
    match('-');
    match('>');
    
    if (expression == 1) {
        branch();
    } else {
        label();
    }
     
}

void branch() {
    double temp;
    int pos;

    if (isdigit(token)) {
        scannum(&temp);
        
        for (pos = 0; pos < PROGLEN; pos++) {
            if (temp == labl[pos]) break;
        }

        idx = pos;
        token = prog[idx];
    } else {
        error(1);
    }
}

void label() {
    double temp;
    scannum(&temp);
    getusch();
}

double logical_expr() {
    double temp = log_expr_term();

    while (token == '|') {
        match('|');
        temp = (int) temp | (int) log_expr_term();
    }

    return temp;
}

double log_expr_term() {
    double temp = log_expr_factor();
    
    while (token == '&') {
        match('&');
        temp = (int) temp & (int) log_expr_factor();
    }

    return temp;
}

double log_expr_factor() {
    double temp = relational_expr();

    switch (token) {
        case '=':
            match('=');
            if (token == '=') {
                match('=');
                temp = (temp == relational_expr());
            } else {
                error(3);
            }
            break;
        case '!':
            match('!');
            if (token == '=') {
                match('=');
                temp = (temp != relational_expr());
            } else {
                error(3);
            }
    }

    return temp;
}

double relational_expr() {
    double temp = simple_expr();
    
    switch (token) {
        case '<':
            match('<');
            if (token == '=') {
                match('=');
                temp = (temp <= simple_expr());
            } else {
                temp = (temp < simple_expr());
            }
            break;
        case '>':
            match('>');
            if (token == '=') {
                match('=');
                temp = (temp >= simple_expr());
            } else {
                temp = (temp > simple_expr());
            }        
    }

    return temp;
}

double simple_expr() {
    double temp = term();

    while (token == '+' || token == '-') {
        switch (token) {
            case '+':
                match('+');
                temp += term();
                break;
            case '-':
                match('-');
                temp -= term();
        }
    }

    return temp;
}

double term() {
    double temp = factor();

    while (token == '*' || token == '/' || token == '%') {
        switch (token) {
            case '*':
                match('*');
                temp *= factor();
                break;
            case '/':
                match('/');
                temp /= factor();
                break;
            case '%':
                match('%');
                temp = (int) temp % (int) factor();
        }       
    }

    return temp;
}

double factor() {
    double temp;

    if (token == '(') {
        match('(');
        temp = logical_expr();
        match(')'); 
    } else if (isdigit(token) || token == '+' || token == '-') {
        scannum(&temp);
        getusch();
    } else if (isalpha(token)) {
        temp = var[toupper(token) - 'A'];
        getusch();
    } else {
        error(2);
    }

    return temp;
}