#include <stdio.h> // printf(), FILE, fopen(), fclose(), scanf()
#include <stdlib.h> // exit()
#include <ctype.h> // isalpha(), isdigit(), isspace(), toupper()
#include <string.h> // strcmp()
#include <time.h> // clock()

#define PROGLEN 10000
#define VARMAX  26
#define VARLEN  1000000
#define STRLEN  1000
#define NUMLEN  100

FILE* file; /*program file*/
double var[VARMAX][VARLEN]; /*array of variables*/
char token; /*current character in program*/
int idx; /*current token index*/
int back_pt; /*point at which the program loops back when return is called*/
int start; /*index at which the program starts*/
int end; /*index at which the program ends*/
char prog[PROGLEN]; /*optimized program string pointer*/
char cont[PROGLEN]; /*program content text*/
double labl[PROGLEN]; /*label characters and their values*/
clock_t dt; /*the number of ticks since the execution starts*/

void program();
void statement_seq();
void statement();
void assign();
void read();
void write();
void sintagma();
void conditional();
void branch();
void subrotine();
void label();
void back();
double logical_expr();
double log_expr_term();
double log_expr_factor();
double relational_expr();
double simple_expr();
double term();
double factor();

void error(int e) {
    static char* errors[] = {
        "Error",                           /*error(0)*/
        "Unexpected token",                /*error(1)*/
        "Factor error",                    /*error(2)*/
        "Invalid comparison simbol",       /*error(3)*/
        "Invalid variable name",           /*error(4)*/
        "Invalid command",                 /*error(5)*/
        "; expected",                      /*error(6)*/
        "the program must start with {",   /*error(7)*/
        "= expected"                       /*error(8)*/
    };

    printf("Snippet: ");
    for (int i = -5; i <= 5; i++)
        printf("%c", prog[idx + i]);

    printf(". Current token: %c. ", token);
    printf("%s\n", errors[e]);

    exit(1);
}

void getnext() { /*get non space char*/
    token = prog[++idx];
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
                    getnext();
                    state = 2;                    
                } else if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();
                    state = 3;                    
                } else {
                    error(1); /*Unexpected token*/
                }
                break;
            case 2:
                if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();
                    state = 3;                    
                } else {
                    error(1); /*Unexpected token*/
                } 
                break;
            case 3:
                if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();                    
                } else if (token == '.') {
                    temp[i++] = token;
                    getnext();
                    state = 4;                    
                } else if (token == 'E' || token == 'e') {
                    temp[i++] = token;
                    getnext();
                    state = 6;                    
                } else {
                    token = prog[--idx];                    
                    state = 9;                    
                }
                break;
            case 4:
                if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();
                    state = 5;                    
                } else {
                    error(1); /*Unexpected token*/
                }
                break;
            case 5:
                if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();                                        
                } else if (token == 'E' || token == 'e') {
                    temp[i++] = token;
                    getnext();
                    state = 6;                    
                } else {
                    token = prog[--idx];                    
                    state = 9;                    
                }
                break;
            case 6:
                if (token == '+' || token == '-') {
                    temp[i++] = token;
                    getnext();
                    state = 7;                    
                } else if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();
                    state = 8;                    
                } else {
                    error(1); /*Unexpected token*/
                }
                break;
            case 7:
                if (isdigit(token)) {
                    temp[i++] = token;
                    getnext();
                    state = 8;                    
                } else {
                    error(1); /*Unexpected token*/
                } 
                break;
            case 8:
                if (isalpha(token)) {
                    temp[i++] = token;
                    getnext();                    
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
        getnext();
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

    while (prog[p] != '\0') {
        if (isspace(prog[p])) {
            while (isspace(cont[++c]));
            prog[p] = cont[c];            
        } 
        
        if (prog[p] == '\"') {
            while ((prog[++p] = cont[++c]) != '\"');
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
* PRE-CONDITION: idx = 0 e token = prog[idx];
* POST-CONDITION: 
*   idx = index od the last character
*   token = last character
*/
void markLabels() {
    double temp;

    if (token == '{') {
        getnext();
    } else {
        error(7); /*the program must start with {*/
    }

    if (isdigit(token)) {
        scannum(&temp);
        labl[idx] = temp;        
    }

    while (prog[idx] != '\0') {
        getnext();

        if (token == ';') {
            getnext();

            if (token == '}') {
                getnext();
            }

            if (isdigit(token)) {
                scannum(&temp);
                labl[idx] = temp;
            }
        }
    }
}

void markBounds() {
    for (int i = 0; prog[i] != '\0'; i++) {
        if (prog[i] == '{') {
            start = i;
        } else if (prog[i] == '}') {
            end = i;
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
        printf("tw\tversion: 1.7.2\n");
        printf("Use: .\\tw <file_name> [<options>]\n");
        printf("Options availables:\n");
        printf("\tc:\tprint program content text.\n");
        printf("\tp:\tprint program content optimized.\n");
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
    idx = 0;
    token = prog[idx];
    markLabels();
    markBounds();

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
    
    idx = start;
    token = prog[idx];
    program();

    if (token == '}'){  
        dt = clock();        
        printf("\nFinish.");
        printf("\nTime: %g seconds.", (float) dt / CLOCKS_PER_SEC);
        printf("\nPress a key to exit.");
        getchar();        
    }
}

void program() {
    match('{');
    statement_seq();
}

void statement_seq() {
    while (token != '}') {
        statement();

        if (token == ';') {
            match(';');
        } else {
            error(6);
        }
    }    
}

void statement() {    
    switch (token) {
        case '>': /*read*/
            match('>');
            if (token == '>') {
                match('>');
                read();

                while (token == ',') {
                    match(',');
                    read();
                }
            } else {
                error(5); /*Invalid command*/
            }
            break;
        case '<': /*write*/
            match('<');
            if (token == '<') {
                match('<');
                write();
            } else if (token == '-') { /*return*/
                match('-');
                match(';');
                back();
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
            break;        
        case 's': /*subrotine*/
            match('s');
            if (token == 'b') {
                match('b');
                subrotine();
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
    getnext();

    if (token == '=') {
        match('=');
        int i = 0;
        var[toupper(variable) - 'A'][i] = logical_expr();

        while (token == ',') {
            match(',');
            var[toupper(variable) - 'A'][++i] = logical_expr();
        }
    } else if (token == '[') {
        match('[');
        double i = logical_expr();
        match(']');

        if (token == '=') {
            match('=');
            var[toupper(variable) - 'A'][(int) i] = logical_expr();

            while (token == ',') {
                match(',');
                i++;
                var[toupper(variable) - 'A'][(int) i] = logical_expr();
            }
        } else {
            error(8);
        }
    } else {
        error(4); /*Invalid variable name*/
    }
}

void read() {
    if (isalpha(token)) {
        char variable = token;
        getnext();

        if (token == '[') {
            match('[');
            scanf("%lf%*c", &var[toupper(variable) - 'A'][(int) logical_expr()]);
            match(']');
        } else {
            scanf("%lf%*c", &var[toupper(variable) - 'A'][0]);
        }              
    } else {
        error(1); /*Unexpected token*/
    }
}

void write() {
    sintagma();

    while (token == ',') {
        match(',');
        sintagma();
    }
}

void sintagma() {
    double result;

    if (token == '\"') {
        getnext();
        
        int i = 0;
        while (token != '\"') {
            if (token == '\\') {
                getnext();

                switch (token) {
                    case 'a':
                        printf("\a");
                        break;
                    case 'b':
                        printf("\b");
                        break;
                    case 'f':
                        printf("\f");
                        break;
                    case 'n':
                        printf("\n");
                        break;
                    case 'r':
                        printf("\r");
                        break;
                    case 't':
                        printf("\t");
                        break;
                    case 'v':
                        printf("\v");                   
                }                
            } else {
                printf("%c", token);
            }
            
            getnext();            
        }

        getnext();      
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
        error(1); /*Unexpected token*/
    }
}

void subrotine() {
    int i = 0;
    while (prog[idx + i] != ';') i++;
    back_pt = idx + i;
    branch();
}

void label() {
    double temp;
    scannum(&temp);
    getnext();
}

void back() {
    idx = back_pt;
    token = prog[idx];    
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
                error(3); /*Invalid comparison simbol*/
            }
            break;
        case '!':
            match('!');
            if (token == '=') {
                match('=');
                temp = (temp != relational_expr());
            } else {
                error(3); /*Invalid comparison simbol*/
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
        getnext();
    } else if (isalpha(token)) {
        char variable = token;
        getnext();
        if (token == '[') {
            match('[');
            temp = var[toupper(variable) - 'A'][(int) logical_expr()];
            match(']');
        } else {
            temp = var[toupper(variable) - 'A'][0];
        } 
    } else {
        error(2); /*Factor error*/
    }

    return temp;
}