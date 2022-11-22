/*
* <programa> -> <decl_sequência>
* <decl_sequência> -> <declaração> {;<declaração>}
* <declaração> -> <decl_atribuição> | <decl_leitura> | <decl_escrita>
* <decl_atribuição> -> <identificador> = <expr>
* <decl_leitura> -> ">>" <identificador>
* <decl_escrita> -> "<<" <expr>
* <expr> -> <expr_simples> [<relacional> <expr_simples>]
* <relacional> -> < | > | <= | >= | == | !=
* <expr_simples> -> <termo> {<soma> <termo>}
* <soma> -> + | -
* <termo> -> <fator> {<mult> <fator>}
* <mult> -> * | / | %
* <fator> -> (expr_simples) | <numero>
*/

#include <stdio.h> // printf(), FILE, fopen(), fclose(), scanf(), fscanf()
#include <stdlib.h> // exit(), malloc()
#include <string.h> // strlen(), strcpy(), strtok(), strchr(), strcmp()
#include <ctype.h> // isalpha(), isdigit(), isspace(), toupper()

FILE* file;
double var[26];
char token;

void program();
void statement_seq();
void statement();
void assign();
void read();
void write();
double expr();
double simple_expr();
double term();
double factor();

void error(int e) {
    static char* errors[] = {
        "Error",
        "Unexpected token",
        "Scanning error",
        "Invalid comparison simbol",
        "Invalid variable name",
        "Invalid command"
    };

    printf("token = %c. ", token);
    printf("%s\n", errors[e]);

    exit(1);
}

int getusch() { /*get non space char*/
    int temp;

    while (isspace(temp = fgetc(file)));

    return temp;
}

void match(char expectedToken) {
    if (token == expectedToken) {
        token = getusch();
    } else {
        error(1);
    }
}

void main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Use: .\\tw <nome_do_arquivo>");
        exit(1);
    }

    if (!(file = fopen(argv[1], "r"))) {
        printf("Error openning file.\n");
        exit(1);
    } else {
        printf("File opened successfully.\n");
    }

    token = getusch();
    program();

    if (token == EOF){
        printf("Finish. Press a key to exit.");
        getchar();
        fclose(file);
    } else {
        error(0);
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
    if (!isalpha(token) && token != '<' && token != '>') error(1);
    
    switch (token) {
        case '>':
            match('>');
            if (token == '>') {
                match('>');
                read();
            } else {
                error(5);
            }
            break;
        case '<':
            match('<');
            if (token == '<') {
                match('<');
                write();
            } else {
                error(5);
            }
            break;        
        default:
            assign();
    }
     
}

void assign() {
    char variable = token;
    token = getusch();

    if (token == '=') {
        match('=');
        var[toupper(variable) - 'A'] = expr();
    } else {
        error(4);
    }
}

void read() {
    if (isalpha(token)) {
        printf("? ");
        scanf("%lf%*c", &var[toupper(token) - 'A']);
        token = getusch();     
    } else {
        error(1);
    }
}

void write() {
    double result = expr();
    printf("= %lf\n", result);
}

double expr() {
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
            break;
        case '=':
            match('=');
            if (token == '=') {
                match('=');
                temp = (temp == simple_expr());
            } else {
                error(3);
            }
            break;
        case '!':
            match('!');
            if (token == '=') {
                match('=');
                temp = (temp != simple_expr());
            } else {
                error(3);
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
        temp = simple_expr();
        match(')');
    } else if (isdigit(token)) {
        ungetc(token, file);
        fscanf(file, "%lf", &temp);
        token = getusch();
    } else if (isalpha(token)) {
        temp = var[toupper(token) - 'A'];
        token = getusch();
    } else {
        error(2);
    }

    return temp;
}