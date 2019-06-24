/*	Definition section */
%{
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <stdbool.h>

extern int yylineno;
extern int scope_flag;
extern int num;
extern bool need_print;
extern bool in_if;
extern bool pp_flag;
extern int yylex();
extern char* yytext;   // Get current token from lex
extern char buf[256];  // Get current code line from lex

FILE *file; // To generate .j file for Jasmin

void yyerror(char *s);

/* Symbol table function - you can add new function if needed. */
int lookup_symbol(char* name, int scope, char* entryType);
void create_symbol();
void insert_symbol(char* name, char* entryType, int dataType, bool hasValue);
void dump_symbol(int scope);
bool has_new_symbol(int scope);
void free_symbol_table();
int get_register_num (char* name);
int get_id_type (char* name);

/* code generation functions, just an example! */
void gencode_function();

/* symbol table flag*/
int value_idx;
char par[100];
char error_message[100];
bool err_flag = false;
char kind[20];
bool declare_flag = false;
int par_num = 0;
char unde_var[20];
char unde_func[20];
char init_val[20];
char par_list[20];
bool op = false;
char return_type[10];
char label_content[200][20];
int label_count = 0;
int label_save = 0;
bool operating = false;
int compare_flag; // 0 : int, 1 : float
bool else_run = false;
int level_flag = 0;
int max_level = 0;
int if_level[100];
char argument_list[100];
bool argument_op = false;
int if_cnt = 0;
bool div_zero_flag = false;
bool diving = false;

/* Symbol table data structure */
struct Content {
    int scope; // 0:global 1:local 2:local local ...
    char name[100];
    char entryType[100]; // function parameter variable
    char dataType[20]; // 0:int 1:float 2:bool 3:string 4:void
    char parameter[100];
    bool hasValue; 
    bool isDump;
} SymbolTable[100];

%}

%union {
    int i_val;
    double f_val;
    char* string;
}

//* Token without return */
%token PRINT 
%token IF ELSE FOR WHILE
%token SEMICOLON
%token ADD SUB MUL DIV MOD INC DEC
%token MD LD MTE LTE EQ NE
%token ASGN ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token AND OR NOT
%token LB RB LCB RCB LSB RSB COMMA
%token VOID INT FLOAT BOOL STRING
%token TRUE FALSE RET
%token COMMENTLINE NEWLINE

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STR_CONST
%token <string> ID

/* Nonterminal with return, which need to sepcify type */
%type <f_val> stat
%type <i_val> type assignment_operator
%type <f_val> declaration
%type <f_val> compound_stat
%type <f_val> expression_stat
%type <f_val> initializer
%type <f_val> print_func
%type <f_val> selection_stat
%type <string> postfix_expression primary_expression assignment_expression
%type <string> unary_expression multiplicative_expression cast_expression


%left MD LD MTE LTE EQ NE AND OR NOT
%left ADD SUB
%left MUL DIV MOD
%left INC DEC


/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : program stat
    |
;

stat
    : declaration
    | compound_stat
    | expression_stat
    | print_func {}
    | function_definition {}
    | selection_stat {
        if_cnt++;
        gencode_function(10, 5, "", "");
        if (if_cnt == max_level) {
            level_flag = max_level;
            printf("complete id\n");
        }
    }
    | while_stat {}
    | return_stat {}
    | NEWLINE {
        if (has_new_symbol(scope_flag) && need_print && pp_flag && in_if) {
            dump_symbol(scope_flag);
        } else {
            dump_symbol(scope_flag+1);
        }
        need_print = false;

        if (err_flag) {
            err_flag = false;
            yyerror(error_message);
        }
        strcpy(error_message, "");
        strcpy(buf, "");
    }
    | COMMENTLINE {}
;

return_stat
    : RET SEMICOLON {
        gencode_function(8, 0, "", "");
    }
    | RET {
        op = true;
    } expression {
        gencode_function(8, 0, "", "");
        op = false;
    }
;

while_stat
    : WHILE {
        level_flag++;
        if_level[level_flag]++;
        gencode_function(10, 0, "", "");
    } LB { op = true; } expression {
        op = false;
    } RB {
    } compound_stat {
        gencode_function(10, 3, "", "");
        if (level_flag > max_level) {
            max_level = level_flag;
        }
        level_flag--;
    }
;

selection_stat
    : IF { op = true; } LB { 
        level_flag++;
        if_level[level_flag]++;
    } expression {
        op = false;
    } RB compound_stat {
        gencode_function(10, 4, "", "");
        if (level_flag > max_level) {
            max_level = level_flag;
        }
        level_flag--;
    } else_stat {
        in_if = false;
    }
;

else_stat
    : ELSE selection_stat {}
    | ELSE compound_stat {}
    | {}
;

function_definition
	: type ID LB parameter_list RB {
        if ($1 == 0) {
            strcpy(return_type, "ireturn");
        } else if ($1 == 1) {
            strcpy(return_type, "freturn");
        } else if ($1 == 2) {
            strcpy(return_type, "ireturn");
        } else if ($1 == 3) {
            strcpy(return_type, "areturn");
        } else {
            strcpy(return_type, "return");
        }
        gencode_function(2, $1, $2, "");
    } compound_stat {
        int lookup_result = lookup_symbol ($2, scope_flag, "function");
        if (lookup_result == -1 || lookup_result == 2 || lookup_result == 3) {
            insert_symbol($2, "function", $1, true);
            fprintf(file, ".end method\n");
        } else if (lookup_result == 0) {
            strcpy(error_message, "Redeclared function ");
            strcat(error_message, $2);
            err_flag = true;
        } else {
            int correct = 0;

            // parameter testing
            if (strcmp(par, SymbolTable[value_idx].parameter) == 0) {
                correct++;
            } else {
                // error
                strcpy(error_message, "function formal parameter is not the same");
                err_flag = true;
            }

            // return type testing
            if (strcmp(SymbolTable[value_idx].dataType, "int") == 0 && $1 == 0) {
                correct++;
            } else if (strcmp(SymbolTable[value_idx].dataType, "float") == 0 && $1 == 1) {
                correct++;
            } else if (strcmp(SymbolTable[value_idx].dataType, "bool") == 0 && $1 == 2) {
                correct++;
            } else if (strcmp(SymbolTable[value_idx].dataType, "string") == 0 && $1 == 3) {
                correct++;
            } else if (strcmp(SymbolTable[value_idx].dataType, "void") == 0 && $1 == 4) {
                correct++;
            } else {
                // error
                strcpy(error_message, "function return type is not the same");
                err_flag = true;
            }

            if (correct == 2) {
                SymbolTable[value_idx].hasValue = true;
                fprintf(file, ".end method\n");
            }

            // printf("%s\n", par);
            // printf("%s\n", SymbolTable[value_idx].parameter);
            // printf("%s\n", SymbolTable[value_idx].dataType);
            // printf("%d\n", $1);
            value_idx = 0;
        }
        strcpy(par, "");
        strcpy(return_type, "");
        par_num = 0;
    }
;

compound_stat
    : LCB RCB {}
    | LCB block_item_list RCB {}
    | LCB block_item_list RCB stat {}
;

block_item_list
    : stat {}
	| block_item_list stat {}
;

expression_stat
    : SEMICOLON {}
    | expression SEMICOLON {}
;

expression
    : assignment_expression
    | expression COMMA assignment_expression
;

assignment_expression
	: conditional_expression {}
	| unary_expression assignment_operator {
        op = true; 
        operating = true;
        int type = get_id_type($1);
        char reg_num[5];
        sprintf(reg_num, "%d", get_register_num($1));
        if (strcmp(reg_num, "-1") == 0) {
            if ($2 != 0) {
                if (type == 0) {
                    gencode_function(6, 2, "I", $1);
                } else  if(type == 1) {
                    gencode_function(6, 2, "F", $1);
                }
            }
        } else {
            if ($2 != 0) {
                if (type == 0) {
                    gencode_function(6, 1, "i", reg_num);
                } else  if(type == 1) {
                    gencode_function(6, 1, "f", reg_num);
                }
            }
        }
    } assignment_expression {
        op = false;
        // printf("%s\n", $1);
        char reg_num[5];
        sprintf(reg_num, "%d", get_register_num($1));
        int type = get_id_type($1);

        if ($2 == 1) {
            gencode_function(7, 0, "", "*");
        } else if ($2 == 2) {
            gencode_function(7, 0, "", "/");
        } else if ($2 == 3) {
            if (type == 0) {
                // problem not identity post type
                fprintf(file, "\tf2i\n");
                fprintf(file, "\tswap\n");
                fprintf(file, "\tf2i\n");
                fprintf(file, "\tswap\n");
            } else {
                // error
                strcpy(error_message, "MOD type error");
                err_flag = true;
            }
            gencode_function(7, 0, "", "%");
        } else if ($2 == 4) {
            gencode_function(7, 0, "", "+");
        } else if ($2 == 5) {
            gencode_function(7, 0, "", "-");
        }

        if (strcmp(reg_num, "-1") == 0) {
            if (type == 0) {
                gencode_function(6, 3, "I", $1);
            } else  if(type == 1) {
                gencode_function(6, 3, "F", $1);
            }
        } else {
            if (type == 0) {
                gencode_function(6, 0, "i", reg_num);
            } else if (type == 1) {
                gencode_function(6, 0, "f", reg_num);
            } else if (type == 2) {
                gencode_function(6, 0, "b", reg_num);
            } else if (type == 3) {
                // jack
                gencode_function(6, 0, "a", reg_num);
            }
        }
        operating = false;
    }
;

unary_expression
	: postfix_expression {
        $$ = $1;
    }
;


postfix_expression
	: primary_expression {
        $$ = $1;
    }
	| ID LB RB {
        if (lookup_symbol ($1, scope_flag, "function") == -1) {
            strcpy(error_message, "Undeclared function ");
            strcat(error_message, $1);
            err_flag = true;
        } else {
            if (strcmp(SymbolTable[value_idx].parameter, "") != 0) {
                // error
                strcpy(error_message, "function formal parameter is not the same");
                err_flag = true;
            }
            gencode_function(5, 0, $1, "");
        }
        strcpy(init_val, "-1");
    }
	| ID LB { 
        op = true;
        operating = false;
        argument_op = true;
    } argument_expression_list RB {
        operating = true;
        if (lookup_symbol ($1, scope_flag, "function") == -1) {
            strcpy(error_message, "Undeclared function ");
            strcat(error_message, $1);
            err_flag = true;
        } else {
            char tmp_par[100];
            strcpy(tmp_par, SymbolTable[value_idx].parameter);
            strcat(tmp_par, ", ");
            if (strcmp(tmp_par, argument_list) != 0) {
                // error
                strcpy(error_message, "function formal parameter is not the same");
                err_flag = true;
            }
            // printf("%s\n", argument_list);
            // printf("%s\n", SymbolTable[value_idx].parameter);
            gencode_function(5, 0, $1, "");

        }
        strcpy(argument_list, "");
        op = false;
        argument_op = false;
        strcpy(init_val, "-1");
    }
	| postfix_expression INC {
        int type = get_id_type($1);
        char reg_num[5];
        sprintf(reg_num, "%d", get_register_num($1));
        if (strcmp(reg_num, "-1") == 0) {
            gencode_function(7, type, $1, "++g");
        } else {
            gencode_function(7, type, reg_num, "++");
        }
    }
	| postfix_expression DEC {
        int type = get_id_type($1);
        char reg_num[5];
        sprintf(reg_num, "%d", get_register_num($1));
        if (strcmp(reg_num, "-1") == 0) {
            gencode_function(7, type, $1, "--g");
        } else {
            gencode_function(7, type, reg_num, "--");
        }
    }
;

primary_expression
	: ID {
        $$ = $1;
        if (lookup_symbol ($1, scope_flag, "variable") == -1 && lookup_symbol ($1, scope_flag, "parameter") == -1) {
            strcpy(error_message, "Undeclared variable ");
            strcat(error_message, $1);
            err_flag = true;
        } else {
            if ((op || operating) && scope_flag != 0) gencode_function(4, "", $1, "");
            int type = get_id_type($1);
            if (type == 0) {
                if (argument_op) strcat(argument_list, "int, ");
                compare_flag = 0;
            } else if (type == 1) {
                if (argument_op) strcat(argument_list, "float, ");
                compare_flag = 1;
            } else if (type == 2) {
                if (argument_op) strcat(argument_list, "bool, ");
            } else if (type == 3) {
                if (argument_op) strcat(argument_list, "string, ");
            }
            sprintf(init_val, "-1");
        }
    }
	| I_CONST {
        $$ = "I";
        sprintf(init_val, "%d", $1);
        if ((op || operating) && scope_flag != 0) gencode_function(6, -1, "i", init_val);
        compare_flag = 0;
        if (argument_op) strcat(argument_list, "int, ");
        if ($1 == 0 && diving) div_zero_flag = true; 
    }
    | F_CONST {
        $$ = "F";
        sprintf(init_val, "%f", $1);
        if ((op || operating) && scope_flag != 0) gencode_function(6, -1, "f", init_val);
        compare_flag = 1;
        if (argument_op) strcat(argument_list, "float, ");
        if ($1 == 0 && diving) div_zero_flag = true; 
    }
	| STR_CONST {
        strcpy(init_val, $1);
        if ((op || operating) && scope_flag != 0) gencode_function(6, -1, "s", init_val);
        if (argument_op) strcat(argument_list, "string, ");
    }
    | TRUE {
        sprintf(init_val, "1");
        if ((op || operating) && scope_flag != 0) gencode_function(6, -1, "b", init_val);
        if (argument_op) strcat(argument_list, "bool, ");
    }
    | FALSE {
        sprintf(init_val, "0");
        if ((op || operating) && scope_flag != 0) gencode_function(6, -1, "b", init_val);
        if (argument_op) strcat(argument_list, "bool, ");
    }
	| LB expression RB {}
;

argument_expression_list
	: assignment_expression
	| argument_expression_list COMMA assignment_expression
;

assignment_operator
	: ASGN {
        $$ = 0;
    }
	| MULASGN {
        $$ = 1;
    }
	| DIVASGN {
        $$ = 2;
    }
	| MODASGN {
        $$ = 3;
    }
	| ADDASGN {
        $$ = 4;
    }
	| SUBASGN {
        $$ = 5;
    }
;

conditional_expression
    : logical_or_expression
;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR logical_and_expression
;

logical_and_expression
	: relational_expression
	| logical_and_expression AND relational_expression
;

relational_expression
	: additive_expression
	| relational_expression EQ additive_expression {
        gencode_function(9, 0, "", "");
    }
	| relational_expression NE additive_expression {
        gencode_function(9, 1, "", "");
    }
    | relational_expression LD additive_expression {
        gencode_function(9, 2, "", "");
    }
	| relational_expression MD additive_expression {
        gencode_function(9, 3, "", "");
    }
	| relational_expression LTE additive_expression {
        gencode_function(9, 4, "", "");
    }
	| relational_expression MTE additive_expression {
        gencode_function(9, 5, "", "");
    }
;

additive_expression
	: multiplicative_expression
	| additive_expression ADD multiplicative_expression {
        gencode_function(7, 0, "", "+");
    }
	| additive_expression SUB multiplicative_expression {
        gencode_function(7, 0, "", "-");
    }
;

multiplicative_expression
	: cast_expression {
        $$ = $1;
    }
	| multiplicative_expression MUL cast_expression {
        gencode_function(7, 0, "", "*");
    }
	| multiplicative_expression {
        diving = true;
    } DIV cast_expression {
        diving = false;
        // process div 0 problem
        if (div_zero_flag) {
            div_zero_flag = false;
            // error div 0
            strcpy(error_message, "DIV 0 error");
            err_flag = true;
        }
        gencode_function(7, 0, "", "/");
    }
	| multiplicative_expression MOD cast_expression {
        // problem
        if (strcmp($1, "I") != 0 && strcmp($1, "F") != 0) {
            // id
            int type = get_id_type($1);
            printf("%d\n", type);
            if (type == 0) strcpy($1, "I"); 
            else if (type == 1) strcpy($1, "F");
        }
        if (strcmp($3, "I") != 0 && strcmp($3, "F") != 0) {
            // id
            int type = get_id_type($3);
            printf("%d\n", type);
            if (type == 0) strcpy($3, "I"); 
            else if (type == 1) strcpy($3, "F");
        }
        // printf("%s %s\n", $1, $3);
        if (strcmp($1, "I") == 0 && strcmp($3, "I") == 0) {
            gencode_function(11, 0, "", "");
        } else {
            // error
            strcpy(error_message, "MOD float error");
            err_flag = true;
        }
        gencode_function(7, 0, "", "%");
    }
;

cast_expression
	: unary_expression {
        $$ = $1;
    }
;

declaration
	: type ID ASGN {
        op = true;
        operating = true;
    } initializer SEMICOLON {
        int lookup_result = lookup_symbol ($2, scope_flag, "variable");
        if (lookup_result == -1 || lookup_result == 2 || lookup_result == 3) {
            insert_symbol($2, "variable", $1, true);
            gencode_function(1, $1, $2, init_val);
        } else {
            strcpy(error_message, "Redeclared variable ");
            strcat(error_message, $2);
            err_flag = true;
        }
        operating = false;
        op = false;
    }
    | type ID SEMICOLON {
        int lookup_result = lookup_symbol ($2, scope_flag, "variable");
        if (lookup_result == -1 || lookup_result == 2 || lookup_result == 3) {
            insert_symbol($2, "variable", $1, false);
            gencode_function(1, $1, $2, "");
        } else {
            strcpy(error_message, "Redeclared variable ");
            strcat(error_message, $2);
            err_flag = true;
        }
    }
    | type ID LB parameter_list RB SEMICOLON {
        int lookup_result = lookup_symbol ($2, scope_flag, "function");
        if (lookup_result == -1 || lookup_result == 2 || lookup_result == 3) {
            num -= par_num;
            insert_symbol($2, "function", $1, false);
            strcpy(par, "");
            par_num = 0;
        } else if (lookup_result == 0) {
            // avoid delete existed function id
            num -= par_num - 1;
            if (strcmp(SymbolTable[value_idx].parameter, par) != 0 ) {
                // error
                strcpy(error_message, "function formal parameter is not the same");
                err_flag = true;
            }
            if ($1 == 0 && strcmp(SymbolTable[value_idx].dataType, "int") == 0) {
                // do nothing
            } else if ($1 == 1 && strcmp(SymbolTable[value_idx].dataType, "float") == 0) {
                // do nothing
            } else if ($1 == 2 && strcmp(SymbolTable[value_idx].dataType, "bool") == 0) {
                // do nothing
            } else if ($1 == 3 && strcmp(SymbolTable[value_idx].dataType, "string") == 0) {
                // do nothing
            } else if ($1 == 4 && strcmp(SymbolTable[value_idx].dataType, "void") == 0) {
                // do nothing
            } else {
                // error jack
                strcpy(error_message, "function return type is not the same");
                err_flag = true;
            }
            strcpy(par, "");
            par_num = 0;
        } else {
            num -= par_num;
            par_num = 0;
            strcpy(par, "");
            strcpy(error_message, "Redeclared function ");
            strcat(error_message, $2);
            err_flag = true;
        }
        strcpy(par_list, "");
    }
;

initializer
	: LCB initializer_list RCB {}
	| LCB initializer_list COMMA RCB {}
	| assignment_expression {}
;

initializer_list
	: initializer
	| initializer_list COMMA initializer
;

parameter_list
	: type ID {
        if ($1 == 0) {
            strcat(par, "int");
            strcat(par_list, "I");
        } else if ($1 == 1) {
            strcat(par, "float");
            strcat(par_list, "F");
        } else if ($1 == 2) {
            strcat(par, "bool");
            strcat(par_list, "Z");
        } else if ($1 == 3) {
            strcat(par, "string");
            strcat(par_list, "S");
        }
        insert_symbol($2, "parameter", $1, false);
        par_num++;
    }
	| parameter_list COMMA type ID {
        strcat(par, ",");
        if ($3 == 0) {
            strcat(par, " int");
            strcat(par_list, "I");
        } else if ($3 == 1) {
            strcat(par, " float");
            strcat(par_list, "F");
        } else if ($3 == 2) {
            strcat(par, " bool");
            strcat(par_list, "Z");
        } else if ($3 == 3) {
            strcat(par, " string");
            strcat(par_list, "S");
        }
        insert_symbol($4, "parameter", $3, false);
        par_num++;
    }
    | {}
;

print_func
    : PRINT LB STR_CONST RB SEMICOLON {
        gencode_function(3, 3, "", $3);
    }
    | PRINT LB I_CONST RB SEMICOLON {
        char s[50];
        sprintf(s, "%d", $3);
        gencode_function(3, 0, "", s);
    }
    | PRINT LB F_CONST RB SEMICOLON {
        char s[50];
        sprintf(s, "%f", $3);
        gencode_function(3, 1, "", s);
    }
    
    | PRINT LB ID RB SEMICOLON {
        if (lookup_symbol ($3, scope_flag, "variable") == -1 && lookup_symbol ($3, scope_flag, "parameter") == -1) {
            strcpy(error_message, "Undeclared variable ");
            strcat(error_message, $3);
            err_flag = true;
        } else {
            int type = get_id_type($3);
            char reg_num[5];
            sprintf(reg_num, "%d", get_register_num($3));
            if (strcmp(reg_num, "-1") == 0) {
                gencode_function(3, type, reg_num, $3);
            } else {
                gencode_function(3, type, reg_num, "");
            }
        }
    }
;

/* actions can be taken when meet the token or rule */
type
    : INT {
        $$ = 0;
    }
    | FLOAT {
        $$ = 1;
    }
    | BOOL  {
        $$ = 2;
    }
    | STRING {
        $$ = 3;
    }
    | VOID {
        $$ = 4;
    }
;

%%

/* C code section */

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;
    
    strcpy(argument_list, "");
    
    file = fopen("compiler_hw3.j","w");

    fprintf(file,   ".class public compiler_hw3\n"
                    ".super java/lang/Object\n");

    yyparse();

    if (!err_flag) dump_symbol(0);
	if(!err_flag) printf("\nTotal lines: %d \n",yylineno);

    fclose(file);
    /*
    printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n", "Index", "Name", "Kind", "Type", "Scope", "Attribute");
    for (int i=0; i<num; i++) {
        printf("%-10d%-10s%-12s%-10s%-10d%-10s%-10d\n", i, SymbolTable[i].name, SymbolTable[i].entryType, SymbolTable[i].dataType , SymbolTable[i].scope, SymbolTable[i].parameter, SymbolTable[i].isDump);
    }
    */

    return 0;
}

void yyerror(char *s)
{
    if (err_flag) {
        yylineno++;
        printf("%d: %s\n", yylineno, buf);
        printf("\n|-----------------------------------------------|\n");
        printf("| Error found in line %d: %s\n", yylineno, buf);
        printf("| %s", error_message);
        printf("\n|-----------------------------------------------|\n\n");
    }
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno, buf);
    printf("| %s", s);
    printf("\n|-----------------------------------------------|\n\n");
    remove("compiler_hw3.j");
}

/*
void yyerror(char *s)
{
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno, buf);
    printf("| %s", s);
    printf("\n| Unmatched token: %s", yytext);
    printf("\n|-----------------------------------------------|\n");
    exit(-1);
}
*/

/* stmbol table functions */
void create_symbol() {}
void insert_symbol(char* name, char* entryType, int dataType, bool hasValue) {
    SymbolTable[num].scope = scope_flag;
    SymbolTable[num].hasValue = hasValue;
    SymbolTable[num].isDump = false;
    if (dataType == 0) { 
        strcpy(SymbolTable[num].dataType, "int");
    } else if (dataType == 1) { 
        strcpy(SymbolTable[num].dataType, "float");
    } else if (dataType == 2) { 
        strcpy(SymbolTable[num].dataType, "bool");
    } else if (dataType == 3) { 
        strcpy(SymbolTable[num].dataType, "string");
    } else if (dataType == 4) { 
        strcpy(SymbolTable[num].dataType, "void");
    }
    strcpy(SymbolTable[num].name, name);
    strcpy(SymbolTable[num].entryType, entryType);
    if (strcmp(entryType, "parameter") != 0) {
        strcpy(SymbolTable[num].parameter, par);
    } else {
        strcpy(SymbolTable[num].parameter, "");
    }
    num++;
}
int lookup_symbol(char* name, int scope, char* entryType) { 
    // -1: not found
    //  0: found and has value
    //  1: found and no value
    for (int i=0; i<num; i++) {
        if (strcmp(name, SymbolTable[i].name) == 0 && !SymbolTable[i].isDump && SymbolTable[i].scope <= scope && strcmp(entryType, SymbolTable[i].entryType) == 0) {
            if (SymbolTable[i].scope == scope ) {
                // the name is existed in the scope
                // So can't be declared
                if (SymbolTable[i].hasValue) {
                    value_idx = i;
                    return 0;
                } else {
                    value_idx = i;
                    return 1;
                }
            } else {
                // the name is existed outer the scope
                // it can be declared
                if (SymbolTable[i].hasValue != -1) {
                    value_idx = i;
                    return 2;
                } else {
                    value_idx = i;
                    return 3;
                }
            }
        }
    }
    return -1;
}

int get_register_num (char* name) {
    int reg_num = -1;
    if (strcmp(name, "") != 0) {
        for (int i=0; i<num; i++) {
            if (!SymbolTable[i].isDump && SymbolTable[i].scope < 1 && strcmp(SymbolTable[i].name, name) == 0) {
                reg_num = -1;
                break;
            }
            if (!SymbolTable[i].isDump && SymbolTable[i].scope >= 1 && strcmp(SymbolTable[i].name, name) != 0) {
                reg_num++;
            }
            if (!SymbolTable[i].isDump && SymbolTable[i].scope >= 1 && strcmp(SymbolTable[i].name, name) == 0) {
                reg_num++;
                break;
            }
        }
    } else {
        for (int i=0; i<num; i++) {
            if (!SymbolTable[i].isDump && SymbolTable[i].scope >= 1) {
                reg_num++;
            }
        }
    }
    return reg_num;
}

int get_id_type (char* name) {
    int type;
    for (int i=num - 1; i>=0; i--) {
        if (!SymbolTable[i].isDump && strcmp(SymbolTable[i].name, name) == 0) {
            if (strcmp(SymbolTable[i].dataType, "int") == 0) {
                type = 0;
            } else if (strcmp(SymbolTable[i].dataType, "float") == 0) {
                type = 1;
            } else if (strcmp(SymbolTable[i].dataType, "bool") == 0) {
                type = 2;
            } else if (strcmp(SymbolTable[i].dataType, "string") == 0) {
                type = 3;
            } else {
                type = 4;
            }
            break;
        }
    }
    return type;
}

bool has_new_symbol (int scope) {
    bool flag = false;
    for (int i=0; i<num; i++) {
        if (!SymbolTable[i].isDump && SymbolTable[i].scope == scope) {
            flag = true;
        }
    }
    return flag;
}

void dump_symbol(int scope) {
    int idx = 0;
    bool flag = false;
    for (int i=0; i<num; i++) {
        if (!SymbolTable[i].isDump && SymbolTable[i].scope == scope) {
            flag = true;
        }
    }
    if (flag) printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
           "Index", "Name", "Kind", "Type", "Scope", "Attribute");
    for (int i=0; i<num; i++) {
        if (!SymbolTable[i].isDump && SymbolTable[i].scope == scope) {
            if (strcmp(SymbolTable[i].parameter, "") != 0) printf("%-10d%-10s%-12s%-10s%-10d%s\n", idx, SymbolTable[i].name, SymbolTable[i].entryType, SymbolTable[i].dataType , SymbolTable[i].scope, SymbolTable[i].parameter);
            else printf("%-10d%-10s%-12s%-10s%-10d\n", idx, SymbolTable[i].name, SymbolTable[i].entryType, SymbolTable[i].dataType , SymbolTable[i].scope);
            SymbolTable[i].isDump = true;
            idx++;
        }
    }
    if (flag) printf("\n");
}

/* code generation functions */
void gencode_function(int mode, int type, char* id, char* assignValue) {
    // mode 1 : variable declaration
    // mode 2 : method
    // mode 3 : print
    // mode 4 : use variable
    // mode 5 : use function
    // mode 6 : ldc store load
    // mode 7 : ++ -- + - * / %
    // mode 8 : return
    // mode 9 : compare
    // mode 10: label
    // mode 11: mode type
    char T[3];
    char temp[1000];
    switch (type) {
        case 0:
            strcpy(T, "I");
            break;
        case 1:
            strcpy(T, "F");
            break;
        case 2:
            strcpy(T, "Z");
            break;
        case 3:
            strcpy(T, "I");
            break;
        case 4:
            strcpy(T, "V");
            break;
        default:
            break;
    } 
    switch (mode) {
        case 1 :
            if (scope_flag == 0 && strcmp(assignValue, "") != 0) {
                fprintf(file, ".field public static %s %s = %s\n", id, T, assignValue);
            } else if (scope_flag == 0) {
                fprintf(file, ".field public static %s %s\n", id, T);
            } else if (scope_flag != 0 && strcmp(assignValue, "") == 0) {
                int reg_num = get_register_num("");
                fprintf(file, "\tldc 0\n");
                if (type == 1) fprintf(file, "\ti2f\n");
                if (type == 0) {
                    fprintf(file, "\tistore %d\n", reg_num);
                } else if (type == 1) {
                    fprintf(file, "\tfstore %d\n", reg_num);
                } else if (type == 2) {
                    fprintf(file, "\tistore %d\n", reg_num);
                } else if (type == 3) {
                    fprintf(file, "\tastore %d\n", reg_num);
                } else {
                    fprintf(file, "\tstore %d\n", reg_num);
                }
            } else if (scope_flag != 0 && strcmp(assignValue, "") != 0) {
                int reg_num = get_register_num("");
                if (type == 0) {
                    if (operating) fprintf(file, "\tf2i\n");
                    fprintf(file, "\tistore %d\n", reg_num);
                } else if (type == 1) {
                    fprintf(file, "\tfstore %d\n", reg_num);
                } else if (type == 2) {
                    fprintf(file, "\tistore %d\n", reg_num);
                } else if (type == 3) {
                    fprintf(file, "\tastore %d\n", reg_num);
                } else {
                    fprintf(file, "\tstore %d\n", reg_num);
                }
            }
            strcpy(init_val, "");
            break;
        case 2 :
            if (strcmp(id, "main") == 0) {
                fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
                fprintf(file, ".limit stack 50\n");
                fprintf(file, ".limit locals 50\n");
            } else {
                fprintf(file, ".method public static %s(%s)%s\n", id, par_list, T);
                fprintf(file, ".limit stack 50\n");
                fprintf(file, ".limit locals 50\n");
            }
            strcpy(par_list, "");
            break;
        case 3 :
            // assignValue is value
            if (strcmp(id, "") == 0) {
                if (type == 0) {
                    fprintf(file, "\tldc %s\n", assignValue);
                    fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                    fprintf(file, "\tswap\n");
                    fprintf(file, "\tinvokevirtual java/io/PrintStream/println(I)V\n");
                } else if (type == 1) {
                    fprintf(file, "\tldc %s\n", assignValue);
                    fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                    fprintf(file, "\tswap\n");
                    fprintf(file, "\tinvokevirtual java/io/PrintStream/println(F)V\n");
                } else if (type == 3) {
                    fprintf(file, "\tldc \"%s\"\n", assignValue);
                    fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                    fprintf(file, "\tswap\n");
                    fprintf(file, "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                }
            } else {
                // id is reg_num
                // assignValue is real id
                if (strcmp(id, "-1") == 0) {
                    if (type == 0) {
                        fprintf(file, "\tgetstatic compiler_hw3/%s I\n", assignValue);
                        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                        fprintf(file, "\tswap\n");
                        fprintf(file, "\tinvokevirtual java/io/PrintStream/println(I)V\n");
                    } else if (type == 1) {
                        fprintf(file, "\tgetstatic compiler_hw3/%s F\n", assignValue);
                        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                        fprintf(file, "\tswap\n");
                        fprintf(file, "\tinvokevirtual java/io/PrintStream/println(F)V\n");
                    } else if (type == 2) {
                        fprintf(file, "\tgetstatic compiler_hw3/%s I\n", assignValue);
                    } else if (type == 3) {
                        fprintf(file, "\tgetstatic compiler_hw3/%s A\n", assignValue);
                        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                        fprintf(file, "\tswap\n");
                        fprintf(file, "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                    }
                } else {
                    if (type == 0) {
                        fprintf(file, "\tiload %s\n", id);
                        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                        fprintf(file, "\tswap\n");
                        fprintf(file, "\tinvokevirtual java/io/PrintStream/println(I)V\n");
                    } else if (type == 1) {
                        fprintf(file, "\tfload %s\n", id);
                        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                        fprintf(file, "\tswap\n");
                        fprintf(file, "\tinvokevirtual java/io/PrintStream/println(F)V\n");
                    } else if (type == 3) {
                        fprintf(file, "\taload %s\n", id);
                        fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                        fprintf(file, "\tswap\n");
                        fprintf(file, "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                    }
                }
            }
            break;
        case 4 :
            for (int i=num-1; i>=0; i--) {
                if (strcmp(id, SymbolTable[i].name) == 0 && !SymbolTable[i].isDump) {
                    char dataType1[3];
                    char dataType2[3];
                    if (strcmp(SymbolTable[i].dataType, "int") == 0) {
                        strcpy(dataType1, "i");
                        strcpy(dataType2, "I");
                    } else if (strcmp(SymbolTable[i].dataType, "float") == 0) {
                        strcpy(dataType1, "f");
                        strcpy(dataType2, "F");
                    } else if (strcmp(SymbolTable[i].dataType, "bool") == 0) {
                        strcpy(dataType1, "z");
                        strcpy(dataType2, "Z");
                    } else if (strcmp(SymbolTable[i].dataType, "string") == 0) {
                        strcpy(dataType1, "s");
                        strcpy(dataType2, "S");
                    }
                    if (SymbolTable[i].scope > 0) {
                        int reg_num = get_register_num(id);
                        fprintf(file, "\t%sload %d\n", dataType1, reg_num);
                        if (strcmp(dataType1, "i") == 0 && operating) fprintf(file, "\ti2f\n");
                    } else {
                        fprintf(file, "\tgetstatic compiler_hw3/%s %s\n", id, dataType2);
                        if (strcmp(dataType2, "I") == 0 && operating) fprintf(file, "\ti2f\n");
                    }
                }
            }
            break;
        case 5 :
            for (int i=num-1; i>=0; i--) {
                if (strcmp(id, SymbolTable[i].name) == 0 && !SymbolTable[i].isDump) {
                    char dataType[3];
                    char param[20];
                    char tmp[20];
                    char* pch;
                    strcpy(dataType, "");
                    if (strcmp(SymbolTable[i].dataType, "int") == 0) {
                        strcpy(dataType, "I");
                    } else if (strcmp(SymbolTable[i].dataType, "float") == 0) {
                        strcpy(dataType, "F");
                    } else if (strcmp(SymbolTable[i].dataType, "bool") == 0) {
                        strcpy(dataType, "Z");
                    } else if (strcmp(SymbolTable[i].dataType, "string") == 0) {
                        strcpy(dataType, "S");
                    } else if (strcmp(SymbolTable[i].dataType, "void") == 0) {
                        strcpy(dataType, "V");
                    }

                    strcpy(tmp, SymbolTable[i].parameter);
                    strcpy(param, "");
                    pch = strtok(tmp, ", ");
                    while (pch != NULL) {
                        if (strcmp(pch, "int") == 0) {
                            strcat(param, "I");
                        } else if (strcmp(pch, "float") == 0) {
                            strcat(param, "F");
                        } else if (strcmp(pch, "bool") == 0) {
                            strcat(param, "Z");
                        } else if (strcmp(pch, "string") == 0) {
                            strcat(param, "S");
                        }
                        pch = strtok(NULL, ", ");
                    }

                    fprintf(file, "\tinvokestatic compiler_hw3/%s(%s)%s\n", id, param, dataType);
                    if (strcmp(dataType, "I") == 0 && operating) fprintf(file, "\ti2f\n");
                }
            }
            break;
        case 6:
            // id is value type
            // assignValue is const or reg_num
            // -1 : ldc
            //  0 : store
            //  1 : load
            //  2 : global load
            //  3 : global store
            if (type == -1) {
                if (strcmp(id, "s") == 0) {
                    fprintf(file, "\tldc \"%s\"\n", assignValue);
                } else {
                    fprintf(file, "\tldc %s\n", assignValue);
                }
                if (strcmp(id, "i") == 0 && operating) fprintf(file, "\ti2f\n");
            } else if (type == 0) {
                if (strcmp(id, "i") == 0) fprintf(file, "\tf2i\n");
                if (strcmp(id, "b") == 0) {
                    fprintf(file, "\tistore %s\n", assignValue);
                } else {
                    fprintf(file, "\t%sstore %s\n",id, assignValue);
                }
            } else if (type == 1) {
                if (strcmp(id, "s") == 0) {
                    fprintf(file, "\taload %s\n", assignValue);
                } else {
                    fprintf(file, "\t%sload %s\n", id, assignValue);
                }
                if (strcmp(id, "i") == 0 && operating) fprintf(file, "\ti2f\n");
            } else if (type == 2) {
                fprintf(file, "\tgetstatic compiler_hw3/%s %s\n", assignValue, id);
                if (strcmp(id, "I") == 0 && operating) fprintf(file, "\ti2f\n");
            } else if (type == 3) {
                if (strcmp(id, "I") == 0) fprintf(file, "\tf2i\n");
                fprintf(file, "\tputstatic compiler_hw3/%s %s\n", assignValue, id);
            }
            break;
        case 7:
            // id is reg_num or global id
            // assignValue is operator
            if (strcmp(assignValue, "++") == 0) {
                if (type == 0) {
                    fprintf(file, "\tiload %s\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\tiadd\n");
                    fprintf(file, "\tistore %s\n", id);
                } else {
                    fprintf(file, "\tfload %s\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfadd\n");
                    fprintf(file, "\tfstore %s\n", id);
                }
            } else if (strcmp(assignValue, "--") == 0) {
                if (type == 0) {
                    fprintf(file, "\tiload %s\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\tisub\n");
                    fprintf(file, "\tistore %s\n", id);
                } else {
                    fprintf(file, "\tfload %s\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfsub\n");
                    fprintf(file, "\tfstore %s\n", id);
                }
            } else if (strcmp(assignValue, "+") == 0) {
                fprintf(file, "\tfadd\n");
            } else if (strcmp(assignValue, "-") == 0) {
                fprintf(file, "\tfsub\n");
            } else if (strcmp(assignValue, "*") == 0) {
                fprintf(file, "\tfmul\n");
            } else if (strcmp(assignValue, "/") == 0) {
                fprintf(file, "\tfdiv\n");
            } else if (strcmp(assignValue, "%") == 0) {
                fprintf(file, "\tirem\n");
                fprintf(file, "\ti2f\n");
            } else if (strcmp(assignValue, "++g") == 0) {
                if (type == 0) {
                    fprintf(file, "\tgetstatic compiler_hw3/%s I\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\tiadd\n");
                    fprintf(file, "\tputstatic compiler_hw3/%s I\n", id);
                } else {
                    fprintf(file, "\tgetstatic compiler_hw3/%s F\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfadd\n");
                    fprintf(file, "\tputstatic compiler_hw3/%s F\n", id);
                }
            } else if (strcmp(assignValue, "--g") == 0) {
                if (type == 0) {
                    fprintf(file, "\tgetstatic compiler_hw3/%s I\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\tisub\n");
                    fprintf(file, "\tputstatic compiler_hw3/%s I\n", id);
                } else {
                    fprintf(file, "\tgetstatic compiler_hw3/%s F\n", id);
                    fprintf(file, "\tldc 1\n");
                    fprintf(file, "\ti2f\n");
                    fprintf(file, "\tfsub\n");
                    fprintf(file, "\tputstatic compiler_hw3/%s F\n", id);
                }
            }
            break;
        case 8:
            fprintf(file, "\t%s\n", return_type);
            break;
        case 9:
            if (type == 0) {
                if (compare_flag == 0) {
                    fprintf(file, "\tisub\n");
                } else {
                    fprintf(file, "\tfsub\n");
                }
                fprintf(file, "\tifeq LABEL_TRUE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto LABEL_FALSE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_TRUE%d%d:\n", level_flag, if_level[level_flag]);
            } else if (type == 1) {
                if (compare_flag == 0) {
                    fprintf(file, "\tisub\n");
                } else {
                    fprintf(file, "\tfsub\n");
                }
                fprintf(file, "\tifne LABEL_TRUE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto LABEL_FALSE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_TRUE%d%d:\n", level_flag, if_level[level_flag]);
            } else if (type == 2) {
                if (compare_flag == 0) {
                    fprintf(file, "\tisub\n");
                } else {
                    fprintf(file, "\tfsub\n");
                }
                fprintf(file, "\tiflt LABEL_TRUE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto LABEL_FALSE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_TRUE%d%d:\n", level_flag, if_level[level_flag]);
            } else if (type == 3) {
                if (compare_flag == 0) {
                    fprintf(file, "\tisub\n");
                } else {
                    fprintf(file, "\tfsub\n");
                }
                fprintf(file, "\tifgt LABEL_TRUE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto LABEL_FALSE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_TRUE%d%d:\n", level_flag, if_level[level_flag]);
            } else if (type == 4) {
                if (compare_flag == 0) {
                    fprintf(file, "\tisub\n");
                } else {
                    fprintf(file, "\tfsub\n");
                }
                fprintf(file, "\tifle LABEL_TRUE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto LABEL_FALSE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_TRUE%d%d:\n", level_flag, if_level[level_flag]);
            } else if (type == 5) {
                if (compare_flag == 0) {
                    fprintf(file, "\tisub\n");
                } else {
                    fprintf(file, "\tfsub\n");
                }
                fprintf(file, "\tifge LABEL_TRUE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto LABEL_FALSE%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_TRUE%d%d:\n", level_flag, if_level[level_flag]);
            }
            break;
        case 10:
            // 0 : while begin
            // 1 : while false ( true process in compare )
            // 2 : definition while true label content
            // 3 : while false label
            // 4 : if condition label
            // 5 : selection_stat end
            if (type == 0) {
                fprintf(file, "LABEL_BEGIN%d%d:\n", level_flag, if_level[level_flag]);
                label_count++;
            } else if (type == 3) {
                fprintf(file, "\tgoto LABEL_BEGIN%d%d\n", level_flag, if_level[level_flag]);
                fprintf(file, "LABEL_FALSE%d%d:\n", level_flag, if_level[level_flag]);
                fprintf(file, "\tgoto EXIT_%d\n", level_flag);
                fprintf(file, "EXIT_%d:\n", level_flag);
                label_count++;
            } else if (type == 4) {
                fprintf(file, "\tgoto EXIT_%d\n", level_flag);
                fprintf(file, "LABEL_FALSE%d%d:\n", level_flag, if_level[level_flag]);
            } else if (type == 5) {
                fprintf(file, "\tgoto EXIT_%d\n", level_flag + 1);
                fprintf(file, "EXIT_%d:\n", level_flag+1);
            }

            break;
        case 11:
            fprintf(file, "\tf2i\n");
            fprintf(file, "\tswap\n");
            fprintf(file, "\tf2i\n");
            fprintf(file, "\tswap\n");
            break;
        default :
            break;
    }
}
