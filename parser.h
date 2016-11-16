
#ifndef PARSER_H_
#define PARSER_H_

/* Header file includes for required modules */
#include "buffer.h" /* Contains definitions pertaining to the Buffer */
#include "token.h" /* Contains definitions for the Token */
#include "stable.h" /* Contains definitions for the Symbol Table */

#define NO_ATTR		0 /* Represents no attribute for a token */

#define ELSE		0 /* Represents the ELSE keyword */
#define IF			1 /* Represents the IF keyword */
#define INPUT		2 /* Represents the INPUT keyword */
#define OUTPUT		3 /* Represents the OUTPUT keyword */
#define PLATYPUS	4 /* Represents the PLATYPUS keyword */
#define REPEAT		5 /* Represents the REPEAT keyword */
#define THEN		6 /* Represents the THEN keyword */
#define USING		7 /* Represents the USING keyword */

#define KWT_SIZE	8 /* The number of elements in the keyword table */

int synerrno; /* Parser error counter */

/* Static (local) global variables */
static Token lookahead_token; /* The token to be evaluated and matched against */
static Buffer *sc_buf; /* A pointer to the Scanner Buffer that contains the source code */

/* Scanner global token retreival function (defined in scanner.c) */
extern Token mlwpar_next_token(Buffer *);

extern int line; /* The current line number */
extern STD sym_table; /* The global symbol table */
extern Buffer *str_LTBL; /* A pointer to a Buffer that holds the string literals */
extern char *kw_table[KWT_SIZE]; /* A pointer to an array of c-strings that make up the keywords */

/* Function prototypes */
void parser(Buffer *in_buf);
void match(int, int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char*);

void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void arithmetic_expression(void);
void assignment_expression(void);
void assignment_statement(void);
void conditional_expression(void);
void input_statement(void);
void iteration_statement(void);
void logical_and_expression(void);
void logical_and_expression_p(void);
void logical_or_expression(void);
void logical_or_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void opt_statements(void);
void opt_variable_list(void);
void output_statement(void);
void primary_a_relational_expression(void);
void primary_arithmetic_expression(void);
void primary_s_relational_expression(void);
void primary_string_expression(void);
void program(void);
void relational_expression(void);
void relational_expression_p(void);
void relational_expression_p_str(void);
void selection_statement(void);
void statement(void);
void statements(void);
void statements_p(void);
void string_expression(void);
void string_expression_p(void);
void unary_arithmetic_expression(void);
void variable_identifier(void);
void variable_list(void);
void variable_list_p(void);

#endif
