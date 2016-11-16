

#ifndef  TABLE_H_
#define  TABLE_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/* Error constants */
#define ERR_MEM_ALLOC 1 /* Memory allocation error */
#define ERR_NULL_BUFFER 2 /* Null buffer error */
#define ERR_B_ADDC_FAIL 3 /* b_addc failure */

/* Numerical constants */
#define PLATY_INT_MAX 32767 /* Maximum int size */
#define MAX_ERR_STRING_CHARS 17 /* Maximum number of characters before truncation */

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the following constants: 255, 0xFF , EOF
 */
#define SEOF 255

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , != , '>' , '<' ,space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', <> ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */

#define ES  12 /* Error state */
#define IS -1    /* Invalid state */

/* State transition table definition */

#define TABLE_COLUMNS 7
/* transition table - type of states defined in separate table */
int st_table[][TABLE_COLUMNS] = { { 1, 6, 4, 4, IS, IS, IS }, /* State 0 */
    { 1, 1, 1, 1, 2, 3, 2 }, /* State 1 */
    { IS, IS, IS, IS, IS, IS, IS }, /* State 2 */
    { IS, IS, IS, IS, IS, IS, IS }, /* State 3 */
    { ES, 4, 4, 4, 7, 5, 5 }, /* State 4 */
    { IS, IS, IS, IS, IS, IS, IS }, /* State 5 */
    { ES, 10, 9, ES, 7, ES, 5 }, /* State 6 */
    { 8, 7, 7, 7, 8, 8, 8 }, /* State 7 */
    { IS, IS, IS, IS, IS, IS, IS }, /* State 8 */
    { ES, 9, 9, ES, ES, ES, 11 }, /* State 9 */
    { ES, ES, ES, ES, ES, ES, 11 }, /* State 10 */
    { IS, IS, IS, IS, IS, IS, IS }, /* State 11 */
    { IS, IS, IS, IS, IS, IS, IS }, /* State 12 */
    { IS, IS, IS, IS, IS, IS, IS }
}; /* State 13 */

/* Accepting state table definition */
#define ASWR     2  /* accepting state with retract */
#define ASNR     3  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS,
                   NOAS, ASWR, ASNR,
                   ASWR
                 };

/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument)
 returning Token
 */
typedef Token (*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */
PTR_AAF aa_table[] = { NULL, NULL, aa_func02, aa_func03, NULL, aa_func05, NULL,
                       NULL, aa_func08,
                       NULL, NULL, aa_func11, aa_func12, NULL
                     };

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  8

char * kw_table[] = { "ELSE", "IF", "INPUT", "OUTPUT", "PLATYPUS", "REPEAT",
                      "THEN", "USING"
                    };

#endif
