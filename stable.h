
#ifndef STABLE_H_
#define STABLE_H_

/* Bit Masks */
/* Used to zero out a mask */
#define SF_ZERO 0x0000 /* 0000 0000 0000 0000 */
/* The default mask */
#define SF_DEFAULT 0xFFF8 /* 1111 1111 1111 1000 */

#define SF_RESET_UPDATE 0xFFFE /* 1111 1111 1111 1110 */
#define SF_SET_UPDATE 0x0001 /* 0000 0000 0000 0001 */

#define SF_RESET_DTI 0xFFF9 /* 1111 1111 1111 1001 */
#define SF_SET_DTI_INT 0x0002 /* 0000 0000 0000 0010 */
#define SF_SET_DTI_FLP 0x0004 /* 0000 0000 0000 0100 */
#define SF_SET_DTI_STR 0x0006 /* 0000 0000 0000 0110 */

/* Used to indicate that an STVR does not exist */
#define STVR_NOT_FOUND -1

/* Used for a new buffer's default inc_factor */
#define ST_BUF_DEFAULT_INC_FATOR 15

#include "buffer.h"

typedef union InitialValue
{
    /* integer variable initial value */
    int int_val;
    /* floating-point variable initial value */
    float fpl_val;
    /* string variable initial value (offset) */
    int str_offset;
} InitialValue;

typedef struct SymbolTableVidRecord
{
    /* variable record status field*/
    unsigned short status_field;
    /* pointer to lexeme (VID name) in CA */
    char * plex;
    /* line of first occurrence */
    int o_line;
    /* variable initial value */
    InitialValue i_value;
    /*offset from the beginning of data segment*/
    size_t ds_offset;
} STVR;

typedef struct SymbolTableDescriptor
{
    /* pointer to array of STVR */
    STVR *pstvr;
    /* size in number of STVR elements */
    int st_size;
    /* offset in number of STVR elements */
    int st_offset;
    /* pointer to the lexeme storage buffer descriptor */
    Buffer *plsBD;
} STD;

/* Function prototypes */
STD st_create(int);
int st_install(STD, char *, int);
int st_lookup(STD, char *);
int st_update_type(STD, int, char);
int st_update_value(STD, int, InitialValue);
char st_get_type(STD, int);
void st_destroy(STD);
int st_print(STD);
int st_store(STD);
int st_sort(STD, char);

#endif /* STABLE_H_ */
