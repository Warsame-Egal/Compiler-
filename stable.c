
/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland 5.02 projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h> /* Standard Library functions including file io */
#include <string.h> /* String library for managing character strings */

#include "stable.h" /* Contains definitions for STD, STVR, InitialValue, and bit masks */

extern STD sym_table;

/* Static (local) function prototypes */
static void st_setsize(void);
static void st_incoffset(void);
static int stvrcmp_asc(const void *, const void *);
static int stvrcmp_desc(const void *, const void *);

/*
* Purpose:				Creates a new symbol table
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		malloc, free
* Parameters:			st_size
*							type: int
* Returns:				STD
*/
STD st_create(int st_size)
{
    /* The symbol table descriptor to be returned */
    STD std;

    /* Initialize the values of the STD */
    std.st_offset = std.st_size = 0;

    /* Allocate memory for the array of STVR the size passed in */
    std.pstvr = (STVR *) malloc(sizeof(STVR) * (size_t) st_size);
    /* If the pointer is null, no memory could be allocated. Return empty STD */
    if (std.pstvr == NULL)
        return std;

    /* Create the buffer */
    std.plsBD = b_create(1, 1, 'a');
    /* If the buffer is null, an error occured. Free dynamic memory and return empty STD */
    if (std.plsBD == NULL)
    {
        /* Release previously allocated memory */
        free(std.pstvr);
        return std;
    }
    /* Set the size */
    std.st_size = st_size;
    return std;
}

/*
* Purpose:				Installs a lexeme into the STD
* Author:				Warsame Egal
* Version:				1.0 (2013-11-08)
* Called Functions:		st_lookup, b_get_chmemloc, b_getsize, strlen, strstr, st_incoffset
* Parameters:			sym_table
*							type: STD
*						lexeme
*							type: char *
*						line
*							type: int
* Returns:				int, The offset of the symbol
*/
int st_install(STD sym_table, char *lexeme, int line)
{
    /* Iteration variable */
    int i;
    /* The length of the lexeme */
    int lexeme_length;
    /* The offset of the STVR to return */
    int st_offset;
    /* A pointer to a buffer used by the sym_table */
    Buffer *buffer;
    /* A new STVR struct */
    STVR stvr;
    /* Used to indicate that the plexs need to be recalculated */
    short r_flag = 0;

    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    /* First, check if the symbol already exists */
    st_offset = st_lookup(sym_table, lexeme);
    /* If the symbol exists, return it */
    if (st_offset != -1)
        return st_offset;

    /* Set the buffer to the current one used by sym_table */
    buffer = sym_table.plsBD;

    /* Set the offset to the sym_table's next offset */
    st_offset = sym_table.st_offset;

    /* If the offset is greater than the size, return FAIL */
    if (st_offset == sym_table.st_size)
        return FAIL;

    /* The plex will begin at the memory location of the next character to be added to the buffer */
    stvr.plex = b_get_chmemloc(buffer, b_getsize(buffer));
    stvr.o_line = line;

    /* Add the lexeme to the buffer */
    for (i = 0, lexeme_length = (int) strlen(lexeme); i <= lexeme_length; ++i)
    {
        if (!b_addc(buffer, lexeme[i]))
            return FAIL;

        /* If the r_flag has been set, set the local one */
        if (b_get_r_flag(buffer))
            r_flag = 1;
    }

    /* Set the status_field to its default value */
    stvr.status_field = SF_ZERO;
    stvr.status_field |= SF_DEFAULT;

    /* Reset the DTI */
    stvr.status_field &= SF_RESET_DTI;

    /* Check if the lexeme represents a string */
    if (strstr(lexeme, "#"))
    {
        /* Set DTI as string */
        stvr.status_field |= SF_SET_DTI_STR;

        /* Set the DTI as updated */
        stvr.status_field &= SF_RESET_UPDATE;
        stvr.status_field |= SF_SET_UPDATE;

        /* Set ivalue */
        stvr.i_value.str_offset = -1;
    }
    else
    {
        /* Get the first letter of the lexeme */
        char c = lexeme[0];
        if ((c == 'i' || c == 'I' || c == 'o' || c == 'd'))
        {
            /* Set DTI as integer */
            stvr.status_field |= SF_SET_DTI_INT;

            /* Set ivalue */
            stvr.i_value.int_val = 0;
        }
        else
        {
            /* Set DTI as floating-point */
            stvr.status_field |= SF_SET_DTI_FLP;

            /* Set ivalue */
            stvr.i_value.fpl_val = 0.0f;
        }
    }
    /* Install stvr into the array */
    sym_table.pstvr[sym_table.st_offset] = stvr;

    if (r_flag)
    {
        int i;
        short current_offset = 0;

        for (i = 0; i <= sym_table.st_offset; i++)
        {
            sym_table.pstvr[i].plex = b_get_chmemloc(sym_table.plsBD, current_offset);
            current_offset += (short) strlen(sym_table.pstvr[i].plex) + 1;
        }
    }

    /* Increment the global sym_table st_offset by 1 */
    st_incoffset();
    return st_offset;
}

/*
* Purpose:				Iterates thorugh the array of symbols to see if there is a match
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		strcmp
* Parameters:			sym_table
*							type: STD
*						lexeme
*							type: char *
* Returns:				The offset of the symbol or -1 if it is not found
*/
int st_lookup(STD sym_table, char *lexeme)
{
    /*
     * The offset of the matching STVR to be returned.
     * Initially set to STVR_NOT_FOUND. If a matching
     * VID is not found, this value will be returned.
     */
    int st_offset = STVR_NOT_FOUND;

    /*
     * The current st_offset of the STVR that will be examined.
     * Initially set to the offset of the last STVR struct in
     * sym_table's array of STVRs.
     */
    int current_st_offset = sym_table.st_offset - 1;

    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    /*
     * Loop until the the match is found or until the loop
     * reaches the beginning of the array.
     */
    while (current_st_offset > -1)
    {
        /* A pointer to the current STVR */
        STVR *stvr = &sym_table.pstvr[current_st_offset];

        /* The current VID stored in sym_table */
        char *vid = stvr->plex;

        /*
         * If the VID and lexeme match, set the st_offset to
         * the current_st_offset.
         */
        if (strcmp(lexeme, vid) == 0)
            st_offset = current_st_offset;

        /* Decrement the current_st_offset to find the next STVR */
        --current_st_offset;
    }

    return st_offset;
}

/*
* Purpose:				Attempts to update the symbol type
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		N/A
* Parameters:			sym_table
*							type: STD
*						vid_offset
*							type: int
*						v_type
*							type: char
* Returns:				The offset of the symbol or -1 on failure
*/
int st_update_type(STD sym_table, int vid_offset, char v_type)
{
    /* A pointer to the STVR in the array */
    STVR *stvr;

    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    /* Get the STVR based on the offset */
    stvr = &sym_table.pstvr[vid_offset];

    /* If the update flag is set, it cannot be updated again. Return FAIL */
    if (stvr->status_field & SF_SET_UPDATE)
        return FAIL;

    /* Reset the DTI */
    stvr->status_field &= SF_RESET_DTI;
    switch (v_type)
    {
    case 'F':
        /* Set the DTI to floating-point */
        stvr->status_field |= SF_SET_DTI_FLP;
        break;
    case 'I':
        /* Set the DTI to integer */
        stvr->status_field |= SF_SET_DTI_INT;
        break;
    default:
        /* Invalid v_type, return FAIL */
        return FAIL;
    }

    /* Set the update flag */
    stvr->status_field &= SF_RESET_UPDATE;
    stvr->status_field |= SF_SET_UPDATE;
    return vid_offset;
}

/*
* Purpose:				Attempts to update the symbol value
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		N/A
* Parameters:			sym_table
*							type: STD
*						vid_offset
*							type: int
*						i_value
*							type: InitialValue
* Returns:				The offset of the symbol or -1 on failure
*/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value)
{
    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    /* Get the STVR based on the offset */
    sym_table.pstvr[vid_offset].i_value = i_value;
    return vid_offset;
}

/*
* Purpose:				Returns the type of the symbol
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		N/A
* Parameters:			sym_table
*							type: STD
*						vid_offset
*							type: int
* Returns:				A char representation of the symbol's type
*/
char st_get_type(STD sym_table, int vid_offset)
{
    /* A pointer to the STVR in the array */
    STVR *stvr;
    /* The mask of the status field */
    unsigned short mask;

    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    /* Get the STVR based on the offset */
    stvr = &sym_table.pstvr[vid_offset];
    /* Get the mask */
    mask = stvr->status_field & SF_SET_DTI_STR;
    /* Return the matching type based on the mask value */
    return (mask == SF_SET_DTI_INT) ? 'I' : (mask == SF_SET_DTI_FLP) ? 'F' : 'S';
}

/*
* Purpose:				Destroys the symbol table and removes all memory associated with it
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		free, b_destroy
* Parameters:			sym_table
*							type: STD
*/
void st_destroy(STD sym_table)
{
    /* Free the buffer if needed and set the pointer to NULL */
    if (sym_table.plsBD)
        b_destroy(sym_table.plsBD);

    /* Free the STVR array if needed and set the pointer to NULL */
    if (sym_table.pstvr)
        free(sym_table.pstvr);

    /* Reset st_size to 0 */
    st_setsize();
}

/*
* Purpose:				Prints the contents of the symbol table
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		printf
* Parameters:			sym_table
*							type: STD
* Returns:				The number of items printed
*/
int st_print(STD sym_table)
{
    /* Iteration variable */
    int i;

    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    printf("\nSymbol Table\n");
    printf("____________\n");
    printf("\nLine Number Variable Identifier\n");

    for (i = 0; i < sym_table.st_offset; ++i)
        printf("%2d          %s\n", sym_table.pstvr[i].o_line,
               sym_table.pstvr[i].plex);

    return sym_table.st_offset;
}

/*
* Purpose:				Stores the contents of the symbol table into a file
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		fopen, fprintf, fclose, printf
* Parameters:			sym_table
*							type: STD
* Returns:				The number of records stored
*/
int st_store(STD sym_table)
{
    /* A pointer to the output file to be written to */
    FILE *outputFile;
    /* Iteration variable */
    int i;

    /* If sym_table is invalid, return FAIL */
    if (sym_table.st_size == 0)
        return FAIL;

    /* Try to open a write stream to the file */
    outputFile = fopen("$stable.ste", "w");
    /* If the file cannot be opened, return FAIL */
    if (!outputFile)
        return FAIL;

    /* Write the st_size to the file */
    fprintf(outputFile, "%d", sym_table.st_size);
    /* For each STVR record, write its contents to the file */
    for (i = 0; i < sym_table.st_size; ++i)
    {
        /* A pointer to the current STVR being output */
        STVR *stvr = &sym_table.pstvr[i];

        /* Write the status_field value in hexadecimal */
        fprintf(outputFile, " %4X", stvr->status_field);
        /* Write the lexeme length */
        fprintf(outputFile, " %d", (int) strlen(stvr->plex));
        /* Write the lexeme */
        fprintf(outputFile, " %s", stvr->plex);
        /* Write the line number */
        fprintf(outputFile, " %d", stvr->o_line);
        /* Write the initial value */
        switch (st_get_type(sym_table, i))
        {
        case 'F':
            fprintf(outputFile, " %.2f", stvr->i_value.fpl_val);
            break;
        case 'I':
            fprintf(outputFile, " %d", stvr->i_value.int_val);
            break;
        case 'S':
            fprintf(outputFile, " %d", stvr->i_value.str_offset);
            break;
        }
    }
    fclose(outputFile);
    printf("\nSymbol Table stored.\n");
    return i;
}

/*
* Purpose:				Sorts the symbol table
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		qsort (stvrcmp_asc, stvrcmp_desc), b_create, b_getcapacity
*						b_getmode, strlen, b_get_chmemloc, b_getsize, b_addc, free
* Parameters:			sym_table
*							type: STD
*						vid_offset
*							type: int
* Returns:				A char representation of the symbol's type
*/
int st_sort(STD sym_table, char s_order)
{
    /* Iteration variables */
    int i, j;
    /* A pointer to the sym_table's buffer */
    Buffer *table_buffer;
    /* A pointer to a new buffer used to contain the sorted lexemes */
    Buffer *sorted_buffer;

    /* If sym_table or the s_order is invalid, return FAIL */
    if (sym_table.st_size == 0 || (s_order != 'A' && s_order != 'D'))
        return FAIL;

    /*
     * Sort the STVR array. If s_order is 'A', it is sorted in
     * ascending order, otherwise it is sorted in descending order.
     */
    qsort(sym_table.pstvr, (size_t) sym_table.st_offset, sizeof(STVR),
          (s_order == 'A') ? stvrcmp_asc : stvrcmp_desc);

    /* Make table_buffer point to the sym_table's buffer */
    table_buffer = sym_table.plsBD;

    /*
     * Attempt to create the sorted lexemes buffer using
     * the same properties as the current buffer. Please note
     * that because there is no buffer function to retrieve the
     * inc_factor, a predefined one is provided
     */
    sorted_buffer = b_create(b_getcapacity(table_buffer),
                             ST_BUF_DEFAULT_INC_FATOR, (char) b_getmode(table_buffer));
    /* If the buffer could not be created, return FAIL */
    if (!sorted_buffer)
        return FAIL;

    /* Iterate through all the sorted STVRs, adding their lexemes to the new buffer */
    for (i = 0; i < sym_table.st_offset; ++i)
    {
        /* A pointer to the current STVR */
        STVR *stvr = &sym_table.pstvr[i];
        /* A pointer to the current STVR's plex */
        char *plex = stvr->plex;
        /* The length of the STVR's plex including trailing '\0' */
        int plex_length = (int) strlen(plex) + 1;

        /* Reassign the STVR's plex to equal the new spot in the sorted buffer */
        stvr->plex = b_get_chmemloc(sorted_buffer, b_getsize(sorted_buffer));

        /* Iterate through the characters in plex, adding them to the sorted buffer */
        for (j = 0; j < plex_length; ++j)
            b_addc(sorted_buffer, plex[j]);
    }
    /* Destroy the old buffer */
    free(table_buffer);
    /* Reassign sym_table's buffer to the sorted one */
    sym_table.plsBD = sorted_buffer;
    return SUCCESS;
}

/* Static (local) functions */

/*
* Purpose:				Resets the global symbol table's size to 0
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		N/A
*/
void st_setsize(void)
{
    sym_table.st_size = 0;
}

/*
* Purpose:				Increments the global symbol table's offset by 1
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		N/A
*/
void st_incoffset(void)
{
    ++sym_table.st_offset;
}

/*
* Purpose:				Compares two STVR's in ascending order. Not type safe.
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		strcmp
* Parameters:			s1
*							type: void *
*						s2
*							type: void *
* Returns:				int, 1 if first is greater, -1 if first is lesser, 0 if they are equal
*/
int stvrcmp_asc(const void *s1, const void *s2)
{
    return strcmp(((STVR *) s1)->plex, ((STVR *) s2)->plex);
}

/*
* Purpose:				Compares two STVR's in descending order. Not type safe.
* Author:				Warsame Egal
* Version:				1.0 (2013-11-16)
* Called Functions:		strcmp
* Parameters:			s1
*							type: void *
*						s2
*							type: void *
* Returns:				int, 1 if first is lesser, -1 if first is greater, 0 if they are equal
*/
int stvrcmp_desc(const void *s1, const void *s2)
{
    return -strcmp(((STVR *) s1)->plex, ((STVR *) s2)->plex);
}
