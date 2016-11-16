
/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland 5.02 projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */
#include <errno.h>   /* access to macro ERANGE used by global errno */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals. It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum; /* defined in platy_st.c - run-time error number */
extern int errno; /* run-time error number used by strtol, strtod */
extern STD sym_table; /* Global symbol table */

static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* scanner.c static(local) function  prototypes */
static int char_class(char); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char *, int *); /* keywords lookup function */
static Token runtime_error(int errorCode); /* generic function that sets scerrnum and returns error token */

/*
 * Purpose:				Initializes the scanner and resets its values.
 * Author:				Svillen Ranev
 * Called Functions:	b_isempty, b_set_getc_offset, b_reset
 * Parameters:			sc_buf
 *							type: Buffer *
 * Returns:				int, EXIT_SUCCESS or EXIT_FAILURE
 */
int scanner_init(Buffer * sc_buf)
{
    if (b_isempty(sc_buf))
        return EXIT_FAILURE;/*1*/
    b_set_getc_offset(sc_buf, 0);/* in case the buffer has been read previously  */
    b_reset(str_LTBL);
    line = 1;
    return EXIT_SUCCESS;/*0*/
    /*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
 * Purpose:				Reads a lexeme character by character from the buffer and attempts
 * 						to match it to an appropriate Token.
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	runtime_error, b_getc, isspace, b_retract, strcpy, b_get_getc_offset,
 * 						b_getsize, get_next_state, malloc, aa_func02, aa_func03, aa_func05,
 * 						aa_func08, aa_func11, aa_func12
 * Parameters:			sc_buf
 *							type: Buffer *
 * Returns:				The next token
 * Algorithm:			Gets a character and analyzes it. If the context of the character demands
 * 						a certain sequence of trailing characters, the function gets and analyzes
 * 						those characters as well. In the event of a runtime error, the function
 * 						runtime_error() is called with the appropriate predefined error code
 */
Token mlwpar_next_token(Buffer * sc_buf)
{
    Token t; /* token to return after recognition */
    unsigned char c; /* input symbol */
    int state = 0; /* initial state of the FSM */
    short lexstart; /*start offset of a lexeme in the input buffer */
    short lexend; /*end offset of a lexeme in the input buffer */
    int accept = NOAS; /* type of state - initially not accepting */
    int i; /* iterator variable */

    /* If the buffer is NULL, return runtime error */
    if (!sc_buf)
        return runtime_error(ERR_NULL_BUFFER);

    /* Label for goto continuation, removes need for infinite loop that generates warnings */
getNextCharacter:

    /* Get the next character from the buffer */
    c = (unsigned char) b_getc(sc_buf);

    /* Ignore the token if it is a space */
    if (isspace(c))
    {
        /* If a new-line character was encountered, increment the line count */
        if (c == '\n' || c == '\r')
            line++;
        /* Skip and get the next character */
        goto getNextCharacter;
    }

    /* If end of file has been encountered, return the SEOF token */
    if (c == '\0' || c == SEOF)
    {
        t.code = SEOF_T;
        return t;
    }

    switch (c)
    {
    case '{': /* left brace */
        t.code = LBR_T;
        return t;
    case '}': /* right brace */
        t.code = RBR_T;
        return t;
    case '(': /* left parenthesis */
        t.code = LPR_T;
        return t;
    case ')': /* left parenthesis */
        t.code = RPR_T;
        return t;
    case ',': /* comma*/
        t.code = COM_T;
        return t;
    case '+': /* plus sign */
        t.code = ART_OP_T;
        t.attribute.arr_op = PLUS;
        return t;
    case '-': /* minus sign */
        t.code = ART_OP_T;
        t.attribute.arr_op = MINUS;
        return t;
    case '*': /* asterisk */
        t.code = ART_OP_T;
        t.attribute.arr_op = MULT;
        return t;
    case '/': /* forward slash */
        t.code = ART_OP_T;
        t.attribute.arr_op = DIV;
        return t;
    case '>': /* greater-than sign */
        t.code = REL_OP_T;
        t.attribute.rel_op = GT;
        return t;
    case ';': /* less-than sign */
        t.code = EOS_T;
        return t;
    case '=':
        /*
         * The '=' character on its own represents the assignment operator, but in the special case  * that it is followed by another '=' character, then the resulting token is the equality relational operator.
         */

        /* Get the next character from the buffer */
        c = (unsigned char) b_getc(sc_buf);

        /* If the next character is also '=' then the scanner has found the equality operator */
        if (c == '=')
        {
            t.code = REL_OP_T;
            t.attribute.rel_op = EQ;
            return t;
        }

        /* Any other character means the token is simply the assignment operator */
        t.code = ASS_OP_T;
        /* Retract the buffer so that we can parse the second character again in the context of a new token */
        b_retract(sc_buf);
        return t;
    case '<':
        /*
         * The '<' character on its own represents the 'less-than' relational operator, but in the special case  * that it is followed by the '>' character, then the resulting token is the string concatenation operator.
         */
        /* Get the next character from the buffer */
        c = (unsigned char) b_getc(sc_buf);

        /* If the next character is '>' then the scanner has found the string concatenation operator. */
        if (c == '>')
        {
            t.code = SCC_OP_T;
            return t;
        }

        /* Any other character means the token is simply the 'less-than' operator */
        t.code = REL_OP_T;
        t.attribute.rel_op = LT;
        /* Retract the buffer so that we can parse the second character again in the context of a new token */
        b_retract(sc_buf);
        return t;
    case '!':
        /*
         * The '!' character immediately followed by the '=' character represents the 'not-equal-to' relational operator.
         * The '!' character immediately followed by the '<' character signals the beginning of a comment.
         * If the '!' character is followed by anything else, then there is a syntax error.
         */
        /* Get the next character from the buffer */
        c = (unsigned char) b_getc(sc_buf);

        /* If the next character is '=', then the token is the 'not-equal-to' relational operator. */
        if (c == '=')
        {
            t.code = REL_OP_T;
            t.attribute.rel_op = NE;
            return t;
        }

        /* If the next character is not '<' then the comment is not valid */
        if (c != '<')
        {
            t.code = ERR_T;
            t.attribute.err_lex[0] = '!';
            t.attribute.err_lex[1] = (signed char) c;
            t.attribute.err_lex[2] = '\0';

            /* Skip the rest of the input until the next line */
            do
            {
                /* Get the next character from the buffer */
                c = (unsigned char) b_getc(sc_buf);
                /* If SEOF is found retract the buffer and return the token */
                if (c == SEOF)
                {
                    b_retract(sc_buf);
                    return t;
                }
            }
            while (c != '\n' && c != '\r' && c != '\0');
            ++line;
            return t;
        }

        /* Get the next character from the buffer */
        c = (unsigned char) b_getc(sc_buf);

        /*
         * Loop through all the characters in the buffer until the scanner encounters a newline ('\n').
         * If an end of file character (SEOF) is encountered before a new line, issue an
         * error as per grammar rules.
         */
        while (c != '\n' && c != '\r' && c != '\0' && c != SEOF)
            c = (unsigned char) b_getc(sc_buf);

        /* If a newline was encountered, the scanner has found the end of the comment and will process the next token */
        if (c == '\n' || c == '\r')
        {
            ++line;
            goto getNextCharacter;
        }

        /* If SEOF has been encountered before a newline, that is an error according to the grammar */
        t.code = ERR_T;
        strcpy(t.attribute.err_lex, "Missing newline");
        return t;
    case '.':
        /*
         * The '.' character followed by "AND." represents the AND logical operator.
         * The '.' character followed by "OR." represents the OR logical operator.
         *
         * Anything else following the '.' character is a syntax error.
         */

        /* Set the lexstart to the location of the next character in case of error */
        lexstart = b_get_getc_offset(sc_buf);

        /* Get the next character from the buffer */
        c = (unsigned char) b_getc(sc_buf);

        /*
         * If the next character is 'A', keep testing to see if the remaining characters
         * represent the AND logical operator.
         */
        if (c == 'A')
        {
            /* Get the next character from the buffer */
            c = (unsigned char) b_getc(sc_buf);
            /*
             * If the next character is 'N', keep testing to see if the remaining characters
             * represent the AND logical operator.
             */
            if (c == 'N')
            {
                /* Get the next character from the buffer */
                c = (unsigned char) b_getc(sc_buf);
                /*
                 * If the next character is 'D', keep testing to see if the remaining characters
                 * represent the AND logical operator.
                 */
                if (c == 'D')
                {
                    /* Get the next character from the buffer */
                    c = (unsigned char) b_getc(sc_buf);
                    /*
                     * If the next character is '.' then the token
                     * represents the AND logical operator
                     */
                    if (c == '.')
                    {
                        t.code = LOG_OP_T;
                        t.attribute.log_op = AND;
                        return t;
                    }
                }
            }
        }
        /*
         * If the next character is 'O', test to see if the remaining characters
         * represent the OR logical operator.
         */
        else if (c == 'O')
        {
            /* Get the next character from the buffer */
            c = (unsigned char) b_getc(sc_buf);
            /*
             * If the next character is 'R', keep testing to see if the remaining character
             * completes the OR logical operator.
             */
            if (c == 'R')
            {
                /* Get the next character from the buffer */
                c = (unsigned char) b_getc(sc_buf);
                /*
                 * If the next character is '.' then the token
                 * represents the OR logical operator
                 */
                if (c == '.')
                {
                    t.code = LOG_OP_T;
                    t.attribute.log_op = OR;
                    return t;
                }
            }
        }
        /* The '.' is out of context; return error */
        t.code = ERR_T;
        t.attribute.err_lex[0] = '.';
        t.attribute.err_lex[1] = '\0';
        /* Reset the buffer's location to the next char after the dot */
        b_set_getc_offset(sc_buf, lexstart);
        return t;
    case '"':
    {
        /* If the next character is '"', begin processing the following characters as a string literal  */

        /* Declare a variable to hold the length of the lexeme */
        int lexeme_length;

        /* Set the lexstart to the location of the first double quote */
        lexstart = (short) (b_get_getc_offset(sc_buf) - 1);

        /* Begin getting and validating each character until the ending double quote is encountered */
        do
        {
            /* Get the next character from the buffer */
            c = (unsigned char) b_getc(sc_buf);

            /* If a newline character has been encountered, increment the line number counter */
            if (c == '\n' || c == '\r')
            {
                ++line;
                continue;
            }

            /* If SEOF of '\0' is encountered, return an error token */
            if (c == SEOF || c == '\0')
            {
                /* Set the lexend */
                lexend = (short) b_get_getc_offset(sc_buf);

                /* Determine the length of the lexeme including the SEOF or '\0' */
                lexeme_length = (int) (lexend - lexstart);

                /* Reset the getc_offset to the beginning of the string */
                b_set_getc_offset(sc_buf, lexstart);

                /* Iterate over the buffer and capture the string into the err_lex */
                for (i = 0; i < lexeme_length; ++i)
                {
                    /* Get the next character from the buffer */
                    c = (unsigned char) b_getc(sc_buf);

                    /*
                     * If i is less than the maximum number of
                     * characters allowed in the error string, add the
                     * character to the error string.
                     */
                    if (i < MAX_ERR_STRING_CHARS)
                        t.attribute.err_lex[i] = (signed char) c;
                    /* If i is less than ERR_LEN, add a '.' */
                    else if (i < ERR_LEN)
                        t.attribute.err_lex[i] = '.';
                    /* If i is greater than or equal to ERR_LEN, continue */
                    else
                        break;
                }
                /* Add the trailing '\0' */
                t.attribute.err_lex[i] = '\0';
                t.code = ERR_T;

                /* Set the buffer back to the end of the input */
                b_set_getc_offset(sc_buf, lexend);

                return t;
            }
        }
        while (c != '"');

        /* Set the lexend  */
        lexend = b_get_getc_offset(sc_buf);

        /* Initialize lexeme_length */
        lexeme_length = (int) lexend - lexstart;

        /* Reset the getc_offset to the beginning of the string */
        b_set_getc_offset(sc_buf, lexstart);

        /* Set the str_offset of the token to the addc_offset of the str_LTBL */
        t.attribute.str_offset = b_getsize(str_LTBL);

        /* Add all the characters of the string to the str_LTBL */
        for (i = 0; i < lexeme_length; ++i)
        {
            /* Get the next character from the buffer */
            c = (unsigned char) b_getc(sc_buf);

            if (c == '"')
                continue;

            if (!b_addc(str_LTBL, (signed char) c))
                return runtime_error(ERR_B_ADDC_FAIL);
        }
        /* Add the null-terminator to the string */
        if (!b_addc(str_LTBL, '\0'))
            return runtime_error(ERR_B_ADDC_FAIL);

        t.code = STR_T;
        return t;
    }
    }

    /* Process state transition table */
    if (isalnum(c))
    {
        /* A pointer to an array of chars that make up the lexeme */
        char *lexeme = NULL;
        /* The size of the lexeme (in characters) */
        int lexeme_size = 0;
        /*
         * Get the starting position of the current lexeme by getting
         * the getc_offset of the next character and subtracting 1.
         */
        lexstart = (short) (b_get_getc_offset(sc_buf) - 1);

        /* Get the next state */
        state = get_next_state(state, (signed char) c, &accept);

        while (accept == NOAS)
        {
            /* Get the next character from the buffer */
            c = (unsigned char) b_getc(sc_buf);
            state = get_next_state(state, (signed char) c, &accept);
        }

        /* If the machine is in an accepting state with retract, retract the buffer */
        if (accept == ASWR)
            b_retract(sc_buf);

        /*
         * Get the ending position of the current lexeme by getting
         * the getc_offset of the next token.
         */
        lexend = (short) (b_get_getc_offset(sc_buf));
        /* Set the lexeme size based on the lexstart and lexend values */
        lexeme_size = (int) (lexend - lexstart);

        /* Reset the buffer back to the beginning of the lexeme */
        b_set_getc_offset(sc_buf, lexstart);

        /* Allocate memory for the lexeme string */
        lexeme = (char *) malloc(sizeof(char) * (size_t) lexeme_size + 1);
        /* If the allocation fails, produce a run-time error */
        if (lexeme == NULL)
            return runtime_error(ERR_MEM_ALLOC);

        /* Copy the string from the buffer character by character */
        for (i = 0; i < lexeme_size; ++i)
            lexeme[i] = b_getc(sc_buf);

        /* Add the null terminator to the string */
        lexeme[i] = '\0';

        /*
         * Call the accepting function for the current state,
         * passing the pointer to the lexeme
         */
        t = aa_table[state](lexeme);

        /* Free the lexeme from memory */
        free(lexeme);
        return t;
    }

    /* This code will run when any invalid or unexpected character is encountered. */
    t.code = ERR_T;
    t.attribute.err_lex[0] = (signed char) c;
    t.attribute.err_lex[1] = '\0';
    return t;
}

/*
 * Purpose:				Sets scerrnum to the passed in errorCode number and returns an error token.
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	runtime_error, b_getc, isspace, b_retract, strcpy, b_get_getc_offset,
 * 						b_getsize, get_next_state, malloc, aa_func02, aa_func03, aa_func05,
 * 						aa_func08, aa_func11, aa_func12
 * Parameters:			errorCode
 *							type: int
 * Returns:				An error token
 */
Token runtime_error(int errorCode)
{
    /* The error message string */
    static const char errorMessage[] = "RUNTIME ERROR:";
    /* The token to return */
    Token t;
    /* Set the token code to error */
    t.code = ERR_T;
    /* Copy the error message into the err_lex */
    strcpy(t.attribute.err_lex, errorMessage);
    /* Set the global error code to the parameter error code */
    scerrnum = errorCode;
    return t;
}

/*
 * Purpose:				Determines the next state of the machine based on current state
 * 						and next character
 * Author:				Warsame Egal
 * Called Functions:	char_class
 * Parameters:			state
 *							type: int
 *						c
 *							type: char
 *						accept
 *							type: int *
 */
int get_next_state(int state, char c, int *accept)
{
    int col;
    int next;
    col = char_class(c);
    next = st_table[state][col];
#ifdef DEBUG
    printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
    assert(next != IS);
#ifdef DEBUG
    if(next == IS)
    {
        printf("Scanner Error: Illegal state:\n");
        printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
        exit(1);
    }
#endif
    *accept = as_table[next];
    return next;
}

/*
 * Purpose:				Returns the appropriate column number from the transition table
 * 						based on the input character
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	isalpha, isdigit
 * Parameters:			c
 *							type: char
 * Returns:				The column number in the transition table
 */
int char_class(char c)
{
    /* Check if c is a letter in the alphabet */
    if (isalpha(c))
        return 0;

    /* Check if c is a number */
    if (isdigit(c))
    {
        /* Get the integer value that the char represents */
        int d = c - '0';
        return (d == 0) ? 1 : (d < 8) ? 2 : 3;
    }

    /* Check the remaining possibilities */
    return (c == '.') ? 4 : (c == '#') ? 5 : 6;
}

/*
 * Purpose:				Tokenizes the lexeme as an AVID or KW
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	iskeyword, strlen, strcpy, strncpy
 * Parameters:			lexeme
 *							type: char *
 * Returns:				The token
 */
Token aa_func02(char lexeme[])
{
    /* The return token */
    Token token;
    /* The keyword table index */
    int keywordIndex;
    /* The return vid_offset from st_install */
    int vid_offset;
    /* A pointer to a trimmed lexeme if the one passed in is too long */
    char *trimmedLexeme = NULL;

    /* Checks if the lexeme is a keyword or not */
    if (iskeyword(lexeme, &keywordIndex))
    {
        token.code = KW_T;
        token.attribute.kwt_idx = keywordIndex;
        return token;
    }

    /* Sets the token as an AVID */
    token.code = AVID_T;

    /* If the length of the variable name out of range, trim it without modifying the original */
    if (strlen(lexeme) > VID_LEN)
    {
        /* Allocate memory for the trimmed lexeme */
        trimmedLexeme = (char *) malloc(sizeof(char) * (size_t) VID_LEN + 1);
        /* If memory could no be allocated, return runtime error */
        if (!trimmedLexeme)
            return runtime_error(ERR_MEM_ALLOC);

        /* Copy the characters in the AVID up until VID_LEN number of characters */
        strncpy(trimmedLexeme, lexeme, VID_LEN);
        /* Add a null-terminator to the end of trimmed lexeme to make it a valid C string */
        trimmedLexeme[VID_LEN] = '\0';
        /* Reassign the lexeme pointer to point to the trimmed lexeme. */
        lexeme = trimmedLexeme;
    }

    /* Attempt to install the vid */
    vid_offset = st_install(sym_table, lexeme, line);

    /* Free the trimmed lexeme if it was used */
    if (trimmedLexeme)
        free(trimmedLexeme);

    /* If the symbol table is full, print error message, store the table, and exit  */
    if (vid_offset == FAIL)
    {
        printf("\nError: The Symbol Table is full - install failed.\n");
        st_store(sym_table);
        exit(EXIT_FAILURE);
    }

    /* Set the vid_offset for the token */
    token.attribute.vid_offset = vid_offset;

    return token;
}

/*
 * Purpose:				Tokenizes the lexeme as a SVID
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	strlen, strcpy
 * Parameters:			lexeme
 *							type: char *
 * Returns:				The token
 */
Token aa_func03(char lexeme[])
{
    /* The return token */
    Token t;
    /* The return vid_offset from st_install */
    int vid_offset;
    /* A pointer to a trimmed lexeme if the one passed in is too long */
    char *trimmedLexeme = NULL;

    /* Set the token as an SVID */
    t.code = SVID_T;

    /* If the length of the variable name out of range, trim it without modifying the original */
    if (strlen(lexeme) > (VID_LEN - 1))
    {
        /* Allocate memory for the trimmed lexeme */
        trimmedLexeme = (char *) malloc(sizeof(char) * (size_t) VID_LEN + 1);
        /* If memory could no be allocated, return runtime error */
        if (!trimmedLexeme)
            return runtime_error(ERR_MEM_ALLOC);

        /* Copy the characters in the SVID up until VID_LEN - 1 number of characters */
        strncpy(trimmedLexeme, lexeme, (VID_LEN - 1));
        /* Append the '#' character to the second last character of the modified SVID */
        trimmedLexeme[VID_LEN - 1] = '#';
        /* Add a null-terminator to the end of the trimmed lexeme to make it a valid C string */
        trimmedLexeme[VID_LEN] = '\0';
        /* Reassign the lexeme pointer to point to the trimmed lexeme. */
        lexeme = trimmedLexeme;
    }

    /* Attempt to install the vid */
    vid_offset = st_install(sym_table, lexeme, line);

    /* Free the trimmed lexeme if it was used */
    if (trimmedLexeme)
        free(trimmedLexeme);

    /* If the symbol table is full, print error message, store the table, and exit  */
    if (vid_offset == FAIL)
    {
        printf("\nError: The Symbol Table is full - install failed.\n");
        st_store(sym_table);
        exit(EXIT_FAILURE);
    }

    /* Set the vid_offset for the token */
    t.attribute.vid_offset = vid_offset;

    return t;
}

/*
 * Purpose:				Tokenizes the lexeme as a FPL
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	strtod, aa_table[ES]
 * Parameters:			lexeme
 *							type: char *
 * Returns:				The token
 */
Token aa_func08(char lexeme[])
{
    /* The return token */
    Token t;
    /* Used to store the float value of a valid lexeme */
    double value;

    /* Attempt to convert the string to numeric*/
    value = strtod(lexeme, NULL);
    /*
     * If the returned value is out of range,
     * return error token.
     */
    if (value > FLT_MAX || value < 0.0f || (value < FLT_MIN && value != 0.0f))
        return aa_table[ES](lexeme);

    t.code = FPL_T;
    t.attribute.flt_value = (float) value;
    return t;
}

/*
 * Purpose:				Tokenizes the lexeme as an IL
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	strlen, strtol, aa_table[ES]
 * Parameters:			lexeme
 *							type: char *
 * Returns:				The token
 */
Token aa_func05(char lexeme[])
{
    /* The return token */
    Token t;
    /* Holds the number of chars that make up the length of the lexeme */
    int lexemeLength = (int) strlen(lexeme);
    /* Holds the result of the conversion  */
    long value;

    /* If the lexeme is out of range for integer, return error */
    if (lexemeLength > INL_LEN)
        return aa_table[ES](lexeme);

    /* Reset errno to clear any previous errors */
    errno = 0;
    /* Attempt to convert the string to numeric*/
    value = strtol(lexeme, NULL, 10);
    /*
     * If strtol encountered errors or the returned value is out of range,
     * return error token.
     */
    if (errno == ERANGE || value > PLATY_INT_MAX || value < 0)
        return aa_table[ES](lexeme);

    t.code = INL_T;
    t.attribute.int_value = (int) value;
    return t;
}

/*
 * Purpose:				Tokenizes the lexeme as an OIL
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	strlen, strtol, aa_table[ES]
 * Parameters:			lexeme
 *							type: char *
 * Returns:				The token
 */
Token aa_func11(char lexeme[])
{
    Token t;
    /* Holds the number of chars that make up the length of the lexeme */
    int lexemeLength = (int) strlen(lexeme);
    /* Holds the result of the conversion  */
    long value;

    /* If the lexeme is out of range for octal literal, return error */
    if (lexemeLength > (INL_LEN + 1)) /* Plus one for leading zero */
        return aa_table[ES](lexeme);

    /* Reset errno to clear any previous errors */
    errno = 0;
    /* Attempt to convert the string to numeric*/
    value = strtol(lexeme, NULL, 8);
    /*
     * If strtol encountered errors or the returned value is out of range,
     * return error token.
     */
    if (errno == ERANGE || value > PLATY_INT_MAX || value < 0)
        return aa_table[ES](lexeme);

    t.code = INL_T;
    t.attribute.int_value = (int) value;
    return t;
}

/*
 * Purpose:				Tokenizes the lexeme as an error token
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	strncpy
 * Parameters:			lexeme
 *							type: char *
 * Returns:				The token
 */
Token aa_func12(char lexeme[])
{
    /* The token to be returned */
    Token t;
    /* Set the code to be error */
    t.code = ERR_T;
    /* Copy the invalid lexeme into the error token */
    strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
    /* Add a null-terminator at the end for errors that take up the whole space */
    t.attribute.err_lex[ERR_LEN] = '\0';
    return t;
}

/*
 * Purpose:				Attempts to match the lexeme to the values in the keyword table.
 * Author:				Warsame Egal
 * Version:				1.0 (2013-11-08)
 * Called Functions:	strlen, strtol, aa_table[ES]
 * Parameters:			kw_lexeme
 *							type: char *
 *						index
 *							type: int *
 * Returns:				TRUE, and sets index to the index of the keyword, if it is a keyword
 * 						FALSE if it is not a keyword
 */
int iskeyword(char * kw_lexeme, int *index)
{
    if (kw_lexeme)
    {
        int i;
        for (i = 0; i < KWT_SIZE; ++i)
        {
            if (!strcmp(kw_lexeme, kw_table[i]))
            {
                *index = i;
                return TRUE;
            }
        }
    }
    return FALSE;
}
