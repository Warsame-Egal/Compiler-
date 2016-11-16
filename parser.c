/*
* File Name:		parser.c
* Compiler:			Microsoft Visual Studio 2013 (Windows 8.1 x64)
* Author:			Warsame Egal
* Purpose:			Parses and outputs information about a PLATYPUS source file
* Function list:	parser, match, syn_eh, syn_printe, program, gen_incode, additive_arithmetic_expression, additive_arithmetic_expression_p,
*					arithmetic_expression, assignment_expression, assignment_statement, conditional_expression,
*					input_statement, iteration_statement, logical_and_expression, logical_and_expression_p,
*					logical_or_expression, logical_or_expression_p, multiplicative_arithmetic_expression, multiplicative_arithmetic_expression_p,
*					opt_statements, opt_variable_list, output_statement, primary_a_relational_expression,
*					primary_arithmetic_expression, primary_s_relational_expression, primary_string_expression, relational_expression,
*					relational_expression_p, relational_expression_p_str, selection_statement, statement,
*					statements, statements_p, string_expression, string_expression_p,
*					unary_arithmetic_expression, variable_identifier, variable_list, variable_list_p,
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland 5.02 projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include "parser.h" /* Contains the definitions and prototypes for the parser */

/*
* Purpose:				Launches the parsing operation
* Author:				Warsame Egal
* Version:				1.0 (2013-12-06)
* Called Functions:		mlwpar_next_token, program, match, gen_incode
* Parameters:			in_buf
*							type: Buffer *
*/
void parser(Buffer *in_buf)
{
    /* Copy the pointer location to the scanner Buffer that will be used */
    sc_buf = in_buf;
    /* Set lookahead_token to the first token */
    lookahead_token = mlwpar_next_token(sc_buf);
    /* Start the recursive parser! */
    program();
    /* Match the final ending of the PLATYPUS source file */
    match(SEOF_T, NO_ATTR);
    gen_incode("PLATY: Source file parsed");
}

/*
* Purpose:				Attempts to match the current token against the expected token
* Author:				Warsame Egal
* Version:				1.0 (2013-12-06)
* Called Functions:		mlwpar_next_token, syn_printe, syn_eh
* Parameters:			pr_token_code
*							type: int
*						pr_token_attribute
*							type: int
*/
void match(int pr_token_code, int pr_token_attribute)
{
    /* If the token codes match, verify the attributes */
    if (pr_token_code == lookahead_token.code)
    {
        switch (pr_token_code)
        {
        case ART_OP_T: /* Arithmetic operator */
        case REL_OP_T: /* Relational operator */
        case LOG_OP_T: /* Logical operator */
        case KW_T: /* Keyword token */
            /* If the attributes do not match, break to print error */
            if (pr_token_attribute != lookahead_token.attribute.get_int)
                break;
        default:
            /* If the next token is SEOF_T, return immediately */
            if (lookahead_token.code == SEOF_T)
                return;

            /* Get the next token */
            lookahead_token = mlwpar_next_token(sc_buf);
            /*
             * If the next token is an error token, print it and increment the
             * synerrno, finally getting the next token.
             */
            if (lookahead_token.code == ERR_T)
            {
                syn_printe();
                ++synerrno;
                /* Get the next token for next evaluation */
                lookahead_token = mlwpar_next_token(sc_buf);
            }

            return;
        }
    }

    /* Could not match token code and/or attribute, print error */
    syn_eh(pr_token_code);
}

/*
* Purpose:				The error handler for the parser
* Author:				Warsame Egal
* Version:				1.0 (2013-12-06)
* Called Functions:		syn_printe, mlwpar_next_token, exit
* Parameters:			sync_token_code
*							type: int
*/
void syn_eh(int sync_token_code)
{
    /* Print the error */
    syn_printe();
    /* Increment the global error counter */
    ++synerrno;

    /* While the token is not SEOF_T, get the next token(s) for validation */
    while (lookahead_token.code != SEOF_T)
    {
        /* Get the next token */
        lookahead_token = mlwpar_next_token(sc_buf);

        /* Check if this token code matches the expected code */
        if (lookahead_token.code == sync_token_code)
        {
            /* If the token is not SEOF_T, get the next token after it */
            if (lookahead_token.code != SEOF_T)
                lookahead_token = mlwpar_next_token(sc_buf);

            return; /* Match was found */
        }
    }

    /* If the sync_token_code is not SEOF_T, no match was found, therefore exit */
    if (sync_token_code != SEOF_T)
        exit(synerrno);
}

/*
* Purpose:				Prints error information
* Author:				Svillen Ranev
* Called Functions:		printf, b_get_chmemloc
*/
void syn_printe(void)
{
    Token t = lookahead_token;

    printf("PLATY: Syntax error:  Line:%3d\n", line);
    printf("*****  Token code:%3d Attribute: ", t.code);
    switch (t.code)
    {
    case  ERR_T: /* ERR_T     0   Error token */
        printf("%s\n", t.attribute.err_lex);
        break;
    case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
        printf("NA\n");
        break;
    case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
    case  SVID_T:/* SVID_T    3  String Variable identifier token */
        printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
        break;
    case  FPL_T: /* FPL_T     4  Floating point literal token */
        printf("%5.1f\n", t.attribute.flt_value);
        break;
    case INL_T: /* INL_T      5   Integer literal token */
        printf("%d\n", t.attribute.get_int);
        break;
    case STR_T:/* STR_T     6   String literal token */
        printf("%s\n", b_get_chmemloc(str_LTBL, (short) t.attribute.get_int));
        break;
    case SCC_OP_T: /* 7   String concatenation operator token */
        printf("NA\n");
        break;
    case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
        printf("NA\n");
        break;
    case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  LPR_T: /*LPR_T    12  Left parenthesis token */
        printf("NA\n");
        break;
    case  RPR_T: /*RPR_T    13  Right parenthesis token */
        printf("NA\n");
        break;
    case LBR_T: /*    14   Left brace token */
        printf("NA\n");
        break;
    case RBR_T: /*    15  Right brace token */
        printf("NA\n");
        break;
    case KW_T: /*     16   Keyword token */
        printf("%s\n", kw_table[t.attribute.get_int]);
        break;
    case COM_T: /* 17   Comma token */
        printf("NA\n");
        break;
    case EOS_T: /*    18  End of statement *(semi - colon) */
        printf("NA\n");
        break;
    default:
        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/* end switch */
}/* end syn_printe()*/

/*
* Purpose:				Prints a string to the console
* Author:				Warsame Egal
* Version:				1.0 (2013-12-06)
* Called Functions:		printf
* Parameters:			string
*							type: char *
*/
void gen_incode(char * string)
{
    printf("%s\n", string);
}

/*
	<additive arithmetic expression> ->
		<multiplicative arithmetic expression><additive arithmetic expression_p>

	FIRST Set = { AVID_T, FPL_T, INL_T, ( }
*/
void additive_arithmetic_expression(void)
{
    multiplicative_arithmetic_expression();
    additive_arithmetic_expression_p();
}

/*
	<additive arithmetic expression_p> ->
		+  <multiplicative arithmetic expression><additive arithmetic expression_p> | -  <multiplicative arithmetic expression><additive arithmetic expression_p> | E

	FIRST Set = { +, -, E }
*/
void additive_arithmetic_expression_p(void)
{
    if (lookahead_token.code == ART_OP_T
            && lookahead_token.attribute.arr_op != MULT
            && lookahead_token.attribute.arr_op != DIV) /* Must be + or - */
    {
        match(lookahead_token.code, lookahead_token.attribute.arr_op);
        multiplicative_arithmetic_expression();
        additive_arithmetic_expression_p();
        gen_incode("PLATY: Additive arithmetic expression parsed");
    }
}

/*
	<arithmetic expression> - >
		<unary arithmetic expression> | <additive arithmetic expression>

	FIRST Set = { -, +, AVID_T, FPL_T, INL_T, ( }
*/
void arithmetic_expression(void)
{
    switch (lookahead_token.code)
    {
    case ART_OP_T:
        switch (lookahead_token.attribute.arr_op)
        {
        case MULT:
        case DIV: /* If { *, / } there is an error */
            syn_printe();
            return;
        }
        /* Operator is +/- */
        unary_arithmetic_expression();
        break;
    case AVID_T:
    case FPL_T:
    case INL_T:
    case LPR_T:
        additive_arithmetic_expression();
        break;
    default:
        /* No suitable match was found, print error */
        syn_printe();
        return;
    }
    gen_incode("PLATY: Arithmetic expression parsed");
}

/*
	<assignment expression> ->
		AVID = <arithmetic expression> | SVID = <string expression>

	FIRST Set = { AVID_T, SVID_T }
*/
void assignment_expression(void)
{
    switch (lookahead_token.code)
    {
    case SVID_T:
        match(SVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR);
        string_expression();
        gen_incode("PLATY: Assignment expression (string) parsed");
        break;
    case AVID_T:
        match(AVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR);
        arithmetic_expression();
        gen_incode("PLATY: Assignment expression (arithmetic) parsed");
        break;
    default: /* Anything other than SVID or AVID is an error */
        syn_printe();
    }
}

/*
	<assignment statement> ->
		<assignment expression>;

	FIRST Set = { AVID_T, SVID_T }
*/
void assignment_statement(void)
{
    assignment_expression();
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Assignment statement parsed");
}

/*
	<conditional expression> ->
		<logical OR expression>

	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void conditional_expression(void)
{
    logical_or_expression();
    gen_incode("PLATY: Conditional expression parsed");
}

/*
	<input statement> ->
		INPUT (<variable list>);

	FIRST Set = { INPUT }
*/
void input_statement(void)
{
    match(KW_T, INPUT);
    match(LPR_T, NO_ATTR);
    variable_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: INPUT statement parsed");
}

/*
	<iteration statement> ->
		USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
		REPEAT {
			<opt_statements>
		};

	FIRST Set = { USING }
*/
void iteration_statement(void)
{
    match(KW_T, USING);
    match(LPR_T, NO_ATTR);
    assignment_expression();
    match(COM_T, NO_ATTR);
    conditional_expression();
    match(COM_T, NO_ATTR);
    assignment_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: USING statement parsed");
}

/*
	<logical AND expression> ->
		<relational expression><logical AND expression_p>

	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void logical_and_expression(void)
{
    relational_expression();
    logical_and_expression_p();
}

/*
	<logical AND expression_p> ->
		.AND.  <relational expression><logical AND expression_p> | E

	FIRST Set = { .AND., E }
*/
void logical_and_expression_p(void)
{
    if (lookahead_token.code == LOG_OP_T && lookahead_token.attribute.log_op == AND) /* Needs to be a true AND token */
    {
        match(LOG_OP_T, AND);
        relational_expression();
        logical_and_expression_p();
        gen_incode("PLATY: Logical AND expression parsed");
    }
}

/*
	<logical  OR expression> ->
		<logical AND expression><logical OR expression_p>

	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
*/
void logical_or_expression(void)
{
    logical_and_expression();
    logical_or_expression_p();
}

/*
	<logical  OR expression_p>
		.OR.  <logical AND expression><logical  OR expression_p> | E

	FIRST Set = { .OR.,  E }
*/
void logical_or_expression_p(void)
{
    if (lookahead_token.code == LOG_OP_T && lookahead_token.attribute.log_op == OR) /* Needs to be a true OR token */
    {
        match(LOG_OP_T, OR);
        logical_and_expression();
        logical_or_expression_p();
        gen_incode("PLATY: Logical OR expression parsed");
    }
}

/*
	<multiplicative arithmetic expression> ->
		<primary arithmetic expression><multiplicative arithmetic expression_p>

	FIRST Set = { AVID_T, FPL_T, INL_T, ( }
*/
void multiplicative_arithmetic_expression(void)
{
    primary_arithmetic_expression();
    multiplicative_arithmetic_expression_p();
}

/*
	<multiplicative arithmetic expression_p> ->
		* <primary arithmetic expression><multiplicative arithmetic expression_p> | / <primary arithmetic expression><multiplicative arithmetic expression_p> | E

	FIRST Set = { *, / , E }
*/
void multiplicative_arithmetic_expression_p(void)
{
    if (lookahead_token.code == ART_OP_T
            && lookahead_token.attribute.arr_op != PLUS
            && lookahead_token.attribute.arr_op != MINUS) /* Must be * or / */
    {
        match(lookahead_token.code, lookahead_token.attribute.arr_op);
        primary_arithmetic_expression();
        multiplicative_arithmetic_expression_p();
        gen_incode("PLATY: Multiplicative arithmetic expression parsed");
    }
}

/*
	<opt_statements> ->
		<statements> | E

	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, E }
*/
void opt_statements(void)
{
    switch (lookahead_token.code)
    {
    case KW_T:
        switch (lookahead_token.attribute.kwt_idx)
        {
        case PLATYPUS:
        case ELSE:
        case THEN:
        case REPEAT:
            gen_incode("PLATY: Opt_statements parsed");
            return;
        } /* Everything else falls through to statements() */
    case AVID_T:
    case SVID_T:
        statements();
        break;
    default:
        gen_incode("PLATY: Opt_statements parsed");
    }
}

/*
	<opt_variable list> ->
		<variable list> | E

	FIRST Set = { AVID_T, SVID_T, E }
*/
void opt_variable_list(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T:
    case SVID_T:
        variable_list();
        break;
    case STR_T:
        match(STR_T, NO_ATTR);
        gen_incode("PLATY: Output list (string literal) parsed");
        break;
    default: /* Empty string encountered */
        gen_incode("PLATY: Output list (empty) parsed");
    }
}

/*
	<output statement> ->
		OUTPUT (<opt_variable list>); | OUTPUT (STR_T);

	FIRST(<output statement>) = { OUTPUT( }
*/
void output_statement(void)
{
    match(KW_T, OUTPUT);
    match(LPR_T, NO_ATTR);
    opt_variable_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: OUTPUT statement parsed");
}

/*
	<primary a_relational expression> ->
		AVID_T | FPL_T | INL_T

	FIRST Set = { AVID_T, FPL_T, INL_T }
*/
void primary_a_relational_expression(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T:
    case FPL_T:
    case INL_T:
        match(lookahead_token.code, lookahead_token.attribute.rel_op);
        break;
    default: /* Anything else is an error */
        syn_printe();
    }
    gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
	<primary arithmetic expression> ->
		AVID_T | FPL_T | INL_T | (<arithmetic expression>)

	FIRST Set = { AVID_T, FPL_T, INL_T, ( }
*/
void primary_arithmetic_expression(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T:
    case FPL_T:
    case INL_T:
        match(lookahead_token.code, lookahead_token.attribute.arr_op);
        break;
    case LPR_T:
        match(lookahead_token.code, lookahead_token.attribute.arr_op);
        arithmetic_expression();
        match(RPR_T, NO_ATTR);
        break;
    default: /* Anything else is an error */
        syn_printe();
        return;
    }

    gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
	<primary s_relational expression> ->
		<primary string expression>

	FIRST Set = { SVID_T, STR_T }
*/
void primary_s_relational_expression(void)
{
    switch (lookahead_token.code)
    {
    case SVID_T:
    case STR_T:
        primary_string_expression();
        break;
    default: /* Anything else is an error */
        syn_printe();
    }
    gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
	<primary string expression> ->
		SVID_T | STR_T

	FIRST Set = { SVID_T, STR_T }
*/
void primary_string_expression(void)
{
    switch (lookahead_token.code)
    {
    case SVID_T:
    case STR_T:
        match(lookahead_token.code, NO_ATTR);
        break;
    default: /* Anything else is an error */
        syn_printe();
    }
    gen_incode("PLATY: Primary string expression parsed");
}

/*
	<program> ->
		PLATYPUS {<opt_statements>}

	FIRST Set = { PLATYPUS }
*/
void program(void)
{
    match(KW_T, PLATYPUS);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    gen_incode("PLATY: Program parsed");
}

/*
	<relational expression> ->
		<primary a_relational expression><relational expression_p> | <primary s_relational expression><relational expression_p_str>

	FIRST Set = { AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
*/
void relational_expression(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T:
    case FPL_T:
    case INL_T:
        primary_a_relational_expression();
        relational_expression_p();
        break;
    case SVID_T:
    case STR_T:
        primary_s_relational_expression();
        relational_expression_p_str();
        break;
    default: /* Anything else is an error */
        syn_printe();
    }
    gen_incode("PLATY: Relational expression parsed");
}

/*
	<relational expression_p> ->
		==  <primary a_relational expression> | !=  <primary a_relational expression> | > <primary a_relational expression> | < <primary a_relational expression>

	FIRST Set = { ==, !=, >, < }
*/
void relational_expression_p(void)
{
    if (lookahead_token.code == REL_OP_T)
    {
        switch (lookahead_token.attribute.rel_op)
        {
        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead_token.code, lookahead_token.attribute.arr_op);
            primary_a_relational_expression();
            return;
        }
    }
    /* Anything else is an error */
    syn_printe();
}

/*
	<relational expression_str> ->
		== <primary s_relational expression> | != <primary s_relational expression> | > <primary s_relational expression> | < <primary s_relational expression>

	FIRST Set = { ==, !=, >, < }
*/
void relational_expression_p_str(void)
{
    if (lookahead_token.code == REL_OP_T)
    {
        switch (lookahead_token.attribute.rel_op)
        {
        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead_token.code, lookahead_token.attribute.arr_op);
            primary_s_relational_expression();
            return;
        }
    }
    /* Anything else is an error */
    syn_printe();
}

/*
	<selection statement> ->
		IF (<conditional expression>)
		THEN <opt_statements>
		ELSE { <opt_statements> } ;

	FIRST Set = { IF }
*/
void selection_statement(void)
{
    match(KW_T, IF);
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    opt_statements();
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: IF statement parsed");
}

/*
	<statement> ->
		<assignment statement> | <selection statement> | <iteration statement> | <input statement> | <output statement>

	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
*/
void statement(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T:
    case SVID_T:
        assignment_statement();
        break;
    case KW_T:
        switch (lookahead_token.attribute.kwt_idx)
        {
        case IF:
            selection_statement();
            break;
        case USING:
            iteration_statement();
            break;
        case INPUT:
            input_statement();
            break;
        case OUTPUT:
            output_statement();
            break;
        default: /* No suitable match could be found, print error */
            syn_printe();
        }
        break;
    default: /* No suitable match could be found, print error */
        syn_printe();
    }
}

/*
	<statements> ->
		<statement><statements_p>

	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
*/
void statements(void)
{
    statement();
    statements_p();
}

/*
	<statements_p> ->
		<statement><statements_p> | E

	FIRST Set = { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, E }
*/
void statements_p(void)
{
    switch (lookahead_token.code)
    {
    case KW_T:
        switch (lookahead_token.attribute.kwt_idx)
        {
        case PLATYPUS:
        case ELSE:
        case THEN:
        case REPEAT:
            return;
        } /* Everything else falls through */
    case AVID_T:
    case SVID_T:
        statement();
        statements_p();
        break;
    }
}

/*
	<string expression> ->
		<primary string expression><string expression_p>

	FIRST Set = { SVID_T, STR_T }
*/
void string_expression(void)
{
    primary_string_expression();
    string_expression_p();
    gen_incode("PLATY: String expression parsed");
}

/*
	<string expression_p> ->
		<>  <primary string expression><string expression_p> | E

	FIRST Set = { <>, E }
*/
void string_expression_p(void)
{
    if (lookahead_token.code == SCC_OP_T)
    {
        match(SCC_OP_T, NO_ATTR);
        primary_string_expression();
        string_expression_p();
    }
}

/*
	<unary arithmetic expression> ->
		-  <primary arithmetic expression> | + <primary arithmetic expression>

	FIRST Set = { -, + }
*/
void unary_arithmetic_expression(void)
{
    switch (lookahead_token.code)
    {
    case ART_OP_T:
        switch (lookahead_token.attribute.arr_op)
        {
        case MULT:
        case DIV: /* If { *, / } there is an error */
            syn_printe();
            return;
        }

        /* Operator is +/- */
        match(lookahead_token.code, lookahead_token.attribute.arr_op);
        primary_arithmetic_expression();
        gen_incode("PLATY: Unary arithmetic expression parsed");
        break;
    default:
        /* No suitable match was found, print error */
        syn_printe();
        return;
    }
}

/*
	<variable identifier> ->
		AVID_T | SVID_T

	No FIRST Set
*/
void variable_identifier(void)
{
    switch (lookahead_token.code)
    {
    case AVID_T:
    case SVID_T:
        match(lookahead_token.code, NO_ATTR);
        break;
    default: /* Anything else is an error */
        syn_printe();
    }
}

/*
	<variable list> ->
		<variable identifier><variable list_p>

	FIRST Set = { AVID_T, SVID_T }
*/
void variable_list(void)
{
    variable_identifier();
    variable_list_p();
    gen_incode("PLATY: Variable list parsed");
}

/*
	<variable list_p> ->
		, <variable identifier><variable list_p> | E

	FIRST Set = {, , E }
*/
void variable_list_p(void)
{
    if (lookahead_token.code != COM_T)
        return;

    match(COM_T, NO_ATTR);
    variable_identifier();
    variable_list_p();
}
