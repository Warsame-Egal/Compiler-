/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland 5.02 projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include "buffer.h"

/*
* Purpose:				Allocates and returns memory for a buffer with a given
*						initial capacity, increment factor, and operation mode.
* Author:				Warsame Egal
* Called Functions:	calloc(), malloc(), free()
* Parameters:			init_capacity
*							type: short
*							valid range: 1 - BUFFER_MAX_SIZE
*						inc_factor
*							type: char
*							valid range: 1 - 255 (Additive mode)
*							valid range: 1 - 100 (Multiplicative mode)
*						o_mode
*							type: char
*							valid range: a, m, f
* Returns:				Pointer to a Buffer structure or NULL on fail
* Algorithm:			Validate input parameters, allocate memory for struct
*						and char array, return pointer to struct or NULL on fail
*/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode)
{
	/* Declare local variables */
	Buffer *buffer; /* Pointer to a Buffer struct */

	/* Run input parameter tests based on expected values (see function header). */
	if ((init_capacity < 1) || (inc_factor < 1)
		|| (o_mode != 'f' && o_mode != 'a' && o_mode != 'm')
		|| (o_mode == 'm' && inc_factor > 100))
		return NULL;

	/* Attempt to allocate memory for the buffer struct and assign it to the pointer */
	buffer = (Buffer *)calloc(1, sizeof(Buffer));
	/* If no memory was available for allocation, exit immediately */
	if (buffer == NULL)
		return NULL;

	/* Attempt to allocate memory for the character buffer and assign it to ca_head */
	buffer->ca_head = (char *)malloc((size_t)init_capacity * sizeof(char));
	if (buffer->ca_head == NULL)
	{
		/* No memory was available for allocation, free the buffer struct memory and exit immediately */
		free(buffer);
		return NULL;
	}

	/* Set the mode and increment factors depending on the mode */
	switch (o_mode)
	{
	case 'f': /* Set the buffer up for 'fixed' mode */
		buffer->mode = BUFFER_MODE_FIXED_SIZE;
		buffer->inc_factor = 0;
		break;
	case 'a': /* Set the buffer up for 'additive' mode */
		buffer->mode = BUFFER_MODE_ADDITIVE;
		buffer->inc_factor = inc_factor;
		break;
	case 'm': /* Set the buffer up for 'multiplicative' mode */
		buffer->mode = BUFFER_MODE_MULTIPLICATIVE;
		buffer->inc_factor = inc_factor;
		break;
	}

	/* Set the buffer's capacity to the initial capacity */
	buffer->capacity = init_capacity;
	return buffer;
}

/*
* Purpose:				Adds a given symbol to a given buffer.
* Author:				Warsame Egal
* Called Functions:	b_isfull(), realloc()
* Parameters:			pBD
*							type: Buffer *
*						symbol
*							type: char
*							valid range: anything
* Returns:				Pointer to a Buffer structure or NULL on fail
* Algorithm:			Validate input parameters, resize buffer if necessary,
*						append symbol to buffer.
*/
Buffer * b_addc(Buffer * const pBD, char symbol)
{
	if (
		/* Test that the buffer exists and is in a valid state and fail if not */
		(pBD == NULL || pBD->ca_head == NULL) ||
		/* Test that the addc_offset is valid */
		(pBD->addc_offset < 0 || pBD->addc_offset > pBD->capacity)
		||
		/* Test that the buffer mode is valid */
		(pBD->mode != BUFFER_MODE_FIXED_SIZE
		&& pBD->mode != BUFFER_MODE_ADDITIVE
		&& pBD->mode != BUFFER_MODE_MULTIPLICATIVE) ||
		/* Test that the inc_factor is valid if BUFFER_MODE_FIXED_SIZE  */
		(pBD->mode == BUFFER_MODE_FIXED_SIZE && pBD->inc_factor != 0)
		||
		/* Test that the inc_factor is valid if BUFFER_MODE_ADDITIVE  */
		(pBD->mode == BUFFER_MODE_ADDITIVE
		&& (pBD->inc_factor < 1 || pBD->inc_factor > 255))
		||
		/* Test that the inc_factor is valid if BUFFER_MODE_MULTIPLICATIVE  */
		(pBD->mode == BUFFER_MODE_MULTIPLICATIVE
		&& (pBD->inc_factor < 1 || pBD->inc_factor > 100)))
		return NULL;

	/* Reset r_flag */
	pBD->r_flag = UNSET_R_FLAG;

	/* Check to see if buffer is full before appending symbol */
	if (b_isfull(pBD))
	{
		/* Declare temporary variables */
		char *newBuffer; /* A pointer to the beginning of a new or reallocated buffer that holds the data */
		int newCapacity; /* A temporary variable to hold the result of new capacity calculations */
		int availableSpace = (BUFFER_MAX_SIZE - pBD->capacity); /* A temporary variable to hold the result of the calculated available space */

		/* If the buffer has no space left to grow, fail immediately */
		if (availableSpace == 0)
			return NULL;

		/* Attempt to grow the buffer depending on the mode */
		switch (pBD->mode)
		{
		case BUFFER_MODE_FIXED_SIZE:
			return NULL; /* A fixed-size buffer's capacity cannot be increased. */
		case BUFFER_MODE_ADDITIVE:
			/* Calculate the new capacity based on the increment factor converted to bytes and added to the old capacity */
			newCapacity = (pBD->capacity + (pBD->inc_factor * sizeof(char)));
			/* If the new capacity exceeds the maximum allowed size, fail immediately */
			if (newCapacity > BUFFER_MAX_SIZE)
				return NULL;
			break;
		case BUFFER_MODE_MULTIPLICATIVE:
			/* Calculate the new capacity based on the multiplicative formula */
			newCapacity = ((((float)availableSpace * pBD->inc_factor) / 100.0f));
			/*
			* If the capacity == 0, it means that the current capacity could not be incremented by the calculations,
			* and is thus assigned a new capacity of BUFFER_MAX_SIZE. Otherwise, the new capacity is the result of
			* adding the calculated size plus the old capacity.
			*/
			newCapacity = (newCapacity == 0) ?
			BUFFER_MAX_SIZE :
							newCapacity + pBD->capacity;
			break;
		}

		/* Attempt to reallocate enough memory for the buffer given a new size */
		newBuffer = (char *)realloc(pBD->ca_head, newCapacity);
		/* If not enough contiguous memory was available, fail immediately */
		if (newBuffer == NULL)
			return NULL;

		/* If the newly reallocated buffer is pointing to a new location in memory, set the r_flag */
		if (newBuffer != pBD->ca_head)
			pBD->r_flag = SET_R_FLAG;

		pBD->ca_head = newBuffer; /* Update the buffer to point to the correct memory location of the data */
		pBD->capacity = newCapacity; /* Update the buffer's capacity */
	}

	/* Dereference the pointer to the struct, copy the symbol into the buffer using the addc_offset, then increment addc_offset */
	(*pBD).ca_head[pBD->addc_offset++] = symbol;
	return pBD;
}

/*
* Purpose:				Clears the buffer so that it appears to be empty.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				SUCCESS or FAIL
* Algorithm:			Validate buffer, reset appropriate data members.
*/
int b_reset(Buffer * const pBD)
{
	/* Test that the buffer exists and fail if not */
	if (pBD == NULL)
		return FAIL;

	/* Reset buffer offsets */
	pBD->addc_offset = pBD->getc_offset = pBD->mark_offset = 0;
	return SUCCESS;
}

/*
* Purpose:				Frees a buffer and its contents from memory.
* Author:				Warsame Egal
* Called Functions:	free()
* Parameters:			pBD
*							type: Buffer *
* Returns:				void
*/
void b_destroy(Buffer * const pBD)
{
	/* If the buffer does not exist, exit immediately */
	if (pBD == NULL)
		return;
	/* Free the buffer char array if it exists */
	if (pBD->ca_head != NULL)
		free(pBD->ca_head);
	/* Free the buffer struct */
	free(pBD);
}

/*
* Purpose:				Checks to see if the buffer is full or not.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				TRUE, FALSE, or FAIL
*/
int b_isfull(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL :
		(pBD->addc_offset == pBD->capacity) ? TRUE : FALSE;
}

/*
* Purpose:				Retrieves the current used size of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's addc_offset, or FAIL
*/
short b_getsize(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->addc_offset;
}

/*
* Purpose:				Retrieves the capacity of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's capacity, or FAIL
*/
short b_getcapacity(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->capacity;
}

/*
* Purpose:				Sets the mark_offset of the buffer to a given value
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
*						mark
*							type: short
*							valid range: 0 - current capacity of the buffer
* Returns:				The value of the buffer's mark_offset, or FAIL
* Algorithm:			Validate buffer and mark and assign mark to buffer.
*/
int b_setmark(Buffer * const pBD, short mark)
{
	if (
		/* Test that the buffer exists and is in a valid state and fail if not */
		(pBD == NULL || pBD->ca_head == NULL)
		|| (pBD->capacity < 1 || pBD->capacity > BUFFER_MAX_SIZE) ||
		/* Test that the mark is within the valid range */
		(mark < 0 || mark > pBD->capacity))
		return FAIL;

	/* Reassign the buffer's mark to the new one */
	pBD->mark_offset = mark;
	return mark;
}

/*
* Purpose:				Retrieves the mark_offset of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's mark_offset, or FAIL
*/
short b_getmark(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->mark_offset;
}

/*
* Purpose:				Retrieves the mode of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's mode, or FAIL
*/
int b_getmode(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->mode;
}

/*
* Purpose:				Reads the contents of a file into the buffer.
* Author:				Warsame Egal
* Called Functions:	fgetc(), b_addc()
* Parameters:			fi
*							type: FILE *
*						pBD
*							type: Buffer *
* Returns:				The number of characters in the buffer, or FAIL
* Algorithm:			Validate the file and buffer, read each character from the file into
*						the buffer until the entire file has been read or the buffer becomes
*						full. Returns FAIL or LOAD_FAIL on error.
*/
int b_load(FILE * const fi, Buffer * const pBD)
{
	/* Declare a variable to store the number of characters */
	int numberOfCharactersInBuffer = 0;
	/* Declare a variable to store the current character */
	char currentCharacter;

	/* Validate that both the file and buffer are valid and operational */
	if (fi == NULL || pBD == NULL || pBD->ca_head == NULL)
		return FAIL;

	/* Get the first character */
	currentCharacter = (char)fgetc(fi);
	/* Loop through the file until the end of line is reached or buffer fills up */
	while (feof(fi) == 0)
	{
		/* Add the character, return LOAD_FAIL if character cannot be added */
		if (b_addc(pBD, currentCharacter) == NULL)
			return LOAD_FAIL;
		/* Increment the character counter */
		numberOfCharactersInBuffer++;
		/* Get the next character */
		currentCharacter = (char)fgetc(fi);
	}
	return numberOfCharactersInBuffer;
}

/*
* Purpose:				Checks to see if the buffer is empty or not.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				TRUE, FALSE, or FAIL
*/
int b_isempty(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : (pBD->addc_offset == 0) ? TRUE : FALSE;
}

/*
* Purpose:				Retrieves the eob of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's eob, or FAIL
*/
int b_eob(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->eob;
}

/*
* Purpose:				Retrieves the next character in the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's next character, GETC_INVALID_BUFFER, or GETC_EOB
*/
char b_getc(Buffer * const pBD)
{
	/* If the buffer is NULL, return appropriate error */
	if (pBD == NULL)
	{
		return GETC_INVALID_BUFFER;
	}
	else if (pBD->getc_offset == pBD->addc_offset)
	{
		pBD->eob = TRUE; /* End of buffer has been reached */
		return GETC_EOB;
	}
	else
	{
		/* Get the character from the buffer and increments getc_offset */
		char character = pBD->ca_head[pBD->getc_offset++];
		pBD->eob = FALSE;
		return character;
	}
}

/*
* Purpose:				Prints the contents of the buffer.
* Author:				Warsame Egal
* Called Functions:	b_isempty(), puts(), b_set_getc_offset(), b_getc(), b_eob(), putchar()
* Parameters:			pBD
*							type: Buffer *
* Returns:				The number of characters printed, or FAIL
* Algorithm:			Validate the buffer, print each character from the buffer.
*						Returns FAIL on error.
*/
int b_print(Buffer * const pBD)
{
	/* Declare a variable for the current character to be printed */
	char currentCharacter;
	/* Declare a counter for how many characters were printed */
	int totalCharsPrinted = 0;

	/* Test that the buffer exists and is in a valid state and fail if not */
	if ((pBD == NULL || pBD->ca_head == NULL))
		return FAIL;

	/* If the buffer is empty, print a message and return. */
	if (b_isempty(pBD))
	{
		puts("The buffer is empty."); /* Outputs message to the screen */
		return totalCharsPrinted;
	}

	/* Reset the getc_offset */
	b_set_getc_offset(pBD, 0);
	/* Attempt to get the first character */
	currentCharacter = b_getc(pBD);
	/* Keep getting and putting chars until EOB is true */
	while (b_eob(pBD) == FALSE)
	{
		putchar(currentCharacter); /* Outputs character to the screen */
		totalCharsPrinted++; /* increment the counter */
		currentCharacter = b_getc(pBD); /* Get the next character */
	}
	putchar('\n'); /* Output a newline */
	return totalCharsPrinted;
}

/*
* Purpose:				Packs the contents of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				A pointer to the buffer or NULL on fail.
* Algorithm:			Validate the buffer, attempt to shrink its capacity leaving room for one more character.
*/
Buffer *b_pack(Buffer * const pBD)
{
	char *newBuffer; /* Declare a variable to store a pointer for the reallocated buffer */
	int newCapacity; /* Declare a buffer to store the new capacity */

	if (
		/* Test that the buffer exists and is in a valid state and fail if not */
		(pBD == NULL || pBD->ca_head == NULL)
		|| (pBD->capacity < 1 || pBD->capacity > BUFFER_MAX_SIZE))
		return NULL;

	/* Calculate the new capacity */
	newCapacity = ((pBD->addc_offset + 1) * sizeof(char));

	/* If new capacity is at maximum, it cannot be packed any further. Return the buffer immediately. */
	if (newCapacity == BUFFER_MAX_SIZE)
		return pBD;

	/* Attempt to reallocate enough room for the buffer */
	newBuffer = (char *)realloc(pBD->ca_head, newCapacity);
	/* If not enough contiguous memory was available, fail immediately */
	if (newBuffer == NULL)
		return NULL;

	/* If the newly reallocated buffer is pointing to a new location in memory, set the r_flag */
	if (newBuffer != pBD->ca_head)
		pBD->r_flag = SET_R_FLAG;

	pBD->ca_head = newBuffer; /* Update the buffer to point to the correct memory location of the data */
	pBD->capacity = newCapacity; /* Update the buffer's capacity */
	return pBD;
}

/*
* Purpose:				Retrieves the r_flag of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's r_flag, or FAIL
*/
char b_get_r_flag(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->r_flag;
}

/*
* Purpose:				Moves the getc_offset back by 1.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				SUCCESS or FAIL
*/
int b_retract(Buffer * const pBD)
{
	/* If the buffer is not valid or operational, return FAIL */
	if (pBD == NULL)
		return FAIL;

	/* If the getc_offset cannot be decremented, return FAIL */
	if (pBD->getc_offset - 1 < 0)
		return FAIL;

	/* Decrement the getc_offset */
	pBD->getc_offset--;
	return SUCCESS;
}

/*
* Purpose:				Retrieves the getc_offset of the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
* Returns:				The value of the buffer's getc_offset, or FAIL
*/
short b_get_getc_offset(Buffer * const pBD)
{
	return (pBD == NULL) ? FAIL : pBD->getc_offset;
}

/*
* Purpose:				Sets the getc_offset of the buffer to a given value.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
*						offset
*							type: short
*							valid range: 0 - current capacity of the buffer
* Returns:				The value of the buffer's mark_offset, or FAIL
* Algorithm:			Validate buffer and mark and assign mark to buffer.
*/
int b_set_getc_offset(Buffer * const pBD, short offset)
{
	if (
		/* Test that the buffer exists and is in a valid state and fail if not */
		(pBD == NULL || pBD->ca_head == NULL)
		|| (pBD->capacity < 1 || pBD->capacity > BUFFER_MAX_SIZE) ||
		/* Test that the offset is within the valid range */
		(offset < 0 || offset > pBD->capacity))
		return FAIL;

	/* Assign the new offset */
	pBD->getc_offset = offset;
	return SUCCESS;
}

/*
* Purpose:				Retrieves a pointer to the character specified by the offset from the buffer.
* Author:				Warsame Egal
* Called Functions:	none
* Parameters:			pBD
*							type: Buffer *
*						offset
*							type: short
*							valid range:
* Returns:				A pointer to the character located in the buffer at the offset, or NULL on error.
*/
char * b_get_chmemloc(Buffer * const pBD, short offset)
{
	if (
		/* Test that the buffer exists and is in a valid state and fail if not */
		(pBD == NULL || pBD->ca_head == NULL)
		|| (pBD->capacity < 1 || pBD->capacity > BUFFER_MAX_SIZE) ||
		/* If the offset is not within the range of the buffer, fail */
		(offset < 0 || offset >(pBD->capacity / sizeof(char))))
		return NULL;

	/* Dereference the pointer to the struct and return the address of the char specified by the offset */
	return &(*pBD).ca_head[offset];
}
