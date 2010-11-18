/*----------------------------------------------------------------
  File: rpn.c
  Name: Steven Tomcavage
  Date Last Modified: December 17, 2006
  Description: Calculates result of RPN (Reverse Polish Notation)
               expressions given in an input file. Works with 
	       expressions that contain operators mixed in with
	       numbers as well as expressions that extend over
	       multiple lines.
  Return Values: 0 - Expression calculated successfully
                -1 - Filename was not specified on command line
                 1 - IO error opening or closing file 
                 2 - Invalid char in expression 
                 3 - Error converting string in expression to number 
                 4 - Not enough operators in expression 
                 5 - Not enough operands in expression 
 ----------------------------------------------------------------*/
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<errno.h>
#include "stack.h"

#define MAXDIGITS 20

/***** Function Prototypes *****/
int countNumsInExpression(FILE *);
stackDataT evaluateExpression(FILE *, Stack *);
void doSingleCalculation(Stack *, int);
int charIsANumber(int);
int charIsWhitespace(int);
int charIsOperator(int);
void fatalIOError(int);

/***** Global Enumerations *****/
enum ascii_nums { ZERO_CHAR = '0', NINE_CHAR = '9'};

enum whitespace { NEWLINE = '\n', SPACE = ' ', TAB = '\t' };

enum operands { 
    PLUS = '+', MINUS = '-', MULTIPLY = '*', DIVIDE = '/', MOD = '%', 
    AND = '&', OR = '|', XOR = '^', NOT = '~' 
};

/***************************************************************************/
/* Function: main(int, char**)                                             */
/***************************************************************************/
int main(int argc, char **argv){
    char *infile_name;
    FILE *infile;
    int num_count_in_line;
    stackDataT answer;
    Stack S;

    // Error out if the user didn't provide a filename to use
    if (argc != 2) {
        fprintf(stderr, "Usage: %s filename\n", *argv);
        return -1;
    }

    // Grab the input filename from the program arguments
    infile_name = *(argv + 1);

    // Try and open the input file for reading
    if ((infile = fopen(infile_name, "r")) == NULL) {
        fatalIOError(errno);
    }

    // Go through input file and count how many numbers are in the expression
    num_count_in_line = countNumsInExpression(infile);

    // Rewind the file back to the beginning. Since rewind doesn't return any 
    // value, clear the global variable errno before calling rewind. If errno 
    // has a value after rewind, then an error occurred.
    errno = 0;
    rewind(infile);

    if (! errno) {
        // Create the stack used for evaluating the expression
        init(&S, num_count_in_line);
        // Get the answer
        answer = evaluateExpression(infile, &S);
        // Delete the stack in order to free the memory
        delete(&S);
    }
    else {
        fatalIOError(errno);
    }

    printf("The expression evaluated to %d\n", answer);

    // Close the input file
    if (fclose(infile) != 0) {
        fatalIOError(errno);
    }

    return 0;
}

/***************************************************************************/
/*    Function: countNumsInExpression(FILE *)                              */
/* Description: Counts how many numbers are in the expression in the input */
/*              file.                                                      */
/*     Returns: Count of numbers in expression                             */
/***************************************************************************/
int countNumsInExpression(FILE *infile) {
    int next_char = 0;
    int num_count_in_line = 0;
    int prev_char_is_numeric = 0;

    // Go through each character in the file
    while ((next_char = fgetc(infile)) != EOF) {

        // Test if the character is a number
        if (charIsANumber(next_char)) {

            // If this is the first digit in a number, flag that a number has 
        // been found and increase the count of numbers found
            if (prev_char_is_numeric == 0) {
                num_count_in_line++;
                prev_char_is_numeric = 1;
            }
        }

        // Test if the next char is whitespace between chars
        else if (charIsWhitespace(next_char)) {

            // Reset flag if characters have switched from #s to whitespace
            if (prev_char_is_numeric == 1) {
                prev_char_is_numeric = 0;
            }
        }
    }

    return num_count_in_line;
}

/***************************************************************************/
/*    Function: evaluateExpression(FILE *, Stack *)                        */
/* Description: Goes through the expression in the input file and          */
/*              calculates the answer. The process used is to read each    */
/*              character, gathering numbers on a stack, until an operator */
/*              is found. Then numbers are popped off the stack and used   */
/*              by the operators. The results of the operations are popped */
/*              back onto the stack. If another number is found, operations*/
/*              stop and the process goes back to gathering numbers. This  */
/*              repeats until EOF.                                         */
/*     Returns: The final answer of the entire expression                  */
/***************************************************************************/
stackDataT evaluateExpression(FILE *infile, Stack *S) {
    int next_char, read_ahead_char;
    int string_position = 0;
    char temp_string[MAXDIGITS];
    stackDataT data;

    while ((next_char = fgetc(infile)) != EOF) {

        // If the character is a minus, it might be an operator or it might 
        // be a negative sign
        if (next_char == MINUS) {
            // Get the next char from the stream for testing
            if ((read_ahead_char = fgetc(infile)) != EOF) {
                // Push the next char back onto the stream so it is correctly
                // processed on the next loop iteration
                if (ungetc(read_ahead_char, infile) != read_ahead_char) {
                    fprintf(stderr, "Error: Unable to parse the expression.\n");
                }
                // If the next char in the stream is a number, this minus is a
                // negative sign, add it to the number string and end this loop
                // iteration
                if (charIsANumber(read_ahead_char)) {
                    temp_string[string_position] = next_char;
                    string_position++;
                    continue;
                }
            }
        }

        // If the character is a number, add it to the temp string
        if (charIsANumber(next_char)) {
            temp_string[string_position] = next_char;
            string_position++;
        }

        // Test if the next char is whitespace between chars
        else if (charIsWhitespace(next_char)) {

            // Test if we have a number string to convert
            if (string_position > 0) {
                // Terminate the string and convert it to data
                temp_string[string_position] = '\0';
                data = atoi(temp_string);
                if (errno) {
                    // Handle number convert error
                    fprintf(
                        stderr,
                        "Error: unable to convert %s to an integer\n",
                        temp_string
                    );
                    exit(3);
                }
                // Push the data onto the stack
                push(S, data);
                // Finally, clear counter to reset temp string
                string_position = 0;
            }
        }

        // Test if the next char is an operator
        else if (charIsOperator(next_char)) {
            doSingleCalculation(S, next_char);
        }
        
        // Handle unexptected characters here
        else {
            fprintf(
                stderr, 
                "Error: the character %c is not valid.\n", 
                next_char
            );
            exit(2);
        }
    }

    // Pop the final value off the stack - that's our answer
    data = pop(S);
    
    // If there is anything left on the stack, that's an error - handle here
    if (!isEmpty(S)) {
        fprintf(
            stderr, 
            "Error: Not enough operators for the number of operands\n"
        );
        exit(4);
    }  

    return data;
}

/***************************************************************************/
/*    Function: doSingleCalculation(int, Stack *)                          */
/* Description: Pops two values off the stack (or one if the operator is   */
/*              unary), does the requested operation, and pushes the       */
/*              result back onto the stack.                                */
/*     Returns: nothing                                                    */
/***************************************************************************/
void doSingleCalculation(Stack *S, int operator) {
    stackDataT leftOperand, rightOperand;
    stackDataT result;

    // Pop operands off the stack - note that the right operand
    // is at the top of the stack, so it is retrieved first
    if (!isEmpty(S)) {
        rightOperand = pop(S);
    }
    // Error if the stack is empty
    else {
        fprintf(
            stderr, 
            "Error: not enough operands for '%c' operator\n", 
            operator
        );
        exit(5);
    }

    // Only get the left operand if the operator is a binary operation
    if (operator != NOT) {
        if (!isEmpty(S)) {
            leftOperand = pop(S);
        }
        // Error if the stack is empty
        else {
            fprintf(
                stderr, 
                "Error: not enough operands for '%c' operator\n", 
                operator
            );
            exit(5);
        }
    }

    switch (operator) {
        case PLUS:
            result = leftOperand + rightOperand;
            break;
        case MINUS:
            result = leftOperand - rightOperand;
            break;
        case MULTIPLY:
            result = leftOperand * rightOperand;
            break;
        case DIVIDE:
            result = leftOperand / rightOperand;
            break;
        case MOD:
            result = leftOperand % rightOperand;
            break;
        case AND:
            result = leftOperand & rightOperand;
            break;
        case OR:
            result = leftOperand | rightOperand;
            break;
        case XOR:
            result = leftOperand ^ rightOperand;
            break;
        case NOT:
            result = ~rightOperand;
            break;
    }
    
    // Print a summary of this operation for the user
    if (operator != NOT) {
        printf("%d %c %d = %d\n", leftOperand, operator, rightOperand, result);
    }
    else {
        printf("%c %d = %d\n", operator, rightOperand, result);
    }
    
    // Push the result of this operation back onto the stack
    push(S, result);
}

/***************************************************************************/
/*    Function: charIsANumber(int)                                         */
/* Description: Tests whether the given char is a number                   */
/*     Returns: 1 if the char is a number, 0 otherwise                     */
/***************************************************************************/
int charIsANumber(int char_to_test) {
    int is_a_num = 0;
    if (char_to_test >= ZERO_CHAR && char_to_test <= NINE_CHAR) { 
        is_a_num = 1;
    }
    return is_a_num;
}   

/***************************************************************************/
/*    Function: charIsWhitespace(int)                                      */
/* Description: Tests whether the given char is whitespace (space, tab, or */
/*              newline).                                                  */
/*     Returns: 1 if the char is whitespace, 0 otherwise                   */
/***************************************************************************/
int charIsWhitespace(int char_to_test) {
    int is_whitespace = 0;
    if (
        char_to_test == SPACE  || 
        char_to_test == TAB    ||
        char_to_test == NEWLINE
    ) {
        is_whitespace = 1;
    }
    return is_whitespace;
}   

/***************************************************************************/
/*    Function: charIsOperator(int)                                        */
/* Description: Tests whether the given char is an operator (+, -, *, /,   */
/*              %, &, |, ^, ~).                                            */
/*     Returns: 1 if the char is an operator, 0 otherwise                  */
/***************************************************************************/
int charIsOperator(int char_to_test) {
    int is_operator = 0;
    if (
        char_to_test == PLUS     ||
        char_to_test == MINUS    ||
        char_to_test == MULTIPLY ||
        char_to_test == DIVIDE   ||
        char_to_test == MOD      ||
        char_to_test == AND      ||
        char_to_test == OR       ||
        char_to_test == XOR      ||
        char_to_test == NOT 
    ) {
        is_operator = 1;
    }
    return is_operator;
}

/***************************************************************************/
/*    Function: fatalIOError(int)                                          */
/* Description: Prints an error message to stderr and then exits program.  */
/*     Returns: nothing                                                    */
/***************************************************************************/
void fatalIOError(int error_num) {
    fprintf(stderr, "Error: %s\n", strerror(error_num));
    exit(1);
}
 
