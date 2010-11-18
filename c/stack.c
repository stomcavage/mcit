/*----------------------------------------------------------------
  File: stack.c
  Name: Steven Tomcavage
  Date Last Modified: December 17, 2006
  Description: Stack definition and access methods.
 ----------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include "stack.h"

/***************************************************************************/
/*    function: init(Stack *, int)                                         */
/* description: Creates the data array with number of elements = size      */
/*     returns: no value, but if memory allocation fails, program exits    */
/***************************************************************************/
void init(Stack * S, int size) {
    if ((S->data = (stackDataT *)malloc(size * sizeof(S->data))) != NULL) {
        S->top = -1;
        S->maxSize = size;
    }
    else {
        exit(1);
    }
}

/***************************************************************************/
/*    function: delete(Stack *)                                            */
/* description: frees all memory associated with the stack                 */
/*     returns: no value                                                   */
/***************************************************************************/
void delete(Stack *S) {
    free(S->data);  
}

/***************************************************************************/
/*    function: isEmpty(Stack *)                                           */
/* description: tests whether the stack is empty or not                    */
/*     returns: 1 if the stack is empty, 0 otherwise                       */
/***************************************************************************/
int isEmpty(Stack *S) {
    int stackIsEmpty = 0;
    if (S->top == -1) {
        stackIsEmpty = 1;
    }
    return stackIsEmpty;
}

/***************************************************************************/
/*    function: isFull(Stack *)                                            */
/* description: tests whether the stack is full or not                     */
/*     returns: 1 if the stack is full, 0 otherwise                        */
/***************************************************************************/
int isFull(Stack * S) {
    int stackIsFull = 0;
    if (S->top == (S->maxSize - 1)) {
        stackIsFull = 1;
    }
    return stackIsFull;
}

/***************************************************************************/
/*    function: push()                                                     */
/* description: Add an element d to stack and updates the stack pointer    */ 
/*     returns: nothing, but exits if the push would cause an overflow     */ 
/***************************************************************************/
void push(Stack *S, stackDataT d) {
    if (isFull(S)) {
        fprintf(stderr, "Error: stack overflow\n");
        exit(1);
    }
    else {
        // Adjust the top of the stack to hold the item being pushed
        S->top++;   
        // Store the item at the address of the top of the stack
        *(S->data + S->top) = d;
    }
}


/***************************************************************************/
/*    function: pop()                                                      */
/* description: Removes an element from top of stack and updates the stack */
/*              pointer                                                    */ 
/*     returns: element that was poped from the stack, or exits if the pop */
/*              would cause an underflow                                   */
/***************************************************************************/
stackDataT pop(Stack *S) {
    stackDataT elementPopped;
    if (isEmpty(S)) {
        fprintf(stderr, "Error: stack underflow\n");
        exit(1);
    }
    else {
        // Pop the element stored at the address of the top of the stack
        elementPopped = *(S->data + S->top);    
        // Readjust the top of the stack to reflect the pop
        S->top--;
    }
    return elementPopped;
}

/***************************************************************************/
/*    function: printStack()                                               */
/* description:                                                            */
/*     returns:                                                            */
/***************************************************************************/
void printStack(Stack *S) {
    int i;
    printf("Stack top is %d\n", S->top);
    printf("Stack maxSize is %d\n", S->maxSize);
    for (i = 0; i <= S->top; i++) {
        printf("Stack position %d has value %d\n", i, *(S->data + i));
    }
    printf("\n");
}
