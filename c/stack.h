/*----------------------------------------------------------------
  File: stack.h
  Name: Steven Tomcavage
  Date Last Modified: December 17, 2006
 ----------------------------------------------------------------*/

//Give the stack element a generic name
//This allows us to change the stack data type and resuse our data structure
typedef int stackDataT;  


struct stack_t{
  stackDataT * data; //pointer of type stackDataT
  int top;           //stack pointer
  int maxSize;      //total size of the stack
};

typedef struct stack_t Stack; //from now on we can use Stack as a type

//Function prototypes 
void init(Stack *S, int size);
void delete(Stack *S);
int isEmpty(Stack *S);
int isFull(Stack *S);
void push(Stack *S, stackDataT d);
stackDataT pop(Stack *S);
void printStack(Stack *S);

