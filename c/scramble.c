/*----------------------------------------------------------------
  File: scramble.c
  Name: Steven Tomcavage
  Date Last Modified: December 17, 2006
  Description: Prompts the user to input a string and then walks the
               user through the process of scrambling that string.
 ----------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <errno.h>
#include <limits.h>

#define WARN_BAD_INPUT "Positions must be two integers between 1 and %d!\n\n" 
#define ASCII_ZERO 0x30
#define ASCII_NINE 0x39

/***** Constants *****/ 
// Maximum length of the string to be scrambled (not including nul char '\0').
#define MAX_STR_LENGTH  80

/***** Function Prototypes *****/ 
void InitializeMap(char *map[MAX_STR_LENGTH],
                   char str[MAX_STR_LENGTH+1],
                   int length);

void ScrambleMap(char *map[MAX_STR_LENGTH], int length);

void PrintScrambled(char *map[MAX_STR_LENGTH], int length);

int StringToInt(char *string);


/***************************************************************************/
/*    Function: main()                                                     */
/***************************************************************************/
int main() {
    char str[MAX_STR_LENGTH + 1]; //will hold original string
    int length;                   // Actual length of string in 'str'.
    char *map[MAX_STR_LENGTH];    //map

    printf("Enter a string to scramble (enter no spaces and no more than 80 characters)\n");
   
    // Characters are scanned in into "str" array and null terminated
    // hence str can be used as string now
    if (scanf("%s",str) != 1) {
        fprintf(stderr, "Error: Unable to read the string entered.\n");
        exit(-1);
    }

    // Clear out the input buffer in case anything is lurking in there
    setbuf(stdin, NULL);

    //get the actual length using strlen() in string.h
    length = strlen(str);

    InitializeMap(map, str, length);

    ScrambleMap(map, length);

    printf("\nThe string: \"%s\" was scrambled to: \"", str);
    PrintScrambled(map, length);
    printf("\"\n\n");
  
    return 0;
}

/***************************************************************************/
/*    Function: InitializeMap(char*[], char[], int)                        */
/* Description: Sets up the initial _unscrambled_ map  i.e., each position */
/*              in the 'map' array is made to point to each corresponding  */
/*              character in the string before any scrambling is done.     */ 
/*     Returns: nothing                                                    */
/***************************************************************************/
void InitializeMap(char *map[MAX_STR_LENGTH],
                   char str[MAX_STR_LENGTH+1],
                   int length) {
    int i;
    
    // Point each position in map to char in pre-scrambled string
    for (i = 0; i < length; i++) {
        map[i] = &str[i];
    }
}

/***************************************************************************/
/*    Function: ScrambleMap(char*[], int)                                  */
/* Description: Prompts user for pairs of numbers that represent positions */ 
/*              to swap as part of the scrambling process. Positions are   */
/*              numbered  starting with 1 (so the code converts these to   */
/*              zero-based array indices). Entering "0 0" signals the end  */
/*              of swapping.                                               */
/*     Returns: nothing                                                    */
/***************************************************************************/
void ScrambleMap(char *map[MAX_STR_LENGTH], int length) {
    char *temp;
    char input1[MAX_STR_LENGTH], input2[MAX_STR_LENGTH];
    int swap1, swap2;
    int exitCode = 0;

    // Print the scrambling instructions
    printf("\n");   
    printf("To scramble, enter a number pair (e.g., 1 3). ");
    printf("Positions are numbered between 1 and %d for this string. ", length);
    printf("Enter \"0 0\" when you are done.\n\n");

    // Swap characters until the user decides to end the program
    while (exitCode != 1) {
        printf("Swap? ");

        // Get the swap positions from the user as two strings. 
        if ((scanf("%s %s", input1, input2)) != 2) {
            // Warn if we didn't get two inputs
            printf(WARN_BAD_INPUT, length);
            setbuf(stdin, NULL);
            continue;
        }

    // Warn if the strings from the user weren't valid integers
    else if ((swap1 = StringToInt(input1)) < 0 || 
             (swap2 = StringToInt(input2)) < 0
    ) {
        printf(WARN_BAD_INPUT, length);
        continue;
    }

        // Check if the user wants to exit the program (by inputing 0 0)
        if (swap1 == 0 && swap2 == 0) {
            exitCode = 1;
        }

        // Warn if one or both of input was outside the valid bounds
        else if (
            (swap1 < 1 || swap1 > length) || 
            (swap2 < 1 || swap2 > length)
        ) {
            printf(WARN_BAD_INPUT, length);
        }   

        // Validation is complete - go ahead and modify the map
        else {
            // Adjust swap1 and swap2 to be zero-based 
            swap1--;
            swap2--;

            // Swap two pointers in the map, which effectively
            // swaps the two chars when the map is used to print 
            temp = map[swap1];
            map[swap1] = map[swap2];
            map[swap2] = temp;

            // Finally, show the user their newly scrambled string
            printf("New scrambled version: ");
            PrintScrambled(map, length);
            printf("\n\n");
        }
    }
}

/***************************************************************************/
/*    Function: PrintScrambled(char*[], int)                               */
/* Description: Prints the string out to standard output in the order      */
/*              specified by the scramble map.                             */
/*     Returns: nothing                                                    */
/***************************************************************************/
void PrintScrambled(char *map[MAX_STR_LENGTH], int length) {
    int i;
    
    // Go through the map and print each character that is pointed to
    for (i = 0; i < length; i++) {
        printf("%c", *map[i]);
    }
} 

/***************************************************************************/
/*    Function: StringToInt(char*)                                         */
/* Description: Tests each character in a string to verify that it's a     */
/*              number. If the string is all numbers, it is converted to   */
/*              an integer and returned.                                   */ 
/*     Returns: Integer representation of input string, or -1 if input     */
/*              string could not be converted to a number                  */
/***************************************************************************/
int StringToInt(char *string) {
    int is_integer, i, character, return_value;
    long long int converted_value;

    // Start with the assumption that this string is a valid integer
    is_integer = 1;

    // Go through each character in the string and verify that the 
    // character is a number. Don't use pointer arithmetic here because
    // the starting address of the string is required later.
    for (i = 0; string[i] != '\0'; i++) {

        // First convert the character to an int
        character = (int) string[i];

        // String is not an integer if character is not between 0 and 
        // 9, inclusive.
        if (character < ASCII_ZERO || character > ASCII_NINE) {
            is_integer = 0;
            break;
        }
    }

    // If string is a valid integer, convert it to an actual integer
    if (is_integer) {
        // First see if string can be converted to long long int. The
        // conversion function will set errno if the conversion
        // can't be made, so clear errno before converting.
        errno = 0;
        converted_value = strtoll(string, (char **)NULL, 10);

        // If the conversion was successful and the value can fit in an
        // integer, cast the value to an int
        if ((errno == 0) && (converted_value <= INT_MAX)) {
            return_value = (int)converted_value;
        }

        // If conversion was unsuccessful or the value can't fit in an
        // integer, set a flag to indicate that
        else {
            return_value = -1;
        }
    }

    // If string is not a valid integer, set a flag to indicate that
    else {
        return_value = -1;
    }

    return return_value;
}

