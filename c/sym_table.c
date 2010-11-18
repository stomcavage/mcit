/*        File: sym_table.c
 *      Author: Steven Tomcavage
 *        Date: April, 2008
 * Description: Contains functions to populate and search a binary tree
 *              which contains symbols and their addresses
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sym_table.h"

typedef struct sym_node_struct sym_node_t;

struct sym_node_struct {
  char* symbol;
  int address;
  sym_node_t* left;
  sym_node_t* right;
};

sym_node_t* g_symboltable_ptr = NULL; 

void sym_table_insert(char* sym, int addr)
{
	sym_node_t* new_node_ptr = NULL;
	sym_node_t* curr_node_ptr = NULL;
	char* dup_sym = NULL;
	int sort_result;
	
	/* Create a new node */
	if ((new_node_ptr = (sym_node_t*)malloc(sizeof(sym_node_t))) == NULL) {
		fprintf(stderr, "ERROR: unable to allocate memory for node\n");
		exit(1);
	}
	
	/* Duplicate the symbol so we don't create a dangling pointer */
	if ((dup_sym = strdup(sym)) == NULL) {
		fprintf(stderr, "ERROR: unable to copy the symbol\n");
		exit(1);
	}
	
	/* Populate the new node */
	new_node_ptr->symbol = dup_sym;
	new_node_ptr->address = addr;
	new_node_ptr->left = NULL;
	new_node_ptr->right = NULL;

	/* If we have a tree, walk it to find where to insert the new node */
	if (g_symboltable_ptr != NULL) {
		curr_node_ptr = g_symboltable_ptr;
		for (;;) {
			sort_result = strcmp(curr_node_ptr->symbol, new_node_ptr->symbol);
			/* If current node symbol sorts after new node's symbol, go left */
			if (sort_result > 0) {
				if (curr_node_ptr->left != NULL) {
					curr_node_ptr = curr_node_ptr->left;
				}
				/* If there isn't a left node, insert new node */
				else {
					curr_node_ptr->left = new_node_ptr;
					break;
				}
			}
			/* If current node symbol sorts before new node's symbol, go right */
			else if (sort_result < 0) {
				if (curr_node_ptr->right != NULL) {
					curr_node_ptr = curr_node_ptr->right;
				}
				/* If there isn't a right node, insert new node */
				else {
					curr_node_ptr->right = new_node_ptr;
					break;
				}
			}
			/* If current node symbol equals new node's symbol, that's an error */
			else {
				fprintf(stderr, "ERROR: duplicate symbol (%s) being added to symbol table\n", new_node_ptr->symbol);
				exit(1);
			}
		}
	}
	/* If we don't have a tree, use the new node as the root */
	else {
		g_symboltable_ptr = new_node_ptr;
	}
}

int sym_table_lookup(char* sym)
{
	int address, sort_result; 
	short int found_node = 0;
	sym_node_t* curr_node_ptr = NULL;
	
	/* Point to the root of the symbol table */
	curr_node_ptr = g_symboltable_ptr;

	/* Walk the tree to find the symbol we're seeking */
	while (! found_node) {
		sort_result = strcmp(curr_node_ptr->symbol, sym);		
		/* If current node symbol sorts after the node we're seeking, go left */
		if (sort_result > 0) {
			if (curr_node_ptr->left != NULL) {
				curr_node_ptr = curr_node_ptr->left;
			}
			/* If there isn't a left node, symbol isn't in the tree */
			else {
				fprintf(stderr, "ERROR: symbol(%s) not found\n", sym);
				exit(1);
			}
		}
		/* If current node symbol sorts before the node we're seeking, go right */
		else if (sort_result < 0) {
			if (curr_node_ptr->right != NULL) {
				curr_node_ptr = curr_node_ptr->right;
			}
			/* If there isn't a right node, symbol isn't in the tree */
			else {
				fprintf(stderr, "ERROR: symbol(%s) not found\n", sym);
				exit(1);
			}
		}
		/* If current node symbol equals new node's symbol, we've found the symbol */
		else {
			address = curr_node_ptr->address;
			found_node = 1;
		}
	}
	
	return address;
}
