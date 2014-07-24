/* ODLL sample (c)2004 Nicolas Cannasse */
#include "sample.h"
#include <malloc.h>
#include <stdio.h>
#include <windows.h>

void main() {	
	char *s;
	array a,b;
	int i,j;

	printf("Running print-one: \n");
	print_one();

	j = fib(10);
	printf("fib(10) = %d \n",j);

	s = ftos(3.1416);
	printf("PI = %s\n", s);
	caml_free(s);

	printf("Testing acreate\n");
	a = acreate(5,'x');
	for(i=0;i<a.size;i++) {
		printf("a[%d] = '%s'\n",i,astring(a,i));
		caml_free(astring(a,i));
	}
	caml_free(a.data);

	printf("Allocating an array...\n");
	a.size = 5;
	a.data = (void *)malloc(sizeof(array)*a.size);
	for(i=0;i<a.size;i++) {
		array *a_i = &aarray(a,i);
		a_i->size = i+1;
		a_i->data = (void *)malloc(sizeof(double)*(i+1));
		for(j=0;j<=i;j++)
			afloat(*a_i,j) = ((double)(j+1)) / (i+1);
	}

	printf("Calling f2d...\n");
	b = f2d(a);

	printf("Printing result of f2d...\n");
	for(i=0;i<b.size;i++) {
		array b_i = aarray(b,i);
		printf("b[%d] = ",i);
		for(j=0;j<b_i.size;j++)
			printf("%f, ",afloat(b_i,j));
		printf("\n");
	}
	printf("\n");

	printf("Freeing arrays...\n");
	afree(&b,2);
	afree(&a,2);	
	
	a.size = 3;
	a.data = (void *)malloc(sizeof(char*)*a.size);
	((char**)a.data)[0] = "a";
	((char**)a.data)[1] = "test";
	((char**)a.data)[2] = "withstring";

	printf("Calling sdecompose...\n");
	b = sdecompose(a);
	
	for(i=0;i<a.size;i++) {
		array b_i = ((array *)b.data)[i];
		printf("b[%d] = ",i);
		for(j=0;j<b_i.size;j++)
			printf("%c.", ((char*)b_i.data)[j]);
		printf("\n");
	}
	printf("\n");

	printf("Freeing arrays...\n");
	afree(&b,2);

	printf("Testing GC 1/2...\n");
	for(i=0;i<1000000;i++)
		caml_free( ftos(1.1) );			

	printf("Testing GC 2/2...\n");
	for(i=0;i<1000000;i++) {
		b = sdecompose(a);		
		afree(&b,2);
	}
		
	afree(&a,1);
	printf("Test Done - press ENTER\n");	
	_getch();
}



