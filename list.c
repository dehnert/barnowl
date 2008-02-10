#include "owl.h"
#include <stdlib.h>

static const char fileIdent[] = "$Id$";

#define INITSIZE 10
#define GROWBY 1.5

int owl_list_create(owl_list *l)
{
  l->size=0;
  l->list=(void **)owl_malloc(INITSIZE*sizeof(void *));
  l->avail=INITSIZE;
  if (l->list==NULL) return(-1);
  return(0);
}

int owl_list_get_size(owl_list *l)
{
  return(l->size);
}


void owl_list_grow(owl_list *l, int n) /*noproto*/
{
  void *ptr;

  if ((l->size+n) > l->avail) {
    ptr=owl_realloc(l->list, l->avail*GROWBY*sizeof(void *));
    if (ptr==NULL) abort();
    l->list=ptr;
    l->avail=l->avail*GROWBY;
  }

}

void *owl_list_get_element(owl_list *l, int n)
{
  if (n>l->size-1) return(NULL);
  return(l->list[n]);
}

int owl_list_insert_element(owl_list *l, int at, void *element)
{
  int i;
  if(at < 0 || at > l->size) return -1;
  owl_list_grow(l, 1);

  for (i=l->size; i>at; i--) {
    l->list[i]=l->list[i-1];
  }

  l->list[at] = element;
  l->size++;
  return(0);
}

int owl_list_append_element(owl_list *l, void *element)
{
  return owl_list_insert_element(l, l->size, element);
}

int owl_list_prepend_element(owl_list *l, void *element)
{
  return owl_list_insert_element(l, 0, element);
}

int owl_list_remove_element(owl_list *l, int n)
{
  int i;

  if (n>l->size-1) return(-1);
  for (i=n; i<l->size-1; i++) {
    l->list[i]=l->list[i+1];
  }
  l->size--;
  return(0);
}

/* todo: might leak memory */
int owl_list_replace_element(owl_list *l, int n, void *element)
{
  if (n>l->size-1) return(-1);

  l->list[n]=element;
  return(0);
}

void owl_list_free_all(owl_list *l, void (*elefree)(void *))
{
  int i;

  for (i=0; i<l->size; i++) {
    (elefree)(l->list[i]);
  }
  owl_free(l->list);
}

void owl_list_free_simple(owl_list *l)
{
  if (l->list) owl_free(l->list);
}

/************* REGRESSION TESTS **************/
#ifdef OWL_INCLUDE_REG_TESTS

#include "test.h"

int owl_list_regtest(void) {
  int numfailed=0;
  owl_list l;
  int i;

  printf("# BEGIN testing owl_list\n");

  FAIL_UNLESS("create", 0 == owl_list_create(&l));
  for(i=0;i<10;i++) {
    FAIL_UNLESS("insert", 0 == owl_list_append_element(&l, (void*)i));
  }

  FAIL_UNLESS("size", 10 == owl_list_get_size(&l));

  for(i=0;i<10;i++) {
    FAIL_UNLESS("get", i == (int)owl_list_get_element(&l, i));
  }

  for(i=0;i<10;i++) {
    FAIL_UNLESS("append tail", 0 == owl_list_append_element(&l, (void*)(10+i)));
  }

  FAIL_UNLESS("size", 20 == owl_list_get_size(&l));

  for(i=10;i<20;i++) {
    FAIL_UNLESS("get", i == (int)owl_list_get_element(&l, i));
  }

  for(i=9;i>=0;i--) {
    FAIL_UNLESS("prepend", 0 == owl_list_prepend_element(&l, (void*)(-i)));
  }

  FAIL_UNLESS("size", 30 == owl_list_get_size(&l));

  for(i=0;i<10;i++) {
    FAIL_UNLESS("get", -i == (int)owl_list_get_element(&l, i));
  }
  
  printf("# END testing owl_list\n");

  return numfailed;
}

#endif /* OWL_INCLUDE_REG_TESTS */
