#include <stdlib.h>
#include "owl.h"

static const char fileIdent[] = "$Id$";

void owl_view_create(owl_view *v, char *name, owl_filter *f, owl_style *s)
{
  v->name=owl_strdup(name);
  v->filter=f;
  v->style=s;
  owl_list_create(&(v->messages));
  owl_view_recalculate(v);
}

char *owl_view_get_name(owl_view *v)
{
  return(v->name);
}

/* if the message matches the filter then add to view */
void owl_view_consider_message(owl_view *v, owl_message *m)
{
  if (owl_filter_message_match(v->filter, m)) {
    owl_list_append_element(&(v->messages),
                            (void*)owl_message_get_id(m));
  }
}

/* remove all messages, add all the global messages that match the
 * filter.
 */
void owl_view_recalculate(owl_view *v)
{
  owl_messagelist *gml;
  owl_list *ml;
  owl_message *m;

  gml=owl_global_get_msglist(&g);
  ml=&(v->messages);

  /* nuke the old list */
  owl_list_free_simple(ml);
  owl_list_create(ml);

  /* find all the messages we want */
  owl_messagelist_start_iterate(gml);
  while((m = owl_messagelist_iterate_next(gml)) != NULL) {
    if (owl_filter_message_match(v->filter, m)) {
      owl_list_append_element(ml, (void*)owl_message_get_id(m));
    }
  }
}

void owl_view_new_filter(owl_view *v, owl_filter *f)
{
  v->filter=f;
  owl_view_recalculate(v);
}

void owl_view_set_style(owl_view *v, owl_style *s)
{
  v->style=s;
}

owl_style *owl_view_get_style(owl_view *v)
{
  return(v->style);
}

char *owl_view_get_style_name(owl_view *v) {
  return(owl_style_get_name(v->style));
}

owl_message *owl_view_get_element(owl_view *v, int index)
{
  if(index < 0 || index >= owl_view_get_size(v))
    return NULL;
  int id = (int)owl_list_get_element(&(v->messages), index);
  return owl_messagelist_get_by_id(owl_global_get_msglist(&g), id);
}

void owl_view_delete_element(owl_view *v, int index)
{
  owl_message_mark_delete(owl_view_get_element(v, index));
}

void owl_view_undelete_element(owl_view *v, int index)
{
  owl_message_unmark_delete(owl_view_get_element(v, index));
}

int owl_view_get_size(owl_view *v)
{
  return(owl_list_get_size(&(v->messages)));
}

/* Returns the position in the view with a message closest 
 * to the passed msgid. */
int owl_view_get_nearest_to_msgid(owl_view *v, int targetid)
{
  int first, last, mid = 0, max, bestdist, curid = 0;

  first = 0;
  last = max = owl_view_get_size(v) - 1;
  while (first <= last) {
    mid = (first + last) / 2;
    curid = (int)owl_list_get_element(&(v->messages), mid);
    if (curid == targetid) {
      return(mid);
    } else if (curid < targetid) {
      first = mid + 1;
    } else {
      last = mid - 1;
    }
  }
  bestdist = abs(targetid-curid);
  if (curid < targetid && mid+1 < max) {
    curid = (int)owl_list_get_element(&(v->messages), mid+1);
    mid = (bestdist < abs(targetid-curid)) ? mid : mid+1;
  }
  else if (curid > targetid && mid-1 >= 0) {
    curid = (int)owl_list_get_element(&(v->messages), mid-1);
    mid = (bestdist < abs(targetid-curid)) ? mid : mid-1;
  }
  return mid;
}

int owl_view_get_nearest_to_saved(owl_view *v)
{
  int cachedid;

  cachedid=owl_filter_get_cachedmsgid(v->filter);
  if (cachedid<0) return(0);
  return (owl_view_get_nearest_to_msgid(v, cachedid));
}

/* saves the current message position in the filter so it can 
 * be restored later if we switch back to this filter. */
void owl_view_save_curmsgid(owl_view *v, int curid)
{
  owl_filter_set_cachedmsgid(v->filter, curid);
}

/* fmtext should already be initialized */
void owl_view_to_fmtext(owl_view *v, owl_fmtext *fm)
{
  owl_fmtext_append_normal(fm, "Name: ");
  owl_fmtext_append_normal(fm, v->name);
  owl_fmtext_append_normal(fm, "\n");

  owl_fmtext_append_normal(fm, "Filter: ");
  owl_fmtext_append_normal(fm, owl_filter_get_name(v->filter));
  owl_fmtext_append_normal(fm, "\n");

  owl_fmtext_append_normal(fm, "Style: ");
  owl_fmtext_append_normal(fm, owl_style_get_name(v->style));
  owl_fmtext_append_normal(fm, "\n");
}

char *owl_view_get_filtname(owl_view *v)
{
  return(owl_filter_get_name(v->filter));
}

void owl_view_free(owl_view *v)
{
  owl_list_free_simple(&(v->messages));
  if (v->name) owl_free(v->name);
}
