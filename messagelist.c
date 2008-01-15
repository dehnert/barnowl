#define OWL_PERL
#include "owl.h"

static const char fileIdent[] = "$Id$";

owl_messagelist * owl_messagelist_new() {
  return (owl_messagelist*)owl_perl_new("BarnOwl::MessageList");
}

int owl_messagelist_get_size(owl_messagelist *ml)
{
  int size;
  OWL_PERL_CALL_METHOD(ml, "get_size",
                       , // No arguments
                       // Error message
                       "Error in get_size: %s",
                       // Errors are fatal
                       1,
                       // Success code
                       size = POPi;
                       );
  return size;
}

void owl_messagelist_start_iterate(owl_messagelist *ml) {
  OWL_PERL_CALL_METHOD(ml, "start_iterate",
                       , // No arguments
                       // Error
                       "Error in start_iterate: %s",
                       1, //Fatal errors
                       OWL_PERL_VOID_CALL
                       );
}

owl_message *owl_messagelist_iterate_next(owl_messagelist *ml) {
  SV *msg;
  OWL_PERL_CALL_METHOD(ml, "iterate_next",
                       , // No arguments
                       // Error
                       "Error in iterate_next: %s",
                       1, //Fatal errors
                       msg = POPs;
                       if(SvROK(msg)) SvREFCNT_inc(msg);
                       );
  return SvROK(msg) ? sv_2mortal(msg) : NULL;
}

owl_message *owl_messagelist_get_by_id(owl_messagelist *ml, int target_id)
{
  SV *msg;
  OWL_PERL_CALL_METHOD(ml, "get_by_id",
                       mXPUSHi(target_id); ,
                       // Error
                       "Error in get_by_id: %s",
                       1, //Fatal errors
                       msg = POPs;
                       if(SvROK(msg)) SvREFCNT_inc(msg);
                       );
  return SvROK(msg) ? sv_2mortal(msg) : NULL;
}

void owl_messagelist_append_element(owl_messagelist *ml, owl_message *msg)
{
  OWL_PERL_CALL_METHOD(ml, "add_message",
                       XPUSHs((SV*)msg); ,
                       // Error
                       "Error in add_message: %s",
                       1, // Fatal
                       OWL_PERL_VOID_CALL
                       );
  // When we insert the message, perl code takes ownership of it, so
  // we relinquish our reference
  sv_2mortal(msg);
}

void owl_messagelist_expunge(owl_messagelist *ml)
{
  OWL_PERL_CALL_METHOD(ml, "expunge",
                       , // No args
                       // Error
                       "Error in expunge: %s",
                       1, // Fatal
                       OWL_PERL_VOID_CALL
                       );
}

void owl_messagelist_invalidate_formats(owl_messagelist *ml)
{
  owl_message *m;

  owl_messagelist_iterate_next(ml);

  while((m = owl_messagelist_iterate_next(ml)) != NULL) {
    owl_message_invalidate_format(m);
  }
}
