#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#define OWL_PERL
#include "owl.h"

static const char fileIdent[] = "$Id$";

extern char *owl_perlwrap_codebuff;

extern XS(boot_BarnOwl);
extern XS(boot_DynaLoader);
/* extern XS(boot_DBI); */

static void owl_perl_xs_init(pTHX)
{
  char *file = __FILE__;
  dXSUB_SYS;
  {
    newXS("BarnOwl::bootstrap", boot_BarnOwl, file);
    newXS("DynaLoader::boot_DynaLoader", boot_DynaLoader, file);
  }
}

SV *owl_perlconfig_message2hashref(owl_message *m)
{
  return (SV*)m;
}

SV *owl_perlconfig_curmessage2hashref(void) /*noproto*/
{
  owl_message *m = owl_global_get_current_message(&g);
  if(m == NULL) {
    return &PL_sv_undef;
  }
  return owl_perlconfig_message2hashref(m);
}

owl_message * owl_perlconfig_hashref2message(SV *msg)
{
  return (owl_message*)SvREFCNT_inc(msg);
}

/* Calls in a scalar context, passing it a hash reference.
   If return value is non-null, caller must free. */
char *owl_perlconfig_call_with_message(char *subname, owl_message *m)
{
  dSP ;
  int count;
  unsigned int len;
  SV *msgref, *srv;
  char *out, *preout;
  
  ENTER ;
  SAVETMPS;
  
  PUSHMARK(SP) ;
  msgref = owl_perlconfig_message2hashref(m);
  XPUSHs(msgref);
  PUTBACK ;
  
  count = call_pv(subname, G_SCALAR|G_EVAL);
  
  SPAGAIN ;

  if (SvTRUE(ERRSV)) {
    STRLEN n_a;
    owl_function_error("Perl Error: '%s'", SvPV(ERRSV, n_a));
    /* and clear the error */
    sv_setsv (ERRSV, &PL_sv_undef);
  }

  if (count != 1) {
    fprintf(stderr, "bad perl!  no biscuit!  returned wrong count!\n");
    abort();
  }

  srv = POPs;

  if (srv) {
    preout=SvPV(srv, len);
    out = owl_malloc(strlen(preout)+1);
    strncpy(out, preout, len);
    out[len] = '\0';
  } else {
    out = NULL;
  }
  
  PUTBACK ;
  FREETMPS ;
  LEAVE ;

  return out;
}


/* Calls a method on a perl object representing a message.
   If the return value is non-null, the caller must free it.
 */
char * owl_perlconfig_message_call_method(owl_message *m, char *method, int argc, char ** argv)
{
  dSP;
  unsigned int count, len, i;
  SV *msgref, *srv;
  char *out, *preout;

  msgref = owl_perlconfig_message2hashref(m);

  ENTER;
  SAVETMPS;

  PUSHMARK(SP);
  XPUSHs(msgref);
  for(i=0;i<argc;i++) {
    XPUSHs(sv_2mortal(newSVpv(argv[i], 0)));
  }
  PUTBACK;

  count = call_method(method, G_SCALAR|G_EVAL);

  SPAGAIN;

  if(count != 1) {
    fprintf(stderr, "perl returned wrong count %d\n", count);
    abort();
  }

  if (SvTRUE(ERRSV)) {
    STRLEN n_a;
    owl_function_error("Error: '%s'", SvPV(ERRSV, n_a));
    /* and clear the error */
    sv_setsv (ERRSV, &PL_sv_undef);
  }

  srv = POPs;

  if (srv) {
    preout=SvPV(srv, len);
    out = owl_malloc(strlen(preout)+1);
    strncpy(out, preout, len);
    out[len] = '\0';
  } else {
    out = NULL;
  }

  PUTBACK;
  FREETMPS;
  LEAVE;

  return out;
}


char *owl_perlconfig_initperl(char * file)
{
  int ret;
  PerlInterpreter *p;
  char *err;
  char *args[] = {"", "-e", "0;", NULL};

  /* create and initialize interpreter */
  p=perl_alloc();
  owl_global_set_perlinterp(&g, (void*)p);
  perl_construct(p);

  owl_global_set_no_have_config(&g);


  ret=perl_parse(p, owl_perl_xs_init, sizeof(args)/sizeof(*args) - 1, args, NULL);
  if (ret || SvTRUE(ERRSV)) {
    STRLEN n_a;
    err=owl_strdup(SvPV(ERRSV, n_a));
    sv_setsv(ERRSV, &PL_sv_undef);     /* and clear the error */
    return(err);
  }

  ret=perl_run(p);
  if (ret || SvTRUE(ERRSV)) {
    STRLEN n_a;
    err=owl_strdup(SvPV(ERRSV, n_a));
    sv_setsv(ERRSV, &PL_sv_undef);     /* and clear the error */
    return(err);
  }

  owl_global_set_have_config(&g);

  /* create legacy variables */
  perl_get_sv("BarnOwl::id", TRUE);
  perl_get_sv("BarnOwl::class", TRUE);
  perl_get_sv("BarnOwl::instance", TRUE);
  perl_get_sv("BarnOwl::recipient", TRUE);
  perl_get_sv("BarnOwl::sender", TRUE);
  perl_get_sv("BarnOwl::realm", TRUE);
  perl_get_sv("BarnOwl::opcode", TRUE);
  perl_get_sv("BarnOwl::zsig", TRUE);
  perl_get_sv("BarnOwl::msg", TRUE);
  perl_get_sv("BarnOwl::time", TRUE);
  perl_get_sv("BarnOwl::host", TRUE);
  perl_get_av("BarnOwl::fields", TRUE);

  if(file) {
    SV * cfg = get_sv("BarnOwl::configfile", TRUE);
    sv_setpv(cfg, file);
  }

  perl_eval_pv(owl_perlwrap_codebuff, FALSE);

  if (SvTRUE(ERRSV)) {
    STRLEN n_a;
    err=owl_strdup(SvPV(ERRSV, n_a));
    sv_setsv (ERRSV, &PL_sv_undef);     /* and clear the error */
    return(err);
  }

  /* check if we have the formatting function */
  if (owl_perlconfig_is_function("BarnOwl::format_msg")) {
    owl_global_set_config_format(&g, 1);
  }

  return(NULL);
}

void owl_perlconfig_shutdown() {
  PerlInterpreter *p;
  p = owl_global_get_perlinterp(&g);
  perl_destruct(p);
  perl_free(p);
}

/* returns whether or not a function exists */
int owl_perlconfig_is_function(char *fn) {
  if (perl_get_cv(fn, FALSE)) return(1);
  else return(0);
}

/* caller is responsible for freeing returned string */
char *owl_perlconfig_execute(char *line)
{
  STRLEN len;
  SV *response;
  char *out, *preout;

  if (!owl_global_have_config(&g)) return NULL;

  /* execute the subroutine */
  response = perl_eval_pv(line, FALSE);

  if (SvTRUE(ERRSV)) {
    STRLEN n_a;
    owl_function_error("Perl Error: '%s'", SvPV(ERRSV, n_a));
    sv_setsv (ERRSV, &PL_sv_undef);     /* and clear the error */
  }

  preout=SvPV(response, len);
  /* leave enough space in case we have to add a newline */
  out = owl_malloc(strlen(preout)+2);
  strncpy(out, preout, len);
  out[len] = '\0';
  if (!strlen(out) || out[strlen(out)-1]!='\n') {
    strcat(out, "\n");
  }

  return(out);
}

char *owl_perlconfig_getmsg(owl_message *m, int mode, char *subname)
{ 
  /* if mode==1 we are doing message formatting.  The returned
   * formatted message needs to be freed by the caller.
   *
   * if mode==0 we are just doing the message-has-been-received
   * thing.
   */
  if (!owl_global_have_config(&g)) return(NULL);
  
  /* run the procedure corresponding to the mode */
  if (mode==1) {
    char *ret = NULL;
    ret = owl_perlconfig_call_with_message(subname?subname
					   :"BarnOwl::_format_msg_legacy_wrap", m);
    if (!ret) {
      ret = owl_sprintf("@b([Perl Message Formatting Failed!])\n");
    } 
    return ret;
  } else {
    char *ptr = NULL;
    if (owl_perlconfig_is_function("BarnOwl::Hooks::_receive_msg")) {
      ptr = owl_perlconfig_call_with_message(subname?subname
				       :"BarnOwl::_receive_msg_legacy_wrap", m);
    }
    if (ptr) owl_free(ptr);
    return(NULL);
  }
}

char *owl_perlconfig_perlcmd(owl_cmd *cmd, int argc, char **argv)
{
  int i, count;
  char * ret = NULL;
  SV *rv;
  STRLEN n_a;
  dSP;

  ENTER;
  SAVETMPS;

  PUSHMARK(SP);
  for(i=0;i<argc;i++) {
    XPUSHs(sv_2mortal(newSVpv(argv[i], 0)));
  }
  PUTBACK;

  count = call_sv(cmd->cmd_perl, G_SCALAR|G_EVAL);

  SPAGAIN;

  if(SvTRUE(ERRSV)) {
    owl_function_error("%s", SvPV(ERRSV, n_a));
    (void)POPs;
  } else {
    if(count != 1)
      croak("Perl command %s returned more than one value!", cmd->name);
    rv = POPs;
    if(SvTRUE(rv)) {
      ret = owl_strdup(SvPV(rv, n_a));
    }
  }

  FREETMPS;
  LEAVE;

  return ret;
}

void owl_perlconfig_cmd_free(owl_cmd *cmd)
{
  SvREFCNT_dec(cmd->cmd_perl);
}

void owl_perlconfig_dispatch_free(owl_dispatch *d)
{
  SvREFCNT_dec(d->pfunc);
}

void owl_perlconfig_edit_callback(owl_editwin *e)
{
  SV *cb = (SV*)(e->cbdata);
  unsigned int n_a;
  dSP;

  if(cb == NULL) {
    owl_function_error("Perl callback is NULL!");
  }

  ENTER;
  SAVETMPS;

  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpv(owl_editwin_get_text(e), 0)));
  PUTBACK;
  
  call_sv(cb, G_DISCARD|G_EVAL);

  if(SvTRUE(ERRSV)) {
    owl_function_error("%s", SvPV(ERRSV, n_a));
  }

  FREETMPS;
  LEAVE;

  SvREFCNT_dec(cb);
  e->cbdata = NULL;
}

void owl_perlconfig_mainloop()
{
  dSP;
  if (!owl_perlconfig_is_function("BarnOwl::Hooks::_mainloop_hook"))
    return;
  PUSHMARK(SP) ;
  call_pv("BarnOwl::Hooks::_mainloop_hook", G_DISCARD|G_EVAL);
  if(SvTRUE(ERRSV)) {
    STRLEN n_a;
    owl_function_error("%s", SvPV(ERRSV, n_a));
  }
  return;
}

SV * owl_perl_new(char *class)
{
  return owl_perl_new_argv(class, NULL, 0);

}

SV * owl_perl_new_argv(char *class, char **argv, int argc)
{
  SV *obj;
  int i;
  OWL_PERL_CALL_METHOD(sv_2mortal(newSVpv(class, 0)), "new",
                       for(i=0;i<argc;i++) {
                         XPUSHs(sv_2mortal(newSVpv(argv[i], 0)));
                       }
                       ,
                       "Error in perl: %s\n",
                       1,
                       obj = POPs;
                       SvREFCNT_inc(obj);
                       );
  return obj;
}

void owl_perl_savetmps() {
  ENTER;
  SAVETMPS;
}

void owl_perl_freetmps() {
  FREETMPS;
  LEAVE;
}

void owl_perlconfig_do_dispatch(owl_dispatch *d)
{
  SV *cb = d->pfunc;
  unsigned int n_a;
  dSP;
  if(cb == NULL) {
    owl_function_error("Perl callback is NULL!");
  }

  ENTER;
  SAVETMPS;

  PUSHMARK(SP);
  PUTBACK;
  
  call_sv(cb, G_DISCARD|G_KEEPERR|G_EVAL);

  if(SvTRUE(ERRSV)) {
    owl_function_error("%s", SvPV(ERRSV, n_a));
  }

  FREETMPS;
  LEAVE;
}
