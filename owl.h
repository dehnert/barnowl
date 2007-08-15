/*  Copyright (c) 2004 James Kretchmar. All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *  
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *  
 *    * Redistributions in any form must be accompanied by information on
 *      how to obtain complete source code for the Owl software and any
 *      accompanying software that uses the Owl software. The source code
 *      must either be included in the distribution or be available for no
 *      more than the cost of distribution plus a nominal fee, and must be
 *      freely redistributable under reasonable conditions. For an
 *      executable file, complete source code means the source code for
 *      all modules it contains. It does not include source code for
 *      modules or files that typically accompany the major components of
 *      the operating system on which the executable file runs.
 *  
 * 
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
 *  NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 *  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 *  OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 *  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef INC_OWL_H
#define INC_OWL_H

#ifndef OWL_PERL
#include <curses.h>
#endif
#include <sys/param.h>
#include <EXTERN.h>
#include <netdb.h>
#include <regex.h>
#include <time.h>
#include <signal.h>
#include <termios.h>
#include <libfaim/aim.h>
#include "config.h"
#ifdef HAVE_LIBZEPHYR
#include <zephyr/zephyr.h>
#endif
#ifdef HAVE_COM_ERR_H
#include <com_err.h>
#endif

/* Perl and curses don't play nice. */
#ifdef OWL_PERL
typedef void WINDOW;
#include <perl.h>
#include "XSUB.h"
#else
typedef void SV;
#endif

static const char owl_h_fileIdent[] = "$Id$";

#define STRINGIFY(x) _STRINGIFY(x)
#define _STRINGIFY(x) #x

#ifndef OWL_SVN_REVNO
#define OWL_SVN_REVNO ????
#endif

#define OWL_VERSION_STRING "r" STRINGIFY(OWL_SVN_REVNO)


/* Feature that is being tested to redirect stderr through a pipe. 
 * There may still be some portability problems with this. */
#define OWL_STDERR_REDIR 1

#define OWL_DEBUG 0
#define OWL_DEBUG_FILE "/var/tmp/owldebug"

#define OWL_CONFIG_DIR "/.owl"             /* this is relative to the user's home directory */
#define OWL_STARTUP_FILE "/.owl/startup"   /* this is relative to the user's home directory */

#define OWL_FMTEXT_ATTR_NONE      0
#define OWL_FMTEXT_ATTR_BOLD      1
#define OWL_FMTEXT_ATTR_REVERSE   2
#define OWL_FMTEXT_ATTR_UNDERLINE 4

#define OWL_COLOR_BLACK     0
#define OWL_COLOR_RED       1
#define OWL_COLOR_GREEN     2
#define OWL_COLOR_YELLOW    3
#define OWL_COLOR_BLUE      4
#define OWL_COLOR_MAGENTA   5
#define OWL_COLOR_CYAN      6
#define OWL_COLOR_WHITE     7
#define OWL_COLOR_DEFAULT   -1

#define OWL_EDITWIN_STYLE_MULTILINE 0
#define OWL_EDITWIN_STYLE_ONELINE   1

#define OWL_PROTOCOL_ZEPHYR         0
#define OWL_PROTOCOL_AIM            1
#define OWL_PROTOCOL_JABBER         2
#define OWL_PROTOCOL_ICQ            3
#define OWL_PROTOCOL_YAHOO          4
#define OWL_PROTOCOL_MSN            5

#define OWL_MESSAGE_DIRECTION_NONE  0
#define OWL_MESSAGE_DIRECTION_IN    1
#define OWL_MESSAGE_DIRECTION_OUT   2

#define OWL_MUX_READ   1
#define OWL_MUX_WRITE  2
#define OWL_MUX_EXCEPT 4

#define OWL_DIRECTION_NONE      0
#define OWL_DIRECTION_DOWNWARDS 1
#define OWL_DIRECTION_UPWARDS   2

#define OWL_LOGGING_DIRECTION_BOTH 0
#define OWL_LOGGING_DIRECTION_IN   1
#define OWL_LOGGING_DIRECTION_OUT  2

#define OWL_SCROLLMODE_NORMAL      0
#define OWL_SCROLLMODE_TOP         1
#define OWL_SCROLLMODE_NEARTOP     2
#define OWL_SCROLLMODE_CENTER      3
#define OWL_SCROLLMODE_PAGED       4
#define OWL_SCROLLMODE_PAGEDCENTER 5

#define OWL_STYLE_TYPE_INTERNAL  0
#define OWL_STYLE_TYPE_PERL      1

#define OWL_TAB               3  /* This *HAS* to be the size of TABSTR below */
#define OWL_TABSTR        "   "
#define OWL_MSGTAB            7
#define OWL_TYPWIN_SIZE       8
#define OWL_HISTORYSIZE       50

/* Indicate current state, as well as what is allowed */
#define OWL_CTX_ANY          0xffff
/* Only one of these may be active at a time... */
#define OWL_CTX_MODE_BITS    0x000f
#define OWL_CTX_STARTUP      0x0001
#define OWL_CTX_READCONFIG   0x0002
#define OWL_CTX_INTERACTIVE  0x0004
/* Only one of these may be active at a time... */
#define OWL_CTX_ACTIVE_BITS  0xfff0
#define OWL_CTX_POPWIN       0x00f0
#define OWL_CTX_POPLESS      0x0010
#define OWL_CTX_RECWIN       0x0f00
#define OWL_CTX_RECV         0x0100
#define OWL_CTX_TYPWIN       0xf000
#define OWL_CTX_EDIT         0x7000
#define OWL_CTX_EDITLINE     0x1000
#define OWL_CTX_EDITMULTI    0x2000
#define OWL_CTX_EDITRESPONSE 0x4000

#define OWL_USERCLUE_NONE       0
#define OWL_USERCLUE_CLASSES    1
#define OWL_USERCLUE_FOOBAR     2
#define OWL_USERCLUE_BAZ        4

#define OWL_WEBBROWSER_NONE     0
#define OWL_WEBBROWSER_NETSCAPE 1
#define OWL_WEBBROWSER_GALEON   2
#define OWL_WEBBROWSER_OPERA    3

#define OWL_VARIABLE_OTHER      0
#define OWL_VARIABLE_INT        1
#define OWL_VARIABLE_BOOL       2
#define OWL_VARIABLE_STRING     3

#define OWL_FILTER_MAX_DEPTH    300

#define OWL_KEYMAP_MAXSTACK     20

#define OWL_KEYBINDING_COMMAND  1   /* command string */
#define OWL_KEYBINDING_FUNCTION 2   /* function taking no args */

#define OWL_DEFAULT_ZAWAYMSG    "I'm sorry, but I am currently away from the terminal and am\nnot able to receive your message.\n"
#define OWL_DEFAULT_AAWAYMSG    "I'm sorry, but I am currently away from the terminal and am\nnot able to receive your message.\n"

#define OWL_INCLUDE_REG_TESTS   1  /* whether to build in regression tests */

#define OWL_CMD_ALIAS_SUMMARY_PREFIX "command alias to: "

#define OWL_WEBZEPHYR_PRINCIPAL "daemon.webzephyr"
#define OWL_WEBZEPHYR_CLASS     "webzephyr"
#define OWL_WEBZEPHYR_OPCODE    "webzephyr"

#define OWL_REGEX_QUOTECHARS    "+*.?[]^\\${}()"
#define OWL_REGEX_QUOTEWITH     "\\"

#if defined(HAVE_DES_STRING_TO_KEY) && defined(HAVE_DES_KEY_SCHED) && defined(HAVE_DES_ECB_ENCRYPT)
#define OWL_ENABLE_ZCRYPT 1
#endif

#define OWL_META(key) ((key)|0200)
/* OWL_CTRL is definied in kepress.c */

#define LINE 2048

typedef struct _owl_variable {
  char *name;
  int   type;  /* OWL_VARIABLE_* */
  void *pval_default;  /* for types other and string */
  int   ival_default;  /* for types int and bool     */
  char *validsettings;		/* documentation of valid settings */
  char *summary;		/* summary of usage */
  char *description;		/* detailed description */
  void *val;                    /* current value */
  int  (*validate_fn)(struct _owl_variable *v, void *newval);
                                /* returns 1 if newval is valid */
  int  (*set_fn)(struct _owl_variable *v, void *newval); 
                                /* sets the variable to a value
				 * of the appropriate type.
				 * unless documented, this 
				 * should make a copy. 
				 * returns 0 on success. */
  int  (*set_fromstring_fn)(struct _owl_variable *v, char *newval);
                                /* sets the variable to a value
				 * of the appropriate type.
				 * unless documented, this 
				 * should make a copy. 
				 * returns 0 on success. */
  void *(*get_fn)(struct _owl_variable *v);
				/* returns a reference to the current value.
				 * WARNING:  this approach is hard to make
				 * thread-safe... */
  int  (*get_tostring_fn)(struct _owl_variable *v, 
			  char *buf, int bufsize, void *val); 
                                /* converts val to a string 
				 * and puts into buf */
  void  (*free_fn)(struct _owl_variable *v);
				/* frees val as needed */
} owl_variable;

typedef struct _owl_fmtext {
  int textlen;
  int bufflen;
  char *textbuff;
  char *fmbuff;
  short *fgcolorbuff;
  short *bgcolorbuff;
} owl_fmtext;

typedef struct _owl_list {
  int size;
  int avail;
  void **list;
} owl_list;

typedef struct _owl_dict_el {
  char *k;			/* key   */
  void *v;			/* value */
} owl_dict_el;

typedef struct _owl_dict {
  int size;
  int avail;
  owl_dict_el *els;		/* invariant: sorted by k */
} owl_dict;
typedef owl_dict owl_vardict;	/* dict of variables */
typedef owl_dict owl_cmddict;	/* dict of commands */

typedef struct _owl_context {
  int   mode;
  void *data;		/* determined by mode */
} owl_context;

typedef struct _owl_cmd {	/* command */
  char *name;

  char *summary;		/* one line summary of command */
  char *usage;			/* usage synopsis */
  char *description;		/* long description of command */

  int validctx;			/* bitmask of valid contexts */

  /* we should probably have a type here that says which of
   * the following is valid, and maybe make the below into a union... */

  /* Only one of these may be non-NULL ... */

  char *cmd_aliased_to;		/* what this command is aliased to... */
  
  /* These don't take any context */
  char *(*cmd_args_fn)(int argc, char **argv, char *buff);  
				/* takes argv and the full command as buff.
				 * caller must free return value if !NULL */
  void (*cmd_v_fn)(void);	/* takes no args */
  void (*cmd_i_fn)(int i);	/* takes an int as an arg */

  /* The following also take the active context if it's valid */
  char *(*cmd_ctxargs_fn)(void *ctx, int argc, char **argv, char *buff);  
				/* takes argv and the full command as buff.
				 * caller must free return value if !NULL */
  void (*cmd_ctxv_fn)(void *ctx);	        /* takes no args */
  void (*cmd_ctxi_fn)(void *ctx, int i);	/* takes an int as an arg */
  SV *cmd_perl;                                /* Perl closure that takes a list of args */
} owl_cmd;


typedef struct _owl_zwrite {
  char *class;
  char *inst;
  char *realm;
  char *opcode;
  char *zsig;
  char *message;
  owl_list recips;
  int cc;
  int noping;
} owl_zwrite;

typedef struct _owl_pair {
  void *key;
  void *value;
} owl_pair;

struct _owl_fmtext_cache;

typedef struct _owl_message {
  int id;
  int direction;
#ifdef HAVE_LIBZEPHYR
  ZNotice_t notice;
#endif
  struct _owl_fmtext_cache * fmtext;
  int delete;
  char *hostname;
  owl_list attributes;            /* this is a list of pairs */
  char *timestr;
  time_t time;
  char *zwriteline;
} owl_message;

#define OWL_FMTEXT_CACHE_SIZE 1000
/* We cache the saved fmtexts for the last bunch of messages we
   rendered */
typedef struct _owl_fmtext_cache {
    owl_message * message;
    owl_fmtext fmtext;
} owl_fmtext_cache;

typedef struct _owl_style {
  char *name;
  char *description;
  int type;
  char *perlfuncname;
  void (*formatfunc) (owl_fmtext *fm, owl_message *m);
} owl_style;

typedef struct _owl_mainwin {
  int curtruncated;
  int lasttruncated;
  int lastdisplayed;
} owl_mainwin;

typedef struct _owl_viewwin {
  owl_fmtext fmtext;
  int textlines;
  int topline;
  int rightshift;
  int winlines, wincols;
  WINDOW *curswin;
  void (*onclose_hook) (struct _owl_viewwin *vwin, void *data);
  void *onclose_hook_data;
} owl_viewwin;
  
typedef struct _owl_popwin {
  WINDOW *borderwin;
  WINDOW *popwin;
  int lines;
  int cols;
  int active;
  int needsfirstrefresh;
  void (*handler) (int ch);
} owl_popwin;

typedef struct _owl_popexec {
  int refcount;
  owl_viewwin *vwin;
  int winactive;
  int pid;			/* or 0 if it has terminated */
  int rfd;  
} owl_popexec;

typedef struct _owl_messagelist {
  owl_list list;
} owl_messagelist;

typedef struct _owl_regex {
  int negate;
  char *string;
  regex_t re;
} owl_regex;

typedef struct _owl_filterelement {
  int (*match_message)(struct _owl_filterelement *fe, owl_message *m);
  /* Append a string representation of the filterelement onto buf*/
  void (*print_elt)(struct _owl_filterelement *fe, char * buf);
  /* Operands for and,or,not*/
  struct _owl_filterelement *left, *right;
  /* For regex filters*/
  owl_regex re;
  /* Used by regexes, filter references, and perl */
  char *field;
} owl_filterelement;

typedef struct _owl_filter {
  char *name;
  int polarity;
  owl_filterelement * root;
  int fgcolor;
  int bgcolor;
  int cachedmsgid;  /* cached msgid: should move into view eventually */
} owl_filter;

typedef struct _owl_view {
  char *name;
  owl_filter *filter;
  owl_messagelist ml;
  owl_style *style;
} owl_view;

typedef struct _owl_history {
  owl_list hist;
  int cur;
  int touched;
  int partial;
  int repeats;
} owl_history;

typedef struct _owl_editwin {
  char *buff;
  owl_history *hist;
  int bufflen;
  int allocated;
  int buffx, buffy;
  int topline;
  int winlines, wincols, fillcol, wrapcol;
  WINDOW *curswin;
  int style;
  int lock;
  int dotsend;
  int echochar;

  char *command;
  void (*callback)(struct _owl_editwin*);
  void *cbdata;
} owl_editwin;

typedef struct _owl_mux {
  int handle;			/* for referencing this */
  int active;			/* has this been deleted? */
  int fd;		       
  int eventmask;		/* bitmask of OWL_MUX_* */
  void (*handler_fn)(int handle, int fd, int eventmask, void *data);
  void *data;			/* data reference to pass to callback */
} owl_mux;
typedef owl_list owl_muxevents;

typedef struct _owl_keybinding {
  int  *j;			/* keypress stack (0-terminated) */  
  int   type;			/* command or function? */
  char *desc;			/* description (or "*user*") */
  char *command;		/* command, if of type command */
  void (*function_fn)(void);	/* function ptr, if of type function */
} owl_keybinding;

typedef struct _owl_keymap {
  char     *name;		/* name of keymap */
  char     *desc;		/* description */
  owl_list  bindings;		/* key bindings */
  struct _owl_keymap *submap;	/* submap */
  void (*default_fn)(int j);	/* default action (takes a keypress) */
  void (*prealways_fn)(int j);	/* always called before a keypress is received */
  void (*postalways_fn)(int j);	/* always called after keypress is processed */
} owl_keymap;

typedef struct _owl_keyhandler {
  owl_dict  keymaps;		/* dictionary of keymaps */
  owl_keymap *active;		/* currently active keymap */
  int	    in_esc;		/* escape pressed? */
  int       kpstack[OWL_KEYMAP_MAXSTACK+1]; /* current stack of keypresses */
  int       kpstackpos;		/* location in stack (-1 = none) */
} owl_keyhandler;

typedef struct _owl_buddy {
  int proto;
  char *name;
  int isidle;
  int idlesince;
} owl_buddy;

typedef struct _owl_buddylist {
  owl_list buddies;
} owl_buddylist;

typedef struct _owl_zbuddylist {
  owl_list zusers;
} owl_zbuddylist;

typedef struct _owl_timer {
  int direction;
  time_t starttime;
  int start;
} owl_timer;

typedef struct _owl_errqueue {
  owl_list errlist;
} owl_errqueue;

typedef struct _owl_colorpair_mgr {
  int next;
  short **pairs;
} owl_colorpair_mgr;

typedef struct _owl_obarray {
  owl_list strings;
} owl_obarray;

typedef struct _owl_global {
  owl_mainwin mw;
  owl_popwin pw;
  owl_history cmdhist;		/* command history */
  owl_history msghist;		/* outgoing message history */
  owl_keyhandler kh;
  owl_list filterlist;
  owl_list puntlist;
  owl_muxevents muxevents;	/* fds to dispatch on */
  owl_vardict vars;
  owl_cmddict cmds;
  owl_context ctx;
  owl_errqueue errqueue;
  int lines, cols;
  int curmsg, topmsg;
  int curmsg_vert_offset;
  owl_view current_view;
  owl_messagelist msglist;
  WINDOW *recwin, *sepwin, *msgwin, *typwin;
  int needrefresh;
  int rightshift;
  int resizepending;
  int recwinlines;
  int typwinactive;
  char *thishost;
  char *homedir;
  char *confdir;
  char *startupfile;
  int direction;
  int zaway;
  char *cur_zaway_msg;
  int haveconfig;
  int config_format;
  void *buffercbdata;
  owl_editwin tw;
  owl_viewwin vw;
  void *perl;
  int debug;
  time_t starttime;
  time_t lastinputtime;
  char *startupargs;
  int userclue;
  int nextmsgid;
  int hascolors;
  int colorpairs;
  owl_colorpair_mgr cpmgr;
  int searchactive;
  int newmsgproc_pid;
  int malloced, freed;
  char *searchstring;
  aim_session_t aimsess;
  aim_conn_t bosconn;
  owl_timer aim_noop_timer;
  owl_timer aim_ignorelogin_timer;
  owl_timer aim_buddyinfo_timer;
  int aim_loggedin;         /* true if currently logged into AIM */
  int aim_doprocessing;     /* true if we should process AIM events (like pending login) */
  char *aim_screenname;     /* currently logged in AIM screen name */
  owl_buddylist buddylist;  /* list of logged in AIM buddies */
  owl_list messagequeue;    /* for queueing up aim and other messages */
  owl_dict styledict;       /* global dictionary of available styles */
  char *response;           /* response to the last question asked */
  int havezephyr;
  int haveaim;
  int got_err_signal;	    /* 1 if we got an unexpected signal */
  siginfo_t err_signal_info;
  owl_zbuddylist zbuddies;
  owl_timer zephyr_buddycheck_timer;
  struct termios startup_tio;
  owl_obarray obarray;
} owl_global;

/* globals */
extern owl_global g;

#include "owl_prototypes.h"

/* these are missing from the zephyr includes for some reason */
#ifdef HAVE_LIBZEPHYR
int ZGetSubscriptions(ZSubscription_t *, int *);
int ZGetLocations(ZLocations_t *,int *);
#endif

#endif /* INC_OWL_H */
