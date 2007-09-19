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

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <sys/param.h>
#include <sys/types.h>
#include <termios.h>
#include <sys/stat.h>
#include "owl.h"

#if OWL_STDERR_REDIR
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif
int stderr_replace(void);
#endif

static const char fileIdent[] = "$Id$";

owl_global g;

char * owl_get_datadir() {
    char * datadir = getenv("BARNOWL_DATA_DIR");
    if(datadir != NULL)
        return strchr(datadir, '=') + 1;
    return DATADIR;
}

int main(int argc, char **argv, char **env)
{
  WINDOW *recwin, *sepwin, *typwin, *msgwin;
  owl_editwin *tw;
  owl_popwin *pw;
  int j, ret, initialsubs, debug, argcsave, followlast;
  int newmsgs, nexttimediff;
  struct sigaction sigact;
  char *configfile, *tty, *perlout, *perlerr, **argvsave, buff[LINE], startupmsg[LINE];
  char *confdir;
  owl_filter *f;
  owl_style *s;
  time_t nexttime, now;
  struct tm *today;
  char *dir;
  struct termios tio;
  owl_message *m;
#if OWL_STDERR_REDIR
  int newstderr;
#endif

  argcsave=argc;
  argvsave=argv;
  configfile=NULL;
  confdir = NULL;
  tty=NULL;
  debug=0;
  initialsubs=1;
  if (argc>0) {
    argv++;
    argc--;
  }
  while (argc>0) {
    if (!strcmp(argv[0], "-n")) {
      initialsubs=0;
      argv++;
      argc--;
    } else if (!strcmp(argv[0], "-c")) {
      if (argc<2) {
        fprintf(stderr, "Too few arguments to -c\n");
        usage();
        exit(1);
      }
      configfile=argv[1];
      argv+=2;
      argc-=2;
    } else if (!strcmp(argv[0], "-t")) {
      if (argc<2) {
        fprintf(stderr, "Too few arguments to -t\n");
        usage();
        exit(1);
      }
      tty=argv[1];
      argv+=2;
      argc-=2;
    } else if (!strcmp(argv[0], "-s")){
      if (argc<2) {
        fprintf(stderr, "Too few arguments to -s\n");
        usage();
        exit(1);
      }
      confdir = argv[1];
      argv+=2;
      argc-=2;
    } else if (!strcmp(argv[0], "-d")) {
      debug=1;
      argv++;
      argc--;
    } else if (!strcmp(argv[0], "-D")) {
      debug=1;
      unlink(OWL_DEBUG_FILE);
      argv++;
      argc--;
    } else if (!strcmp(argv[0], "-v")) {
      printf("This is owl version %s\n", OWL_VERSION_STRING);
      exit(0);
    } else {
      fprintf(stderr, "Uknown argument\n");
      usage();	      
      exit(1);
    }
  }

  owl_function_debugmsg("startup: Finished parsing arguments");

  /* signal handler */
  /*sigact.sa_handler=sig_handler;*/
  sigact.sa_sigaction=sig_handler;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags=SA_SIGINFO;
  sigaction(SIGWINCH, &sigact, NULL);
  sigaction(SIGALRM, &sigact, NULL);
  sigaction(SIGPIPE, &sigact, NULL);
  sigaction(SIGTERM, &sigact, NULL);
  sigaction(SIGHUP, &sigact, NULL);

  /* save initial terminal settings */
  tcgetattr(0, owl_global_get_startup_tio(&g));

  /* turn ISTRIP off */
  tcgetattr(0, &tio);
  tio.c_iflag &= ~ISTRIP;
  tcsetattr(0, TCSAFLUSH, &tio);

  /* screen init */
  if (!getenv("TERMINFO")) {
    owl_function_debugmsg("startup: Not setting TERMINFO");
  } else {
    owl_function_debugmsg("startup: leaving TERMINFO as %s from envrionment", getenv("TERMINFO"));
  }
  initscr();
  start_color();
#ifdef HAVE_USE_DEFAULT_COLORS
  use_default_colors();
#endif
  raw();
  noecho();

  /* define simple color pairs */
  if (has_colors() && COLOR_PAIRS>=8) {
    int bg = COLOR_BLACK;
#ifdef HAVE_USE_DEFAULT_COLORS
    bg = -1;
#endif
    init_pair(OWL_COLOR_BLACK,   COLOR_BLACK,   bg);
    init_pair(OWL_COLOR_RED,     COLOR_RED,     bg);
    init_pair(OWL_COLOR_GREEN,   COLOR_GREEN,   bg);
    init_pair(OWL_COLOR_YELLOW,  COLOR_YELLOW,  bg);
    init_pair(OWL_COLOR_BLUE,    COLOR_BLUE,    bg);
    init_pair(OWL_COLOR_MAGENTA, COLOR_MAGENTA, bg);
    init_pair(OWL_COLOR_CYAN,    COLOR_CYAN,    bg);
    init_pair(OWL_COLOR_WHITE,   COLOR_WHITE,   bg);
  }

  /* owl global init */
  owl_global_init(&g);
  if (debug) owl_global_set_debug_on(&g);
  if (confdir) owl_global_set_confdir(&g, confdir);
  owl_function_debugmsg("startup: first available debugging message");
  owl_global_set_startupargs(&g, argcsave, argvsave);
  owl_global_set_haveaim(&g);

#ifdef HAVE_LIBZEPHYR
  /* zephyr init */
  ret=owl_zephyr_initialize();
  if (!ret)
      owl_global_set_havezephyr(&g);
#endif

#if OWL_STDERR_REDIR
  /* Do this only after we've started curses up... */
  owl_function_debugmsg("startup: doing stderr redirection");
  newstderr = stderr_replace();
  owl_muxevents_add(owl_global_get_muxevents(&g), newstderr, OWL_MUX_READ,
		    stderr_redirect_handler, NULL);
#endif   

  /* create the owl directory, in case it does not exist */
  owl_function_debugmsg("startup: creating owl directory, if not present");
  dir=owl_global_get_confdir(&g);
  mkdir(dir, S_IRWXU);

  /* set the tty, either from the command line, or by figuring it out */
  owl_function_debugmsg("startup: setting tty name");
  if (tty) {
    owl_global_set_tty(&g, tty);
  } else {
    owl_global_set_tty(&g, owl_util_get_default_tty());
  }

  /* Initialize perl */
  owl_function_debugmsg("startup: processing config file");
  owl_context_set_readconfig(owl_global_get_context(&g));
  perlerr=owl_perlconfig_initperl(configfile);
  if (perlerr) {
    endwin();
    owl_function_error("Internal perl error: %s\n", perlerr);
    fprintf(stderr, "Internal perl error: %s\n", perlerr);
    fflush(stderr);
    printf("Internal perl error: %s\n", perlerr);
    fflush(stdout);
    exit(1);
  }

  /* setup the built-in styles */
  owl_function_debugmsg("startup: creating built-in styles");

  s=owl_malloc(sizeof(owl_style));
  owl_style_create_internal(s, "basic", &owl_stylefunc_basic, "Basic message formatting.");
  owl_global_add_style(&g, s);
#if 0
  s=owl_malloc(sizeof(owl_style));
  owl_style_create_internal(s, "vt", &owl_stylefunc_vt, "VT message formatting.");
  owl_global_add_style(&g, s);
#endif
  s=owl_malloc(sizeof(owl_style));
  owl_style_create_internal(s, "oneline", &owl_stylefunc_oneline, "Formats for one-line-per-message");
  owl_global_add_style(&g, s);

  /* setup the default filters */
  /* the personal filter will need to change again when AIM chat's are
   *  included.  Also, there should be an %aimme% */
  owl_function_debugmsg("startup: creating default filters");
  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "personal", "isprivate ^true$ and ( not type ^zephyr$"
                             " or ( class ^message and"
                             " ( instance ^personal$ or instance ^urgent$ ) ) )");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "wordwrap", "not ( type ^admin$ or type ^zephyr$ ) ");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "trash", "class ^mail$ or opcode ^ping$ or type ^admin$ or ( not login ^none$ )");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "ping", "opcode ^ping$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "auto", "opcode ^auto$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "login", "not login ^none$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "reply-lockout", "class ^noc or class ^mail$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "out", "direction ^out$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "aim", "type ^aim$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "zephyr", "type ^zephyr$");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "none", "false");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  f=owl_malloc(sizeof(owl_filter));
  owl_filter_init_fromstring(f, "all", "true");
  owl_list_append_element(owl_global_get_filterlist(&g), f);

  /* set the current view */
  owl_function_debugmsg("startup: setting the current view");
  owl_view_create(owl_global_get_current_view(&g), "main", f, owl_global_get_style_by_name(&g, "default"));

  /* AIM init */
  owl_function_debugmsg("startup: doing AIM initialization");
  owl_aim_init();

  /* execute the startup function in the configfile */
  owl_function_debugmsg("startup: executing perl startup, if applicable");
  perlout = owl_perlconfig_execute("BarnOwl::Hooks::_startup();");
  if (perlout) owl_free(perlout);

  /* hold on to the window names for convenience */
  msgwin=owl_global_get_curs_msgwin(&g);
  recwin=owl_global_get_curs_recwin(&g);
  sepwin=owl_global_get_curs_sepwin(&g);
  typwin=owl_global_get_curs_typwin(&g);
  tw=owl_global_get_typwin(&g);

  /* welcome message */
  owl_function_debugmsg("startup: creating splash message");
  strcpy(startupmsg, "-----------------------------------------------------------------------\n");
  sprintf(buff,      "Welcome to barnowl version %s.  Press 'h' for on-line help.            \n", OWL_VERSION_STRING);
  strcat(startupmsg, buff);
  strcat(startupmsg, "                                                                       \n");
  strcat(startupmsg, "This is a development build of barnowl. If you are using this          \n");
  strcat(startupmsg, "build regularly, please add yourself to barnowl-users@mit.edu          \n");
  strcat(startupmsg, "                                                                 ^ ^   \n");
  strcat(startupmsg, "                                                                 OvO   \n");
  strcat(startupmsg, "Please report any bugs to dirty-owl-hackers@mit.edu             (   )  \n");
  strcat(startupmsg, "-----------------------------------------------------------------m-m---\n");
  owl_function_adminmsg("", startupmsg);
  sepbar(NULL);

  /* process the startup file */
  owl_function_debugmsg("startup: processing startup file");
  owl_function_source(NULL);

  wrefresh(sepwin);

  /* load zephyr subs */
  if (initialsubs) {
    int ret2;
    owl_function_debugmsg("startup: loading initial zephyr subs");

    /* load default subscriptions */
    ret=owl_zephyr_loaddefaultsubs();
    
    /* load subscriptions from subs file */
    ret2=owl_zephyr_loadsubs(NULL, 0);

    if (ret || ret2) {
      owl_function_adminmsg("", "Error loading zephyr subscriptions");
    } else if (ret2!=-1) {
      owl_global_add_userclue(&g, OWL_USERCLUE_CLASSES);
    }

    /* load login subscriptions */
    if (owl_global_is_loginsubs(&g)) {
      owl_function_debugmsg("startup: loading login subs");
      owl_function_loadloginsubs(NULL);
    }
  }

  /* First buddy check to sync the list without notifications */
  owl_function_debugmsg("startup: doing initial zephyr buddy check");
  /* owl_function_zephyr_buddy_check(0); */

  /* set the startup and default style, based on userclue and presence of a
   * formatting function */
  owl_function_debugmsg("startup: setting startup and default style");
  if (0 != strcmp(owl_global_get_default_style(&g), "__unspecified__")) {
    /* the style was set by the user: leave it alone */
  } else if (owl_global_is_config_format(&g)) {
    owl_global_set_default_style(&g, "perl");
  } else if (owl_global_is_userclue(&g, OWL_USERCLUE_CLASSES)) {
    owl_global_set_default_style(&g, "default");
  } else {
    owl_global_set_default_style(&g, "basic");
  }

  /* zlog in if we need to */
  if (owl_global_is_startuplogin(&g)) {
    owl_function_debugmsg("startup: doing zlog in");
    owl_zephyr_zlog_in();
  }

  owl_function_debugmsg("startup: set style for the view: %s", owl_global_get_default_style(&g));
  s = owl_global_get_style_by_name(&g, owl_global_get_default_style(&g));
  if(s)
      owl_view_set_style(owl_global_get_current_view(&g), s);
  else
      owl_function_error("No such style: %s", owl_global_get_default_style(&g));

  owl_function_debugmsg("startup: setting context interactive");
  owl_context_set_interactive(owl_global_get_context(&g));

  nexttimediff=10;
  nexttime=time(NULL);

  owl_function_debugmsg("startup: entering main loop");
  /* main loop */
  while (1) {

    /* if a resize has been scheduled, deal with it */
    owl_global_resize(&g, 0, 0);

    /* these are here in case a resize changes the windows */
    msgwin=owl_global_get_curs_msgwin(&g);
    recwin=owl_global_get_curs_recwin(&g);
    sepwin=owl_global_get_curs_sepwin(&g);
    typwin=owl_global_get_curs_typwin(&g);

    followlast=owl_global_should_followlast(&g);
    
    /* Do AIM stuff */
    if (owl_global_is_doaimevents(&g)) {
      owl_aim_process_events();

      if (owl_global_is_aimloggedin(&g)) {
	if (owl_timer_is_expired(owl_global_get_aim_buddyinfo_timer(&g))) {
	  /* owl_buddylist_request_idletimes(owl_global_get_buddylist(&g)); */
	  owl_timer_reset(owl_global_get_aim_buddyinfo_timer(&g));
	}
      }
    }

    owl_perlconfig_mainloop();

    /* little hack */
    now=time(NULL);
    today=localtime(&now);
    if (today->tm_mon==9 && today->tm_mday==31 && owl_global_get_runtime(&g)<600) {
      if (time(NULL)>nexttime) {
	if (nexttimediff==1) {
	  nexttimediff=10;
	} else {
	  nexttimediff=1;
	}
	nexttime+=nexttimediff;
	owl_hack_animate();
      }
    }

    owl_zephyr_process_events();
    
    /* Grab incoming messages. */
    newmsgs=0;
    while(owl_global_messagequeue_pending(&g)) {

      m = owl_global_messagequeue_popmsg(&g);

      if(owl_process_message(m))
        newmsgs = 1;
    }

    /* is it time to check zbuddies? */
    if (owl_global_is_pseudologins(&g)) {
      if (owl_timer_is_expired(owl_global_get_zephyr_buddycheck_timer(&g))) {
	owl_function_debugmsg("Doing zephyr buddy check");
	owl_function_zephyr_buddy_check(1);
	owl_timer_reset(owl_global_get_zephyr_buddycheck_timer(&g));
      }
    }

    /* dispatch any muxevents */
    owl_muxevents_dispatch(owl_global_get_muxevents(&g), 0);

    /* follow the last message if we're supposed to */
    if (newmsgs && followlast) {
      owl_function_lastmsg_noredisplay();
    }

    /* do the newmsgproc thing */
    if (newmsgs) {
      owl_function_do_newmsgproc();
    }
    
    /* redisplay if necessary */
    /* this should be optimized to not run if the new messages won't be displayed */
    if (newmsgs) {
      owl_mainwin_redisplay(owl_global_get_mainwin(&g));
      sepbar(NULL);
      if (owl_popwin_is_active(owl_global_get_popwin(&g))) {
	owl_popwin_refresh(owl_global_get_popwin(&g));
	/* TODO: this is a broken kludge */
	if (owl_global_get_viewwin(&g)) {
	  owl_viewwin_redisplay(owl_global_get_viewwin(&g), 0);
	}
      }
      owl_global_set_needrefresh(&g);
    }

    /* if a popwin just came up, refresh it */
    pw=owl_global_get_popwin(&g);
    if (owl_popwin_is_active(pw) && owl_popwin_needs_first_refresh(pw)) {
      owl_popwin_refresh(pw);
      owl_popwin_no_needs_first_refresh(pw);
      owl_global_set_needrefresh(&g);
      /* TODO: this is a broken kludge */
      if (owl_global_get_viewwin(&g)) {
	owl_viewwin_redisplay(owl_global_get_viewwin(&g), 0);
      }
    }

    /* update the terminal if we need to */
    if (owl_global_is_needrefresh(&g)) {
      /* leave the cursor in the appropriate window */
      if (owl_global_is_typwin_active(&g)) {
	owl_function_set_cursor(typwin);
      } else {
	owl_function_set_cursor(sepwin);
      }
      doupdate();
      owl_global_set_noneedrefresh(&g);
    }

    /* Handle all keypresses.  If no key has been pressed, sleep for a
     * little bit, but otherwise do not.  This lets input be grabbed
     * as quickly as possbile */
    j=wgetch(typwin);
    if (j==ERR) {
      usleep(10);
    } else {
      owl_global_update_lastinputtime(&g);
      /* find and activate the current keymap.
       * TODO: this should really get fixed by activating
       * keymaps as we switch between windows... 
       */
      if (pw && owl_popwin_is_active(pw) && owl_global_get_viewwin(&g)) {
	owl_context_set_popless(owl_global_get_context(&g), 
				owl_global_get_viewwin(&g));
	owl_function_activate_keymap("popless");
      } else if (owl_global_is_typwin_active(&g) 
		 && owl_editwin_get_style(tw)==OWL_EDITWIN_STYLE_ONELINE) {
	/*
	  owl_context_set_editline(owl_global_get_context(&g), tw);
	  owl_function_activate_keymap("editline");
	*/
      } else if (owl_global_is_typwin_active(&g) 
		 && owl_editwin_get_style(tw)==OWL_EDITWIN_STYLE_MULTILINE) {
	owl_context_set_editmulti(owl_global_get_context(&g), tw);
	owl_function_activate_keymap("editmulti");
      } else {
	owl_context_set_recv(owl_global_get_context(&g));
	owl_function_activate_keymap("recv");
      }
      /* now actually handle the keypress */
      ret = owl_keyhandler_process(owl_global_get_keyhandler(&g), j);
      if (ret!=0 && ret!=1) {
	owl_function_makemsg("Unable to handle keypress");
      }
    }

    /* Log any error signals */
    {
      siginfo_t si;
      int signum;
      if ((signum = owl_global_get_errsignal_and_clear(&g, &si)) > 0) {
	owl_function_error("Got unexpected signal: %d %s  (code: %d band: %d  errno: %d)", 
			   signum, signum==SIGPIPE?"SIGPIPE":"SIG????",
			   si.si_code, si.si_band, si.si_errno);
      }
    }

  }
}

/*
 * Process a new message passed to us on the message queue from some
 * protocol. This includes adding it to the message list, updating the
 * view and scrolling if appropriate, logging it, and so on.
 *
 * Either a pointer is kept to the message internally, or it is freed
 * if unneeded. The caller no longer ``owns'' the message's memory.
 *
 * Returns 1 if the message was added to the message list, and 0 if it
 * was ignored due to user settings or otherwise.
 */
int owl_process_message(owl_message *m) {
  owl_filter *f;
  /* if this message it on the puntlist, nuke it and continue */
  if (owl_global_message_is_puntable(&g, m)) {
    owl_message_free(m);
    return 0;
  }

  /*  login or logout that should be ignored? */
  if (owl_global_is_ignorelogins(&g)
      && owl_message_is_loginout(m)) {
    owl_message_free(m);
    return 0;
  }

  if (!owl_global_is_displayoutgoing(&g)
      && owl_message_is_direction_out(m)) {
    owl_message_free(m);
    return 0;
  }

  /* add it to the global list */
  owl_messagelist_append_element(owl_global_get_msglist(&g), m);
  /* add it to any necessary views; right now there's only the current view */
  owl_view_consider_message(owl_global_get_current_view(&g), m);

  if(owl_message_is_direction_in(m)) {
    /* let perl know about it*/
    owl_perlconfig_getmsg(m, 0, NULL);

    /* do we need to autoreply? */
    if (owl_global_is_zaway(&g) && !owl_message_get_attribute_value(m, "isauto")) {
      if (owl_message_is_type_zephyr(m)) {
        owl_zephyr_zaway(m);
      } else if (owl_message_is_type_aim(m)) {
        if (owl_message_is_private(m)) {
          owl_function_send_aimawymsg(owl_message_get_sender(m), owl_global_get_zaway_msg(&g));
        }
      }
    }

    /* ring the bell if it's a personal */
    if (!strcmp(owl_global_get_personalbell(&g), "on")) {
      if (!owl_message_is_loginout(m) &&
          !owl_message_is_mail(m) &&
          owl_message_is_personal(m)) {
        owl_function_beep();
      }
    } else if (!strcmp(owl_global_get_personalbell(&g), "off")) {
      /* do nothing */
    } else {
      f=owl_global_get_filter(&g, owl_global_get_personalbell(&g));
      if (f && owl_filter_message_match(f, m)) {
        owl_function_beep();
      }
    }

    /* if it matches the alert filter, do the alert action */
    f=owl_global_get_filter(&g, owl_global_get_alert_filter(&g));
    if (f && owl_filter_message_match(f, m)) {
      owl_function_command(owl_global_get_alert_action(&g));
    }

    /* if it's a zephyr login or logout, update the zbuddylist */
    if (owl_message_is_type_zephyr(m) && owl_message_is_loginout(m)) {
      if (owl_message_is_login(m)) {
        owl_zbuddylist_adduser(owl_global_get_zephyr_buddylist(&g), owl_message_get_sender(m));
      } else if (owl_message_is_logout(m)) {
        owl_zbuddylist_deluser(owl_global_get_zephyr_buddylist(&g), owl_message_get_sender(m));
      } else {
        owl_function_error("Internal error: received login notice that is neither login nor logout");
      }
    }

    /* check for burning ears message */
    /* this is an unsupported feature */
    if (owl_global_is_burningears(&g) && owl_message_is_burningears(m)) {
      char *buff;
      buff = owl_sprintf("@i(Burning ears message on class %s)", owl_message_get_class(m));
      owl_function_adminmsg(buff, "");
      owl_free(buff);
      owl_function_beep();
    }
  }

  /* log the message if we need to */
  owl_log_message(m);

  return 1;
}

void sig_handler(int sig, siginfo_t *si, void *data)
{
  if (sig==SIGWINCH) {
    /* we can't inturrupt a malloc here, so it just sets a flag
     * schedulding a resize for later
     */
    owl_function_resize();
  } else if (sig==SIGPIPE || sig==SIGCHLD) {
    /* Set a flag and some info that we got the sigpipe
     * so we can record that we got it and why... */
    owl_global_set_errsignal(&g, sig, si);
  } else if (sig==SIGTERM || sig==SIGHUP) {
    owl_function_quit();
  }

}

void usage()
{
  fprintf(stderr, "Owl version %s\n", OWL_VERSION_STRING);
  fprintf(stderr, "Usage: owl [-n] [-d] [-D] [-v] [-h] [-c <configfile>] [-s <confdir>] [-t <ttyname>]\n");
  fprintf(stderr, "  -n      don't load zephyr subscriptions\n");
  fprintf(stderr, "  -d      enable debugging\n");
  fprintf(stderr, "  -D      enable debugging and delete previous debug file\n");
  fprintf(stderr, "  -v      print the Owl version number and exit\n");
  fprintf(stderr, "  -h      print this help message\n");
  fprintf(stderr, "  -c      specify an alternate config file\n");
  fprintf(stderr, "  -s      specify an alternate config dir (default ~/.owl)\n");
  fprintf(stderr, "  -t      set the tty name\n");
}

#if OWL_STDERR_REDIR

/* Replaces stderr with a pipe so that we can read from it. 
 * Returns the fd of the pipe from which stderr can be read. */
int stderr_replace(void)
{
  int pipefds[2];
  if (0 != pipe(pipefds)) {
    perror("pipe");
    owl_function_debugmsg("stderr_replace: pipe FAILED\n");
    return -1;
  }
    owl_function_debugmsg("stderr_replace: pipe: %d,%d\n", pipefds[0], pipefds[1]);
  if (-1 == dup2(pipefds[1], 2 /*stderr*/)) {
    owl_function_debugmsg("stderr_replace: dup2 FAILED (%s)\n", strerror(errno));
    perror("dup2");
    return -1;
  }
  return pipefds[0];
}

/* Sends stderr (read from rfd) messages to the error console */
void stderr_redirect_handler(int handle, int rfd, int eventmask, void *data) 
{
  int navail, bread;
  char *buf;
  /*owl_function_debugmsg("stderr_redirect: called with rfd=%d\n", rfd);*/
  if (rfd<0) return;
  if (-1 == ioctl(rfd, FIONREAD, (void*)&navail)) {
    return;
  }
  /*owl_function_debugmsg("stderr_redirect: navail = %d\n", navail);*/
  if (navail<=0) return;
  if (navail>256) { navail = 256; }
  buf = owl_malloc(navail+1);
  bread = read(rfd, buf, navail);
  if (buf[navail-1] != '\0') {
    buf[navail] = '\0';
  }
  owl_function_error("Err: %s", buf);
  owl_free(buf);
}

#endif /* OWL_STDERR_REDIR */
