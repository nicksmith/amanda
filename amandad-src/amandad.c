/*
 * Amanda, The Advanced Maryland Automatic Network Disk Archiver
 * Copyright (c) 1991-1999 University of Maryland at College Park
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of U.M. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  U.M. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * U.M. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL U.M.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Authors: the Amanda Development Team.  Its members are listed in a
 * file named AUTHORS, in the root directory of this distribution.
 */

/*
 * $Id: amandad.c,v 1.18 2006/08/21 20:17:09 martinea Exp $
 *
 * handle client-host side of Amanda network communications, including
 * security checks, execution of the proper service, and acking the
 * master side
 */

/*#define	AMANDAD_DEBUG*/

#include "amanda.h"
#include "amandad.h"
#include "clock.h"
#include "event.h"
#include "amfeatures.h"
#include "packet.h"
#include "version.h"
#include "queue.h"
#include "security.h"
#include "stream.h"
#include "util.h"

#define	REP_TIMEOUT	(6*60*60)	/* secs for service to reply */
#define	ACK_TIMEOUT  	10		/* XXX should be configurable */
#define	MAX_REP_RETRIES	5

/*
 * These are the actions for entering the state machine
 */
typedef enum { A_START, A_RECVPKT, A_RECVREP, A_PENDING, A_FINISH, A_CONTINUE,
    A_SENDNAK, A_TIMEOUT } action_t;

/*
 * This is a state in the state machine.  It is a function pointer to
 * the function that actually implements the state.
 */
struct active_service;
typedef action_t (*state_t)(struct active_service *, action_t, pkt_t *);

/*
 * This structure describes an active running service.
 *
 * An active service is something running that we have received
 * a request for.  This structure holds info on that service, including
 * file descriptors for data, etc, as well as the security handle
 * for communications with the amanda server.
 */
struct active_service {
    char *cmd;				/* name of command we ran */
    char *arguments;			/* arguments we sent it */
    security_handle_t *security_handle;	/* remote server */
    state_t state;			/* how far this has progressed */
    pid_t pid;				/* pid of subprocess */
    int send_partial_reply;		/* send PREP packet */
    int reqfd;				/* pipe to write requests */
    int repfd;				/* pipe to read replies */
    event_handle_t *ev_repfd;		/* read event handle for repfd */
    event_handle_t *ev_reptimeout;	/* timeout for rep data */
    pkt_t rep_pkt;			/* rep packet we're sending out */
    char *repbuf;			/* buffer to read the rep into */
    size_t bufsize;			/* length of repbuf */
    size_t repbufsize;			/* length of repbuf */
    int repretry;			/* times we'll retry sending the rep */
    /*
     * General user streams to the process, and their equivalent
     * network streams.
     */
    struct datafd_handle {
	int fd_read;			/* pipe to child process */
	int fd_write;			/* pipe to child process */
	event_handle_t *ev_read;	/* it's read event handle */
	event_handle_t *ev_write;	/* it's write event handle */
	security_stream_t *netfd;	/* stream to amanda server */
	struct active_service *as;	/* pointer back to our enclosure */
    } data[DATA_FD_COUNT];
    char databuf[NETWORK_BLOCK_BYTES];	/* buffer to relay netfd data in */
    TAILQ_ENTRY(active_service) tq;	/* queue handle */
};

/* 
 * Here are the services that we allow.
 */
static struct services {
    char *name;
    int  active;
} services[] = {
    { "noop", 1 },
    { "sendsize", 1 },
    { "sendbackup", 1 },
    { "selfcheck", 1 },
    { "amindexd", 0 },
    { "amidxtaped", 0 }
};
#define	NSERVICES	(int)(sizeof(services) / sizeof(services[0]))

/*
 * Queue of outstanding requests that we are running.
 */
static struct {
    TAILQ_HEAD(, active_service) tailq;
    int qlength;
} serviceq = {
    TAILQ_HEAD_INITIALIZER(serviceq.tailq), 0
};

/*
 * Data for dbmalloc to check for memory leaks
 */
#ifdef USE_DBMALLOC
static struct {
    struct {
	unsigned long size, hist;
    } start, end;
} dbmalloc_info;
#endif

static int wait_30s = 1;
static int exit_on_qlength = 1;
static char *auth = NULL;

int main(int argc, char **argv);

static int allocstream(struct active_service *, int);
static void exit_check(void *);
static void protocol_accept(security_handle_t *, pkt_t *);
static void state_machine(struct active_service *, action_t, pkt_t *);

static action_t s_sendack(struct active_service *, action_t, pkt_t *);
static action_t s_repwait(struct active_service *, action_t, pkt_t *);
static action_t s_processrep(struct active_service *, action_t, pkt_t *);
static action_t s_sendrep(struct active_service *, action_t, pkt_t *);
static action_t s_ackwait(struct active_service *, action_t, pkt_t *);

static void repfd_recv(void *);
static void timeout_repfd(void *);
static void protocol_recv(void *, pkt_t *, security_status_t);
static void process_readnetfd(void *);
static void process_writenetfd(void *, void *, ssize_t);
static struct active_service *service_new(security_handle_t *,
    const char *, const char *);
static void service_delete(struct active_service *);
static int writebuf(struct active_service *, const void *, size_t);
static ssize_t do_sendpkt(security_handle_t *handle, pkt_t *pkt);

static void child_signal(int signal);

#ifdef AMANDAD_DEBUG
static const char *state2str(state_t);
static const char *action2str(action_t);
#endif

/*
 * Harvests defunct processes...
 */

static void
child_signal(
    int		signal)
{
    pid_t	rp;

    (void)signal;	/* Quite compiler warning */
    /*
     * Reap and child status and promptly ignore since we don't care...
     */
    do {
    	rp = waitpid(-1, NULL, WNOHANG);
    } while (rp > 0);
}

int
main(
    int		argc,
    char **	argv)
{
    int i, j;
    int have_services;
    int in, out;
    const security_driver_t *secdrv;
    int no_exit = 0;
    struct sigaction act, oact;
    char *pgm = "amandad";		/* in case argv[0] is not set */
#if defined(AMANDAD_DEBUG) && defined(USE_REUSEADDR)
    const int on = 1;
    int r;
#endif

    safe_fd(-1, 0);
    safe_cd();

    /*
     * When called via inetd, it is not uncommon to forget to put the
     * argv[0] value on the config line.  On some systems (e.g. Solaris)
     * this causes argv and/or argv[0] to be NULL, so we have to be
     * careful getting our name.
     */
    if ((argv == NULL) || (argv[0] == NULL)) {
	    pgm = "amandad";		/* in case argv[0] is not set */
    } else {
	    pgm = basename(argv[0]);	/* Strip of leading path get debug name */
    }
    set_pname(pgm);
    dbopen(DBG_SUBDIR_AMANDAD);

    if(argv == NULL) {
	error("argv == NULL\n");
	/*NOTREACHED*/
    }

    /* Don't die when child closes pipe */
    signal(SIGPIPE, SIG_IGN);

    /* Tell me when a child exits or dies... */
    act.sa_handler = child_signal;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if(sigaction(SIGCHLD, &act, &oact) != 0) {
	error("error setting SIGCHLD handler: %s", strerror(errno));
	/*NOTREACHED*/
    }

#ifdef USE_DBMALLOC
    dbmalloc_info.start.size = malloc_inuse(&dbmalloc_info.start.hist);
#endif

    erroutput_type = (ERR_INTERACTIVE|ERR_SYSLOG);

#ifdef FORCE_USERID
    /* we'd rather not run as root */
    if (geteuid() == 0) {
	if(client_uid == (uid_t) -1) {
	    error("error [cannot find user %s in passwd file]\n", CLIENT_LOGIN);
	    /*NOTREACHED*/
	}
	initgroups(CLIENT_LOGIN, client_gid);
	setgid(client_gid);
	setegid(client_gid);
	seteuid(client_uid);
    }
#endif	/* FORCE_USERID */

    /*
     * ad-hoc argument parsing
     *
     * We accept	-auth=[authentication type]
     *			-no-exit
#ifdef AMANDAD_DEBUG
     *			-tcp=[port]
     *			-udp=[port]
#endif
     * We also add a list of services that amandad can launch
     */
    secdrv = NULL;
    in = 0; out = 1;		/* default to stdin/stdout */
    have_services = 0;
    for (i = 1; i < argc; i++) {
	/*
	 * accept -krb4 as an alias for -auth=krb4 (for compatibility)
	 */
	if (strcmp(argv[i], "-krb4") == 0) {
	    argv[i] = "-auth=krb4";
	    /* FALLTHROUGH */
	    auth = "krb4";
	}

	/*
	 * Get a driver for a security type specified after -auth=
	 */
	else if (strncmp(argv[i], "-auth=", strlen("-auth=")) == 0) {
	    argv[i] += strlen("-auth=");
	    secdrv = security_getdriver(argv[i]);
	    auth = argv[i];
	    if (secdrv == NULL) {
		error("no driver for security type '%s'\n", argv[i]);
                /*NOTREACHED*/
	    }
	    continue;
	}

	/*
	 * If -no-exit is specified, always run even after requests have
	 * been satisfied.
	 */
	else if (strcmp(argv[i], "-no-exit") == 0) {
	    no_exit = 1;
	    continue;
	}

#ifdef AMANDAD_DEBUG
	/*
	 * Allow us to directly bind to a udp port for debugging.
	 * This may only apply to some security types.
	 */
	else if (strncmp(argv[i], "-udp=", strlen("-udp=")) == 0) {
	    struct sockaddr_in sin;

	    argv[i] += strlen("-udp=");
	    in = out = socket(AF_INET, SOCK_DGRAM, 0);
	    if (in < 0) {
		error("can't create dgram socket: %s\n", strerror(errno));
		/*NOTREACHED*/
	    }
#ifdef USE_REUSEADDR
	    r = setsockopt(in, SOL_SOCKET, SO_REUSEADDR,
		(void *)&on, (socklen_t)sizeof(on));
	    if (r < 0) {
		dbprintf(("%s: amandad: setsockopt(SO_REUSEADDR) failed: %s\n",
			  debug_prefix(NULL),
			  strerror(errno)));
	    }
#endif

	    sin.sin_family = (sa_family_t)AF_INET;
	    sin.sin_addr.s_addr = INADDR_ANY;
	    sin.sin_port = (in_port_t)htons((in_port_t)atoi(argv[i]));
	    if (bind(in, (struct sockaddr *)&sin, (socklen_t)sizeof(sin)) < 0) {
		error("can't bind to port %d: %s\n", atoi(argv[i]),
		    strerror(errno));
		/*NOTREACHED*/
	    }
	}
	/*
	 * Ditto for tcp ports.
	 */
	else if (strncmp(argv[i], "-tcp=", strlen("-tcp=")) == 0) {
	    struct sockaddr_in sin;
	    int sock;
	    socklen_t n;

	    argv[i] += strlen("-tcp=");
	    sock = socket(AF_INET, SOCK_STREAM, 0);
	    if (sock < 0) {
		error("can't create tcp socket: %s\n", strerror(errno));
		/*NOTREACHED*/
	    }
	    n = 1;
#ifdef USE_REUSEADDR
	    r = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
		(void *)&on, (socklen_t)sizeof(on));
	    if (r < 0) {
		dbprintf(("%s: amandad: setsockopt(SO_REUSEADDR) failed: %s\n",
			  debug_prefix(NULL),
			  strerror(errno)));
	    }
#endif
	    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
		(void *)&n, (socklen_t)sizeof(n));
	    sin.sin_family = (sa_family_t)AF_INET;
	    sin.sin_addr.s_addr = INADDR_ANY;
	    sin.sin_port = (in_port_t)htons((in_port_t)atoi(argv[i]));
	    if (bind(sock, (struct sockaddr *)&sin, (socklen_t)sizeof(sin)) < 0) {
		error("can't bind to port %d: %s\n", atoi(argv[i]),
		    strerror(errno));
		/*NOTREACHED*/
	    }
	    listen(sock, 10);
	    n = (socklen_t)sizeof(sin);
	    in = out = accept(sock, (struct sockaddr *)&sin, &n);
	}
#endif
	/*
	 * It must be a service name
	 */
	else {
	    /* clear all services */
	    if(!have_services) {
		for (j = 0; j < (int)NSERVICES; j++)
		    services[j].active = 0;
	    }
	    have_services = 1;

	    if(strcmp(argv[i],"amdump") == 0) {
		services[0].active = 1;
		services[1].active = 1;
		services[2].active = 1;
		services[3].active = 1;
	    }
	    else {
		for (j = 0; j < (int)NSERVICES; j++)
		    if (strcmp(services[j].name, argv[i]) == 0)
			break;
		if (j == (int)NSERVICES) {
		    dbprintf(("%s: %s: invalid service\n",
			      debug_prefix_time(NULL), argv[i]));
		    exit(1);
		}
		services[j].active = 1;
	    }
	}
    }

    /*
     * If no security type specified, use BSD
     */
    if (secdrv == NULL) {
	secdrv = security_getdriver("BSD");
	auth = "bsd";
	if (secdrv == NULL) {
	    error("no driver for default security type 'BSD'\n");
	    /*NOTREACHED*/
	}
    }

    if(strcasecmp(auth, "rsh") == 0 ||
       strcasecmp(auth, "ssh") == 0 ||
       strcasecmp(auth, "bsdtcp") == 0) {
	wait_30s = 0;
	exit_on_qlength = 1;
    }

    /* initialize */

    startclock();

    dbprintf(("%s: version %s\n", get_pname(), version()));
    for (i = 0; version_info[i] != NULL; i++) {
	dbprintf(("%s: %s", debug_prefix(NULL), version_info[i]));
    }

    if (! (argc >= 1 && argv != NULL && argv[0] != NULL)) {
	dbprintf(("%s: WARNING: argv[0] not defined: check inetd.conf\n",
		  debug_prefix(NULL)));
    }

    /*
     * Schedule to call protocol_accept() when new security handles
     * are created on stdin.
     */
    security_accept(secdrv, in, out, protocol_accept);

    /*
     * Schedule an event that will try to exit every 30 seconds if there
     * are no requests outstanding.
     */
    if(wait_30s)
	(void)event_register((event_id_t)30, EV_TIME, exit_check, &no_exit);

    /*
     * Call event_loop() with an arg of 0, telling it to block until all
     * events are completed.
     */
    event_loop(0);

    close(in);
    close(out);
    dbclose();
    return(0);
}

/*
 * This runs periodically and checks to see if we have any active services
 * still running.  If we don't, then we quit.
 */
static void
exit_check(
    void *	cookie)
{
    int no_exit;

    assert(cookie != NULL);
    no_exit = *(int *)cookie;

    /*
     * If things are still running, then don't exit.
     */
    if (serviceq.qlength > 0)
	return;

    /*
     * If the caller asked us to never exit, then we're done
     */
    if (no_exit)
	return;

#ifdef USE_DBMALLOC
    dbmalloc_info.end.size = malloc_inuse(&dbmalloc_info.end.hist);

    if (dbmalloc_info.start.size != dbmalloc_info.end.size) {
	malloc_list(dbfd(), dbmalloc_info.start.hist,
	    dbmalloc_info.end.hist);
    }
#endif

    dbclose();
    exit(0);
}

/*
 * Handles new incoming protocol handles.  This is a callback for
 * security_accept(), which gets called when new handles are detected.
 */
static void
protocol_accept(
    security_handle_t *	handle,
    pkt_t *		pkt)
{
    pkt_t pkt_out;
    struct active_service *as;
    char *pktbody, *tok, *service, *arguments;
    char *service_path = NULL;
    int i;

    pkt_out.body = NULL;

    /*
     * If handle is NULL, then the connection is closed.
     */
    if(handle == NULL) {
	return;
    }

    /*
     * If pkt is NULL, then there was a problem with the new connection.
     */
    if (pkt == NULL) {
	dbprintf(("%s: accept error: %s\n",
	    debug_prefix_time(NULL), security_geterror(handle)));
	pkt_init(&pkt_out, P_NAK, "ERROR %s\n", security_geterror(handle));
	do_sendpkt(handle, &pkt_out);
	amfree(pkt_out.body);
	security_close(handle);
	return;
    }

    dbprintf(("%s: accept recv %s pkt:\n<<<<<\n%s>>>>>\n",
	debug_prefix_time(NULL), pkt_type2str(pkt->type), pkt->body));

    /*
     * If this is not a REQ packet, just forget about it.
     */
    if (pkt->type != P_REQ) {
	dbprintf(("%s: received unexpected %s packet:\n<<<<<\n%s>>>>>\n\n",
	    debug_prefix_time(NULL), pkt_type2str(pkt->type), pkt->body));
	security_close(handle);
	return;
    }

    pktbody = service = arguments = NULL;
    as = NULL;

    /*
     * Parse out the service and arguments
     */

    pktbody = stralloc(pkt->body);

    tok = strtok(pktbody, " ");
    if (tok == NULL)
	goto badreq;
    if (strcmp(tok, "SERVICE") != 0)
	goto badreq;

    tok = strtok(NULL, " \n");
    if (tok == NULL)
	goto badreq;
    service = stralloc(tok);

    /* we call everything else 'arguments' */
    tok = strtok(NULL, "");
    if (tok == NULL)
	goto badreq;
    arguments = stralloc(tok);

    /* see if it's one we allow */
    for (i = 0; i < (int)NSERVICES; i++)
	if (services[i].active == 1 && strcmp(services[i].name, service) == 0)
	    break;
    if (i == (int)NSERVICES) {
	dbprintf(("%s: %s: invalid service\n",
	    debug_prefix_time(NULL), service));
	pkt_init(&pkt_out, P_NAK, "ERROR %s: invalid service\n", service);
	goto send_pkt_out;
    }

    service_path = vstralloc(libexecdir, "/", service, versionsuffix(), NULL);
    if (access(service_path, X_OK) < 0) {
	dbprintf(("%s: can't execute %s: %s\n",
	    debug_prefix_time(NULL), service_path, strerror(errno)));
	    pkt_init(&pkt_out, P_NAK,
		     "ERROR execute access to \"%s\" denied\n",
		     service_path);
	goto send_pkt_out;
    }

    /* see if its already running */
    for (as = TAILQ_FIRST(&serviceq.tailq); as != NULL;
	as = TAILQ_NEXT(as, tq)) {
	    if (strcmp(as->cmd, service_path) == 0 &&
		strcmp(as->arguments, arguments) == 0) {
		    dbprintf(("%s: %s %s: already running, acking req\n",
			debug_prefix_time(NULL), service, arguments));
		    pkt_init(&pkt_out, P_ACK, "");
		    goto send_pkt_out_no_delete;
	    }
    }

    /*
     * create a new service instance, and send the arguments down
     * the request pipe.
     */
    dbprintf(("%s: creating new service: %s\n%s\n",
	debug_prefix_time(NULL), service, arguments));
    as = service_new(handle, service_path, arguments);
    if (writebuf(as, arguments, strlen(arguments)) < 0) {
	const char *errmsg = strerror(errno);
	dbprintf(("%s: error sending arguments to %s: %s\n",
	    debug_prefix_time(NULL), service, errmsg));
	pkt_init(&pkt_out, P_NAK, "ERROR error writing arguments to %s: %s\n",
	    service, errmsg);
	goto send_pkt_out;
    }
    aclose(as->reqfd);

    amfree(pktbody);
    amfree(service);
    amfree(service_path);
    amfree(arguments);

    /*
     * Move to the sendack state, and start up the state
     * machine.
     */
    as->state = s_sendack;
    state_machine(as, A_START, NULL);
    return;

badreq:
    pkt_init(&pkt_out, P_NAK, "ERROR invalid REQ\n");
    dbprintf(("%s: received invalid %s packet:\n<<<<<\n%s>>>>>\n\n",
	debug_prefix_time(NULL), pkt_type2str(pkt->type), pkt->body));

send_pkt_out:
    if(as)
	service_delete(as);
send_pkt_out_no_delete:
    amfree(pktbody);
    amfree(service_path);
    amfree(service);
    amfree(arguments);
    do_sendpkt(handle, &pkt_out);
    security_close(handle);
    amfree(pkt_out.body);
}

/*
 * Handles incoming protocol packets.  Routes responses to the proper
 * running service.
 */
static void
state_machine(
    struct active_service *	as,
    action_t			action,
    pkt_t *			pkt)
{
    action_t retaction;
    state_t curstate;
    pkt_t nak;

#ifdef AMANDAD_DEBUG
    dbprintf(("%s: state_machine: %p entering\n",
	debug_prefix_time(NULL), as));
#endif
    for (;;) {
	curstate = as->state;
#ifdef AMANDAD_DEBUG
	dbprintf(("%s: state_machine: %p curstate=%s action=%s\n",
	    debug_prefix_time(NULL), as,
	    state2str(curstate), action2str(action)));
#endif
	retaction = (*curstate)(as, action, pkt);
#ifdef AMANDAD_DEBUG
	dbprintf(("%s: state_machine: %p curstate=%s returned %s (nextstate=%s)\n",
	    debug_prefix_time(NULL),
	    as, state2str(curstate), action2str(retaction),
	    state2str(as->state)));
#endif

	switch (retaction) {
	/*
	 * State has queued up and is now blocking on input.
	 */
	case A_PENDING:
#ifdef AMANDAD_DEBUG
	    dbprintf(("%s: state_machine: %p leaving (A_PENDING)\n",
		debug_prefix_time(NULL), as));
#endif
	    return;

	/*
	 * service has switched states.  Loop.
	 */
	case A_CONTINUE:
	    break;

	/*
	 * state has determined that the packet it received was bogus.
	 * Send a nak, and return.
	 */
	case A_SENDNAK:
	    dbprintf(("%s: received unexpected %s packet\n",
		debug_prefix_time(NULL), pkt_type2str(pkt->type)));
	    dbprintf(("<<<<<\n%s----\n\n", pkt->body));
	    pkt_init(&nak, P_NAK, "ERROR unexpected packet type %s\n",
		pkt_type2str(pkt->type));
	    do_sendpkt(as->security_handle, &nak);
	    amfree(nak.body);
#ifdef AMANDAD_DEBUG
	    dbprintf(("%s: state_machine: %p leaving (A_SENDNAK)\n",
		debug_prefix_time(NULL), as));
#endif
	    return;

	/*
	 * Service is done.  Remove it and finish.
	 */
	case A_FINISH:
	    service_delete(as);
#ifdef AMANDAD_DEBUG
	    dbprintf(("%s: state_machine: %p leaving (A_FINISH)\n",
		debug_prefix_time(NULL), as));
#endif
	    return;

	default:
	    assert(0);
	    break;
	}
    }
    /*NOTREACHED*/
}

/*
 * This state just sends an ack.  After that, we move to the repwait
 * state to wait for REP data to arrive from the subprocess.
 */
static action_t
s_sendack(
    struct active_service *	as,
    action_t			action,
    pkt_t *			pkt)
{
    pkt_t ack;

    (void)action;	/* Quiet unused parameter warning */
    (void)pkt;		/* Quiet unused parameter warning */

    pkt_init(&ack, P_ACK, "");
    if (do_sendpkt(as->security_handle, &ack) < 0) {
	dbprintf(("%s: error sending ACK: %s\n",
	    debug_prefix_time(NULL), security_geterror(as->security_handle)));
	amfree(ack.body);
	return (A_FINISH);
    }
    amfree(ack.body);

    /*
     * move to the repwait state
     * Setup a listener for data on the reply fd, but also
     * listen for packets over the wire, as the server may
     * poll us if we take a long time.
     * Setup a timeout that will fire if it takes too long to
     * receive rep data.
     */
    as->state = s_repwait;
    as->ev_repfd = event_register((event_id_t)as->repfd, EV_READFD, repfd_recv, as);
    as->ev_reptimeout = event_register(REP_TIMEOUT, EV_TIME,
	timeout_repfd, as);
    security_recvpkt(as->security_handle, protocol_recv, as, -1);
    return (A_PENDING);
}

/*
 * This is the repwait state.  We have responded to the initial REQ with
 * an ACK, and we are now waiting for the process we spawned to pass us 
 * data to send in a REP.
 */
static action_t
s_repwait(
    struct active_service *	as,
    action_t			action,
    pkt_t *			pkt)
{
    ssize_t n;
    char *repbuf_temp;

    /*
     * We normally shouldn't receive any packets while waiting
     * for our REP data, but in some cases we do.
     */
    if (action == A_RECVPKT) {
	assert(pkt != NULL);
	/*
	 * Another req for something that's running.  Just send an ACK
	 * and go back and wait for more data.
	 */
	if (pkt->type == P_REQ) {
	    dbprintf(("%s: received dup P_REQ packet, ACKing it\n",
		debug_prefix_time(NULL)));
	    amfree(as->rep_pkt.body);
	    pkt_init(&as->rep_pkt, P_ACK, "");
	    do_sendpkt(as->security_handle, &as->rep_pkt);
	    return (A_PENDING);
	}
	/* something unexpected.  Nak it */
	return (A_SENDNAK);
    }

    if (action == A_TIMEOUT) {
	amfree(as->rep_pkt.body);
	pkt_init(&as->rep_pkt, P_NAK, "ERROR timeout on reply pipe\n");
	dbprintf(("%s: %s timed out waiting for REP data\n",
	    debug_prefix_time(NULL), as->cmd));
	do_sendpkt(as->security_handle, &as->rep_pkt);
	return (A_FINISH);
    }

    assert(action == A_RECVREP);
    if(as->bufsize == 0) {
	as->bufsize = NETWORK_BLOCK_BYTES;
	as->repbuf = alloc(as->bufsize);
    }

    do {
	n = read(as->repfd, as->repbuf + as->repbufsize,
		 as->bufsize - as->repbufsize - 1);
    } while ((n < 0) && ((errno == EINTR) || (errno == EAGAIN)));
    if (n < 0) {
	const char *errstr = strerror(errno);
	dbprintf(("%s: read error on reply pipe: %s\n",
		  debug_prefix_time(NULL), errstr));
	amfree(as->rep_pkt.body);
	pkt_init(&as->rep_pkt, P_NAK, "ERROR read error on reply pipe: %s\n",
		 errstr);
	do_sendpkt(as->security_handle, &as->rep_pkt);
	return (A_FINISH);
    }
    /*
     * If we got some data, go back and wait for more, or EOF.  Nul terminate
     * the buffer first.
     */
    as->repbuf[n + as->repbufsize] = '\0';
    if (n > 0) {
	as->repbufsize += n;
	if(as->repbufsize >= (as->bufsize - 1)) {
	    as->bufsize *= 2;
	    repbuf_temp = alloc(as->bufsize);
	    memcpy(repbuf_temp, as->repbuf, as->repbufsize + 1);
	    amfree(as->repbuf);
	    as->repbuf = repbuf_temp;
	}
	else if(as->send_partial_reply) {
	    amfree(as->rep_pkt.body);
	    pkt_init(&as->rep_pkt, P_PREP, "%s", as->repbuf);
	    do_sendpkt(as->security_handle, &as->rep_pkt);
	    amfree(as->rep_pkt.body);
	    pkt_init(&as->rep_pkt, P_REP, "");
	}
 
	return (A_PENDING);
    }

    /*
     * If we got 0, then we hit EOF.  Process the data and release
     * the timeout.
     */
    assert(n == 0);

    assert(as->ev_repfd != NULL);
    event_release(as->ev_repfd);
    as->ev_repfd = NULL;

    assert(as->ev_reptimeout != NULL);
    event_release(as->ev_reptimeout);
    as->ev_reptimeout = NULL;

    as->state = s_processrep;
    aclose(as->repfd);
    return (A_CONTINUE);
}

/*
 * After we have read in all of the rep data, we process it and send
 * it out as a REP packet.
 */
static action_t
s_processrep(
    struct active_service *	as,
    action_t			action,
    pkt_t *			pkt)
{
    char *tok, *repbuf;

    (void)action;	/* Quiet unused parameter warning */
    (void)pkt;		/* Quiet unused parameter warning */

    /*
     * Copy the rep lines into the outgoing packet.
     *
     * If this line is a CONNECT, translate it
     * Format is "CONNECT <tag> <handle> <tag> <handle> etc...
     * Example:
     *
     *  CONNECT DATA 4 MESG 5 INDEX 6
     *
     * The tags are arbitrary.  The handles are in the DATA_FD pool.
     * We need to map these to security streams and pass them back
     * to the amanda server.  If the handle is -1, then we don't map.
     */
    repbuf = stralloc(as->repbuf);
    amfree(as->rep_pkt.body);
    pkt_init(&as->rep_pkt, P_REP, "");
    tok = strtok(repbuf, " ");
    if (tok == NULL)
	goto error;
    if (strcmp(tok, "CONNECT") == 0) {
	char *line, *nextbuf;

	/* Save the entire line */
	line = strtok(NULL, "\n");
	/* Save the buf following the line */
	nextbuf = strtok(NULL, "");

	if (line == NULL || nextbuf == NULL)
	    goto error;

	pkt_cat(&as->rep_pkt, "CONNECT");

	/* loop over the id/handle pairs */
	for (;;) {
	    /* id */
	    tok = strtok(line, " ");
	    line = NULL;	/* keep working from line */
	    if (tok == NULL)
		break;
	    pkt_cat(&as->rep_pkt, " %s", tok);

	    /* handle */
	    tok = strtok(NULL, " \n");
	    if (tok == NULL)
		goto error;
	    /* convert the handle into something the server can process */
	    pkt_cat(&as->rep_pkt, " %d", allocstream(as, atoi(tok)));
	}
	pkt_cat(&as->rep_pkt, "\n%s", nextbuf);
    } else {
error:
	pkt_cat(&as->rep_pkt, "%s", as->repbuf);
    }

    /*
     * We've setup our REP packet in as->rep_pkt.  Now move to the transmission
     * state.
     */
    as->state = s_sendrep;
    as->repretry = MAX_REP_RETRIES;
    amfree(repbuf);
    return (A_CONTINUE);
}

/*
 * This is the state where we send the REP we just collected from our child.
 */
static action_t
s_sendrep(
    struct active_service *	as,
    action_t			action,
    pkt_t *			pkt)
{
    (void)action;	/* Quiet unused parameter warning */
    (void)pkt;		/* Quiet unused parameter warning */

    /*
     * Transmit it and move to the ack state.
     */
    do_sendpkt(as->security_handle, &as->rep_pkt);
    security_recvpkt(as->security_handle, protocol_recv, as, ACK_TIMEOUT);
    as->state = s_ackwait;
    return (A_PENDING);
}

/*
 * This is the state in which we wait for the server to ACK the REP
 * we just sent it.
 */
static action_t
s_ackwait(
    struct active_service *	as,
    action_t			action,
    pkt_t *			pkt)
{
    struct datafd_handle *dh;
    int npipes;

    /*
     * If we got a timeout, try again, but eventually give up.
     */
    if (action == A_TIMEOUT) {
	if (--as->repretry > 0) {
	    as->state = s_sendrep;
	    return (A_CONTINUE);
	}
	dbprintf(("%s: timeout waiting for ACK for our REP\n",
	    debug_prefix_time(NULL)));
	return (A_FINISH);
    }
#ifdef AMANDAD_DEBUG
    dbprintf(("%s: received ACK, now opening streams\n",
	debug_prefix_time(NULL)));
#endif

    assert(action == A_RECVPKT);

    if (pkt->type == P_REQ) {
	dbprintf(("%s: received dup P_REQ packet, resending REP\n",
		  debug_prefix_time(NULL)));
	as->state = s_sendrep;
	return (A_CONTINUE);
    }

    if (pkt->type != P_ACK)
	return (A_SENDNAK);

    /*
     * Got the ack, now open the pipes
     */
    for (dh = &as->data[0]; dh < &as->data[DATA_FD_COUNT]; dh++) {
	if (dh->netfd == NULL)
	    continue;
	if (security_stream_accept(dh->netfd) < 0) {
	    dbprintf(("%s: stream %d accept failed: %s\n",
		debug_prefix_time(NULL),
		dh - &as->data[0], security_geterror(as->security_handle)));
	    security_stream_close(dh->netfd);
	    dh->netfd = NULL;
	}
	/* setup an event for reads from it */
	dh->ev_read = event_register((event_id_t)dh->fd_read, EV_READFD,
				     process_readnetfd, dh);

	security_stream_read(dh->netfd, process_writenetfd, dh);

    }

    /*
     * Pipes are open, so auth them.  Count them at the same time.
     */
    for (npipes = 0, dh = &as->data[0]; dh < &as->data[DATA_FD_COUNT]; dh++) {
	if (dh->netfd == NULL)
	    continue;
	if (security_stream_auth(dh->netfd) < 0) {
	    security_stream_close(dh->netfd);
	    dh->netfd = NULL;
	    event_release(dh->ev_read);
	    event_release(dh->ev_write);
	    dh->ev_read = NULL;
	    dh->ev_write = NULL;
	} else {
	    npipes++;
	}
    }

    /*
     * If no pipes are open, then we're done.  Otherwise, just start running.
     * The event handlers on all of the pipes will take it from here.
     */
#ifdef AMANDAD_DEBUG
    dbprintf(("%s: at end of s_ackwait, npipes is %d\n",
	debug_prefix_time(NULL), npipes));
#endif
    if (npipes == 0)
	return (A_FINISH);
    else {
	security_close(as->security_handle);
	as->security_handle = NULL;
	return (A_PENDING);
    }
}

/*
 * Called when a repfd has received data
 */
static void
repfd_recv(
    void *	cookie)
{
    struct active_service *as = cookie;

    assert(as != NULL);
    assert(as->ev_repfd != NULL);

    state_machine(as, A_RECVREP, NULL);
}

/*
 * Called when a repfd has timed out
 */
static void
timeout_repfd(
    void *	cookie)
{
    struct active_service *as = cookie;

    assert(as != NULL);
    assert(as->ev_reptimeout != NULL);

    state_machine(as, A_TIMEOUT, NULL);
}

/*
 * Called when a handle has received data
 */
static void
protocol_recv(
    void *		cookie,
    pkt_t *		pkt,
    security_status_t	status)
{
    struct active_service *as = cookie;

    assert(as != NULL);

    switch (status) {
    case S_OK:
	dbprintf(("%s: received %s pkt:\n<<<<<\n%s>>>>>\n",
	    debug_prefix_time(NULL), pkt_type2str(pkt->type), pkt->body));
	state_machine(as, A_RECVPKT, pkt);
	break;
    case S_TIMEOUT:
	dbprintf(("%s: timeout\n", debug_prefix_time(NULL)));
	state_machine(as, A_TIMEOUT, NULL);
	break;
    case S_ERROR:
	dbprintf(("%s: receive error: %s\n",
	    debug_prefix_time(NULL), security_geterror(as->security_handle)));
	break;
    }
}

/*
 * This is a generic relay function that just reads data from one of
 * the process's pipes and passes it up the equivalent security_stream_t
 */
static void
process_readnetfd(
    void *	cookie)
{
    pkt_t nak;
    struct datafd_handle *dh = cookie;
    struct active_service *as = dh->as;
    ssize_t n;

    nak.body = NULL;

    do {
	n = read(dh->fd_read, as->databuf, SIZEOF(as->databuf));
    } while ((n < 0) && ((errno == EINTR) || (errno == EAGAIN)));

    /*
     * Process has died.
     */
    if (n < 0) {
	pkt_init(&nak, P_NAK, "A ERROR data descriptor %d broken: %s\n",
	    dh->fd_read, strerror(errno));
	goto sendnak;
    }
    /*
     * Process has closed the pipe.  Just remove this event handler.
     * If all pipes are closed, shut down this service.
     */
    if (n == 0) {
	event_release(dh->ev_read);
	dh->ev_read = NULL;
	if(dh->ev_write == NULL) {
	    security_stream_close(dh->netfd);
	    dh->netfd = NULL;
	}
	for (dh = &as->data[0]; dh < &as->data[DATA_FD_COUNT]; dh++) {
	    if (dh->netfd != NULL)
		return;
	}
	service_delete(as);
	return;
    }
    if (security_stream_write(dh->netfd, as->databuf, (size_t)n) < 0) {
	/* stream has croaked */
	pkt_init(&nak, P_NAK, "ERROR write error on stream %d: %s\n",
	    security_stream_id(dh->netfd),
	    security_stream_geterror(dh->netfd));
	goto sendnak;
    }
    return;

sendnak:
    do_sendpkt(as->security_handle, &nak);
    service_delete(as);
    amfree(nak.body);
}

/*
 * This is a generic relay function that just read data from one of
 * the security_stream_t and passes it up the equivalent process's pipes
 */
static void
process_writenetfd(
    void *	cookie,
    void *	buf,
    ssize_t	size)
{
    struct datafd_handle *dh;

    assert(cookie != NULL);
    dh = cookie;

    if (dh->fd_write <= 0) {
	dbprintf(("%s: process_writenetfd: dh->fd_write <= 0\n",
	    debug_prefix_time(NULL)));
    } else if (size > 0) {
	fullwrite(dh->fd_write, buf, (size_t)size);
	security_stream_read(dh->netfd, process_writenetfd, dh);
    }
    else {
	aclose(dh->fd_write);
    }
}


/*
 * Convert a local stream handle (DATA_FD...) into something that
 * can be sent to the amanda server.
 *
 * Returns a number that should be sent to the server in the REP packet.
 */
static int
allocstream(
    struct active_service *	as,
    int				handle)
{
    struct datafd_handle *dh;

    /* if the handle is -1, then we don't bother */
    if (handle < 0)
	return (-1);

    /* make sure the handle's kosher */
    if (handle < DATA_FD_OFFSET || handle >= DATA_FD_OFFSET + DATA_FD_COUNT)
	return (-1);

    /* get a pointer into our handle array */
    dh = &as->data[handle - DATA_FD_OFFSET];

    /* make sure we're not already using the net handle */
    if (dh->netfd != NULL)
	return (-1);

    /* allocate a stream from the security layer and return */
    dh->netfd = security_stream_server(as->security_handle);
    if (dh->netfd == NULL) {
	dbprintf(("%s: couldn't open stream to server: %s\n",
	    debug_prefix_time(NULL), security_geterror(as->security_handle)));
	return (-1);
    }

    /*
     * convert the stream into a numeric id that can be sent to the
     * remote end.
     */
    return (security_stream_id(dh->netfd));
}

/*
 * Create a new service instance
 */
static struct active_service *
service_new(
    security_handle_t *	security_handle,
    const char *	cmd,
    const char *	arguments)
{
    int i;
    int data_read[DATA_FD_COUNT + 1][2];
    int data_write[DATA_FD_COUNT + 1][2];
    struct active_service *as;
    pid_t pid;
    int newfd;

    assert(security_handle != NULL);
    assert(cmd != NULL);
    assert(arguments != NULL);

    /* a plethora of pipes */
    for (i = 0; i < DATA_FD_COUNT + 1; i++) {
	if (pipe(data_read[i]) < 0) {
	    error("pipe: %s\n", strerror(errno));
	    /*NOTREACHED*/
	}
	if (pipe(data_write[i]) < 0) {
	    error("pipe: %s\n", strerror(errno));
	    /*NOTREACHED*/
	}
    }

    switch(pid = fork()) {
    case -1:
	error("could not fork service %s: %s\n", cmd, strerror(errno));
	/*NOTREACHED*/
    default:
	/*
	 * The parent.  Close the far ends of our pipes and return.
	 */
	as = alloc(SIZEOF(*as));
	as->cmd = stralloc(cmd);
	as->arguments = stralloc(arguments);
	as->security_handle = security_handle;
	as->state = NULL;
	as->pid = pid;
	as->send_partial_reply = 0;
	if(strcmp(cmd+(strlen(cmd)-8), "sendsize") == 0) {
	    g_option_t *g_options;
	    char *option_str, *p;

	    option_str = stralloc(as->arguments+8);
	    p = strchr(option_str,'\n');
	    if(p) *p = '\0';

	    g_options = parse_g_options(option_str, 1);
	    if(am_has_feature(g_options->features, fe_partial_estimate)) {
		as->send_partial_reply = 1;
	    }
	    free_g_options(g_options);
	    amfree(option_str);
	}

	/* write to the request pipe */
	aclose(data_read[0][0]);
	as->reqfd = data_read[0][1];

	/*
	 * read from the reply pipe
	 */
	as->repfd = data_write[0][0];
	aclose(data_write[0][1]);
	as->ev_repfd = NULL;
	as->repbuf = NULL;
	as->repbufsize = 0;
	as->bufsize = 0;
	as->repretry = 0;
	as->rep_pkt.body = NULL;

	/*
	 * read from the rest of the general-use pipes
	 * (netfds are opened as the client requests them)
	 */
	for (i = 0; i < DATA_FD_COUNT; i++) {
	    aclose(data_read[i + 1][1]);
	    aclose(data_write[i + 1][0]);
	    as->data[i].fd_read = data_read[i + 1][0];
	    as->data[i].fd_write = data_write[i + 1][1];
	    as->data[i].ev_read = NULL;
	    as->data[i].ev_write = NULL;
	    as->data[i].netfd = NULL;
	    as->data[i].as = as;
	}

	/* add it to the service queue */
	/* increment the active service count */
	TAILQ_INSERT_TAIL(&serviceq.tailq, as, tq);
	serviceq.qlength++;

	return (as);
    case 0:
	/*
	 * The child.  Put our pipes in their advertised locations
	 * and start up.
	 */
#ifdef FORCE_USERID
	seteuid((uid_t)0);
	setuid(client_uid);
#endif

	/*
	 * The data stream is stdin in the new process
	 */
        if (dup2(data_read[0][0], 0) < 0) {
	    error("dup %d to %d failed: %s\n", data_read[0][0], 0,
		strerror(errno));
	    /*NOTREACHED*/
	}
	aclose(data_read[0][0]);
	aclose(data_read[0][1]);

	/*
	 * The reply stream is stdout
	 */
        if (dup2(data_write[0][1], 1) < 0) {
	    error("dup %d to %d failed: %s\n", data_write[0][1], 1,
		strerror(errno));
	}
        aclose(data_write[0][0]);
        aclose(data_write[0][1]);

	/*
	 *  Make sure they are not open in the range DATA_FD_OFFSET to
	 *      DATA_FD_OFFSET + DATA_FD_COUNT*2 - 1
	 */
	for (i = 0; i < DATA_FD_COUNT; i++) {
	    while(data_read[i + 1][1] >= DATA_FD_OFFSET &&
		  data_read[i + 1][1] <= DATA_FD_OFFSET + DATA_FD_COUNT*2 - 1) {
		newfd = dup(data_read[i + 1][1]);
		if(newfd == -1)
		    error("Can't dup out off DATA_FD range");
		data_read[i + 1][1] = newfd;
	    }
	    while(data_write[i + 1][0] >= DATA_FD_OFFSET &&
		  data_write[i + 1][0] <= DATA_FD_OFFSET + DATA_FD_COUNT*2 - 1) {
		newfd = dup(data_write[i + 1][0]);
		if(newfd == -1)
		    error("Can't dup out off DATA_FD range");
		data_write[i + 1][0] = newfd;
	    }
	}
	for (i = 0; i < DATA_FD_COUNT; i++)
	    close(DATA_FD_OFFSET + i);

	/*
	 * The rest start at the offset defined in amandad.h, and continue
	 * through the internal defined.
	 */
	for (i = 0; i < DATA_FD_COUNT; i++) {
	    if (dup2(data_read[i + 1][1], i*2 + DATA_FD_OFFSET) < 0) {
		error("dup %d to %d failed: %s\n", data_read[i + 1][1],
		    i + DATA_FD_OFFSET, strerror(errno));
	    }
	    aclose(data_read[i + 1][0]);
	    aclose(data_read[i + 1][1]);

	    if (dup2(data_write[i + 1][0], i*2 + 1 + DATA_FD_OFFSET) < 0) {
		error("dup %d to %d failed: %s\n", data_write[i + 1][0],
		    i + DATA_FD_OFFSET, strerror(errno));
	    }
	    aclose(data_write[i + 1][0]);
	    aclose(data_write[i + 1][1]);
	}

	/* close all unneeded fd */
	safe_fd(DATA_FD_OFFSET, DATA_FD_COUNT*2);
	close(2);

	execle(cmd, cmd, "amandad", auth, (char *)NULL, safe_env());
	error("could not exec service %s: %s\n", cmd, strerror(errno));
	/*NOTREACHED*/
    }
    return NULL;
}

/*
 * Unallocate a service instance
 */
static void
service_delete(
    struct active_service *	as)
{
    int i;
    struct datafd_handle *dh;

#ifdef AMANDAD_DEBUG
	dbprintf(("%s: closing service: %s\n",
	    debug_prefix_time(NULL), (as->cmd)?as->cmd:"??UNKONWN??"));
#endif

    assert(as != NULL);

    assert(as->cmd != NULL);
    amfree(as->cmd);

    assert(as->arguments != NULL);
    amfree(as->arguments);

    if (as->reqfd != -1)
	aclose(as->reqfd);
    if (as->repfd != -1)
	aclose(as->repfd);

    if (as->ev_repfd != NULL)
	event_release(as->ev_repfd);
    if (as->ev_reptimeout != NULL)
	event_release(as->ev_reptimeout);

    for (i = 0; i < DATA_FD_COUNT; i++) {
	dh = &as->data[i];

	aclose(dh->fd_read);
	aclose(dh->fd_write);

	if (dh->netfd != NULL)
	    security_stream_close(dh->netfd);

	if (dh->ev_read != NULL)
	    event_release(dh->ev_read);
	if (dh->ev_write != NULL)
	    event_release(dh->ev_write);
    }

    if (as->security_handle != NULL)
	security_close(as->security_handle);

    assert(as->pid > 0);
    kill(as->pid, SIGTERM);
    waitpid(as->pid, NULL, WNOHANG);

    TAILQ_REMOVE(&serviceq.tailq, as, tq);
    assert(serviceq.qlength > 0);
    serviceq.qlength--;

    amfree(as->cmd);
    amfree(as->arguments);
    amfree(as->repbuf);
    amfree(as->rep_pkt.body);
    amfree(as);

    if(exit_on_qlength == 0 && serviceq.qlength == 0) {
	dbclose();
	exit(0);
    }
}

/*
 * Like 'fullwrite', but does the work in a child process so pipelines
 * do not hang.
 */
static int
writebuf(
    struct active_service *	as,
    const void *		bufp,
    size_t			size)
{
    pid_t pid;
    ssize_t    writesize;

    switch (pid=fork()) {
    case -1:
	break;

    default:
	waitpid(pid, NULL, WNOHANG);
	return 0;			/* this is the parent */

    case 0: 				/* this is the child */
	close(as->repfd);
	writesize = fullwrite(as->reqfd, bufp, size);
	exit(writesize != (ssize_t)size);
	/* NOTREACHED */
    }
    return -1;
}

static ssize_t
do_sendpkt(
    security_handle_t *	handle,
    pkt_t *		pkt)
{
    dbprintf(("%s: sending %s pkt:\n<<<<<\n%s>>>>>\n",
	debug_prefix_time(NULL), pkt_type2str(pkt->type), pkt->body));
    return security_sendpkt(handle, pkt);
}

#ifdef AMANDAD_DEBUG
/*
 * Convert a state into a string
 */
static const char *
state2str(
    state_t	state)
{
    static const struct {
	state_t state;
	const char str[13];
    } states[] = {
#define	X(state)	{ state, stringize(state) }
	X(s_sendack),
	X(s_repwait),
	X(s_processrep),
	X(s_sendrep),
	X(s_ackwait),
#undef X
    };
    int i;

    for (i = 0; i < (int)(sizeof(states) / sizeof(states[0])); i++)
	if (state == states[i].state)
	    return (states[i].str);
    return ("INVALID STATE");
}

/*
 * Convert an action into a string
 */
static const char *
action2str(
    action_t	action)
{
    static const struct {
	action_t action;
	const char str[12];
    } actions[] = {
#define	X(action)	{ action, stringize(action) }
	X(A_START),
	X(A_RECVPKT),
	X(A_RECVREP),
	X(A_PENDING),
	X(A_FINISH),
	X(A_CONTINUE),
	X(A_SENDNAK),
	X(A_TIMEOUT),
#undef X
    };
    int i;

    for (i = 0; i < (int)(sizeof(actions) / sizeof(actions[0])); i++)
	if (action == actions[i].action)
	    return (actions[i].str);
    return ("UNKNOWN ACTION");
}
#endif	/* AMANDAD_DEBUG */