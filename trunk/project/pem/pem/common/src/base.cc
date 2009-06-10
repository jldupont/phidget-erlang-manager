/**
 * @file   base.cc
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 */
#include "base.h"

drvBase::drvBase() {

	ph = new PktHandler();
	mh = new MsgHandler(ph);
	eq = queue_create(-1);

	startReadThread();
}//

drvBase::~drvBase() {

	delete mh;
	delete ph;
	free(eq);
}//

void
drvBase::startReadThread(void) {

	drvReadThreadParams *p = new drvReadThreadParams();
	p->eq = eq;
	p->mh = mh;

	pthread_create( &readThread, NULL, &drvBase::readThreadFun, (void*) p);
}//

event *
drvBase::createEvent(eventType type) {
	event *e = (event *)malloc(sizeof(event));
	if (NULL!=e)
		e->type = type;

	return e;
}//


void *
drvBase::readThreadFun( void *params ) {

	DBGLOG(LOG_INFO, "drvBase::readThreadFun: BEGIN");

	drvReadThreadParams *p = (drvReadThreadParams *) params;

	int result;
	queue *q = p->eq;
	Msg *m;
	MsgHandler *mh = p->mh;

	//prepare an "EVENT_READ_ERROR" just in case
	event *ere = (event *) malloc(sizeof(event));
	ere->type = EVENT_READ_ERROR;

	event *me;

	while(1) {

		result=mh->rx( &m );

		//error?
		if (result) {
			queue_put( q, (void *) ere );
			break;
		}

		//queue the rx message for the main thread
		me = createEvent( EVENT_MSG );
		me->m = m;
		queue_put(q, (void *) me);

	}//while

	DBGLOG(LOG_INFO, "drvBase::readThreadFun: END");
}//


/**
 * @return -1 no message
 * @return  1 FAILURE
 * @return  0 SUCCESS
 */
int
drvBase::waitMsg(Msg **m, int usec_timeout) {

	event *e;
	int result;

	if (-1==usec_timeout) {
		result = queue_wait( eq );
	} else {
		result = queue_wait_timer( eq, usec_timeout );
	}

	//error
	if (result) {
		return 1;
	}

	 e = (event *) queue_get_nb( eq );
	 if (NULL==e) {
		 *m = NULL;
		 return -1;
	 }

	 //extract the 'real' message
	 *m = e->m;

	 return 0;
}//


int
drvBase::waitMsg(Msg **m) {

	return waitMsg(m, -1);
}//
