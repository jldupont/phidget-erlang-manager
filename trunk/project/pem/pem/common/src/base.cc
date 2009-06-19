/**
 * @file   base.cc
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 */
#include "base.h"

drvBase::drvBase() {

	//DEBUG_LOG(LOG_INFO, "drvBase::drvBase()");

	ph = new PktHandler();
	mh = new MsgHandler(ph);
	eq = queue_create(-1);
}//

drvBase::~drvBase() {

	//DEBUG_LOG(LOG_INFO, "drvBase::~drvBase()");

	delete mh;
	delete ph;
	free(eq);
}//

void
drvBase::start(void) {

	init();
	startReadThread();

}//

void
drvBase::startReadThread(void) {

	DEBUG_LOG(LOG_INFO, "drvBase::startReadThread()");

	drvReadThreadParams *p = new drvReadThreadParams();
	p->eq = eq;
	p->mh = mh;
	p->drv = this;

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

	drvReadThreadParams *p = (drvReadThreadParams *) params;

	queue      *q   = p->eq;
	MsgHandler *mh  = p->mh;
	drvBase    *drv = p->drv;

	DBGLOG(LOG_INFO, "drvBase::readThreadFun: BEGIN, q[%x]", q);

	//prepare an "EVENT_READ_ERROR" just in case
	event *ere = (event *) malloc(sizeof(event));
	ere->type = EVENT_READ_ERROR;

	Msg   *m;
	event *me;

	while(1) {

		int result=mh->rx( &m );

		//error?
		if (result) {
			DBGLOG(LOG_INFO, "drvBase::readThreadFun: got MsgHandler::rx ERROR");
			queue_put( q, (void *) ere );
			break;
		}

		//queue the rx message for the main thread
		me = createEvent( EVENT_MSG );
		me->m = m;
		queue_put(q, (void *) me);

	}//while

	DBGLOG(LOG_INFO, "drvBase::readThreadFun: END, q[%x]", q);
}//


/**
 * @return -1 no message
 * @return  1 FAILURE
 * @return  0 SUCCESS
 */
int
drvBase::waitMsg(Msg **m, int usec_timeout) {

	*m = NULL;

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
		 return -1;
	 }

	 // READ error?
	 eventType type = e->type;
	 if (EVENT_READ_ERROR==type) {
		 DBGLOG(LOG_INFO, "drvBase::waitMsg: got EVENT_READ_ERROR");
		 free(e);
		 return 1;
	 }

	 // anything other...
	 if (EVENT_MSG!=type) {
		 free(e);
		 return 1;
	 }

	 //extract the 'real' message
	 *m = e->m;

	 free(e);

	 return 0;
}//


int
drvBase::waitMsg(Msg **m) {

	return waitMsg(m, -1);
}//
