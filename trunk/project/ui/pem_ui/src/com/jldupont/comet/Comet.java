/**
 * COMET
 * 
 * @author Jean-Lou Dupont
 * 
 * Parameters:
 *   - URL
 *   - timeout
 *   - callbacks
 *     - onError
 *     - onResponse
 *     
 * Notes:
 *  1) Errors signaled:
 *     - connection
 *     
 *  Timeouts are not signaled as they are considered "normal".
 * 
 */
package com.jldupont.comet;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.jldupont.system.Logger;


public class Comet implements RequestCallback {

	// Current 
	protected Request         rq;
	protected CometCallback   cb;
	protected HandlerManager hm;
	protected String url;
	protected CometFactory cf;
	
	// "raise" if an exception occured
	// that is irrecoverable
	boolean exception = false;
	
	/**
	 * COMET connection manager
	 * 
	 * @param hm
	 * @param cf
	 */
	public Comet(HandlerManager hm, CometFactory cf) {
		this.hm = hm;
		this.cf = cf;
		
		this.rq  = null;
		this.url = null;
		this.cb = null;
	}
	
	/**
	 * Sets the URL to use for the connection
	 * 
	 * @param url
	 */
	public void setUrl(String url) {
		this.url = url;
	}
	
	/**
	 * Can only be called once
	 * 
	 * 1) parameter errors
	 * 2) already started
	 */
	public void start() throws RuntimeException {

		if (null==this.url){
			throw new RuntimeException("url not specified");
		}
		
		// request already pending?
		if (null!=this.rq) {
			if (this.rq.isPending())
				return;
			
			// try limiting leaks
			this.rq = null;
		}
		
		do_request();
	}
	
	/**
	 * 1) not started
	 */
	public void stop() {
		
		if (null!=this.rq) {
			this.rq.cancel();
		}
		
	}

	protected void do_request() {
		
		Logger.logInfo("COMET: do_request");
		
		// Use Factory
		RequestBuilder rb = this.cf.getRequestBuilder(RequestBuilder.GET, this.url);
		
		/*
		 *  Subscribe to the events in order to
		 *  re-establish the connection upon error/completion 
		 */
		
			try {
				this.rq = rb.sendRequest(null, this);
			} catch (RequestException e) {
				Logger.logError("COMET request exception: "+ e);
				this.rq = null;
				this.exception = true;
			}	
	}
	
	// RequestCallback interface methods
	
	@Override
	public void onError(Request request, Throwable exception) {
		
		Logger.logInfo("COMET: onError");
		
		// restart
		do_request();
		
	}

	@Override
	public void onResponseReceived(Request request, Response response) {
		Logger.logInfo("COMET: onResponseReceived");
		
		do_request();
	}
	
}//
