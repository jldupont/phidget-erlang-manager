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
import com.google.gwt.http.client.RequestBuilder;


public class Comet implements CometErrorHandler<CometErrorHandler>{

	// Current 
	protected RequestBuilder  rb;
	protected CometCallback   cb;
	protected HandlerManager hm;
	protected String url;
	protected CometFactory cf;
	
	/**
	 * COMET connection manager
	 * 
	 * @param hm
	 * @param cf
	 */
	public Comet(HandlerManager hm, CometFactory cf) {
		this.hm = hm;
		this.cf = cf;
		
		this.url = null;
		this.rb = null;
		this.cb = null;
		this.rb = null;
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
		
		if (null!=this.rb) {
			throw new RuntimeException("request already started");
		}
		
		if (null==this.url){
			throw new RuntimeException("url not specified");
		}
		
		/*
		 *  Subscribe to the events in order to
		 *  re-establish the connection upon error/completion 
		 */
		this.hm.addHandler(CometEvent<CometError> this, CometEventHandler<CometErrorHandler> this);
		
		this.rb = this.cf.getRequestBuilder(RequestBuilder.GET, this.url);
		
	}
	
	/**
	 * 1) not started
	 */
	public void stop() {
		
	}
	
}//
