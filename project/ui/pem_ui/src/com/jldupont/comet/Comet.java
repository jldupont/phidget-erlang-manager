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


public class Comet {

	// Current 
	protected RequestBuilder  rb;
	protected CometCallback   cb;
	protected HandlerManager hm;
	protected String url;
	private CometFactory cf;
	
	/**
	 * @param rb
	 * @param cb
	 */
	public Comet(HandlerManager hm, CometFactory factory) {
		this.hm = hm;
		this.cf = factory;
		
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
		
		this.rb = this.cf.getRequestBuilder(RequestBuilder.GET, this.url);
		
	}
	
	/**
	 * 1) not started
	 */
	public void stop() {
		
	}
	
}//
