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

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;

public class Comet {

	protected RequestBuilder  rb;
	protected CometCallback   cb;
	
	/**
	 * @param rb
	 * @param cb
	 */
	public Comet(RequestBuilder rb, CometCallback cb) {
		this.rb = rb;
		this.cb = cb;
		
		this.rb = null;

	}
	
	/**
	 * 1) parameter errors
	 * 2) already started
	 */
	public void start(String url) throws RuntimeException {
		
		if (null!=this.rb) {
			throw new RuntimeException("request already started");
		}
		
	}
	
	/**
	 * 1) not started
	 */
	public void stop() {
		
	}
	
}//
