/**
 * @author Jean-Lou Dupont
 */
package com.jldupont.comet;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestTimeoutException;
import com.google.gwt.http.client.Response;



public class CometCallback implements RequestCallback{

	private HandlerManager hm;

	/**
	 * Default Constructor
	 */
	CometCallback(HandlerManager hm) {
		this.hm = hm;
	}
	
	
	public void onError(Request request, java.lang.Throwable exception) {
		
		// If timeout, no big deal: just restore connection
		if (exception instanceof RequestTimeoutException) {
	
		}
		
	}
	
	public void onResponseReceived(Request request, Response response) {
		
	}
	
	
}//
