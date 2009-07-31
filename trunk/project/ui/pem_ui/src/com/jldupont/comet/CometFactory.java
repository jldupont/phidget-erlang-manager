/**
 * @author Jean-Lou Dupont
 */
package com.jldupont.comet;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;

/**
 * Factory for easier testing 
 * 
 * @author Jean-Lou Dupont
 *
 */
public class CometFactory {

	public RequestBuilder getRequestBuilder(RequestBuilder.Method method, String url) {
		
		return new RequestBuilder(method, url); 
	}
	
	
}//
