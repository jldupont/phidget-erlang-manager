/**
 * @author Jean-Lou Dupont
 */
package com.jldupont.comet;

import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;


public class CometFactory {

	public RequestBuilder getRequestBuilder(RequestBuilder.Method method, String url) {
		
		return new RequestBuilder(method, url); 
	}
	
	public RequestCallback getRequestCallback() {
		return new CometCallback();
	}
	
}//
