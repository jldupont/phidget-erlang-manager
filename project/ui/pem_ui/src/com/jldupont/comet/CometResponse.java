/**
 * @author Jean-Lou Dupont
 */
package com.jldupont.comet;

import com.google.gwt.event.shared.GwtEvent;

public class CometResponse extends GwtEvent<CometResponseHandler> {

	/**
	 * Handler type.
	 */
	private static Type<CometResponseHandler> TYPE;
	
	@Override
	protected void dispatch(CometResponseHandler handler) {
		// TODO Auto-generated method stub
		
	}

	@SuppressWarnings("unchecked")
	@Override
	public com.google.gwt.event.shared.GwtEvent.Type<CometResponseHandler> getAssociatedType() {
		return (Type) TYPE;
	}
	
}
