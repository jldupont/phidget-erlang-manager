/**
 * @author Jean-Lou Dupont
 */
package com.jldupont.comet;

import com.google.gwt.event.shared.GwtEvent;

public class CometError extends GwtEvent<CometErrorHandler> {

	/**
	 * Handler type.
	 */
	private static Type<CometErrorHandler> TYPE;
	
	@Override
	protected void dispatch(CometErrorHandler handler) {
		// TODO Auto-generated method stub
		
	}

	@SuppressWarnings("unchecked")
	@Override
	public com.google.gwt.event.shared.GwtEvent.Type<CometErrorHandler> getAssociatedType() {
		return (Type) TYPE;
	}
	
}
