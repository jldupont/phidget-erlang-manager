/**
 * @author Jean-Lou Dupont
 */
package com.jldupont.comet;

import com.google.gwt.event.shared.GwtEvent;

public class CometEvent<T> extends GwtEvent<CometEventHandler<T>> {

	/**
	 * Handler type.
	 */
	private static Type<CometEventHandler<?>> TYPE;
	
	@Override
	protected void dispatch(CometEventHandler<T> handler) {
		// TODO Auto-generated method stub
		
	}

	@SuppressWarnings("unchecked")
	@Override
	public com.google.gwt.event.shared.GwtEvent.Type<CometEventHandler<T>> getAssociatedType() {
		return (Type) TYPE;
	}
	
}
