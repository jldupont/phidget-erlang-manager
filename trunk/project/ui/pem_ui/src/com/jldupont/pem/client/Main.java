package com.jldupont.pem.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.RootPanel;
import com.jldupont.comet.Comet;
import com.jldupont.comet.CometFactory;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class Main implements EntryPoint {
	private Button clickMeButton;
	public void onModuleLoad() {
		RootPanel rootPanel = RootPanel.get();

		HandlerManager hm = new HandlerManager(null);
		CometFactory   cf = new CometFactory();
		
		Comet c = new Comet(hm, cf);
		
		c.setUrl("/status");
		
		c.start();
	}
	
	
}//
