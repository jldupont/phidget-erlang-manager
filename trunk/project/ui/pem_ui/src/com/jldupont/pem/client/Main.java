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
import com.google.gwt.user.client.ui.HorizontalSplitPanel;
import com.google.gwt.user.client.ui.Frame;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class Main implements EntryPoint {
	private Button clickMeButton;
	public void onModuleLoad() {
		RootPanel rootPanel = RootPanel.get();
		{
			HorizontalSplitPanel horizontalSplitPanel = new HorizontalSplitPanel();
			rootPanel.add(horizontalSplitPanel, 18, 22);
			horizontalSplitPanel.setSize("414px", "246px");
			{
				Frame frame = new Frame("/status?timeout=2");
				horizontalSplitPanel.setLeftWidget(frame);
				frame.setSize("100%", "100%");
			}
			{
				Frame frame = new Frame("/status?timeout=5");
				horizontalSplitPanel.setRightWidget(frame);
				frame.setSize("100%", "100%");
			}
		}

		
		HandlerManager hm = new HandlerManager(null);
		CometFactory   cf = new CometFactory();
		
		Comet c = new Comet(hm, cf);
		
		c.setUrl("/status");
		
		c.start();
		
		
	}
}//
