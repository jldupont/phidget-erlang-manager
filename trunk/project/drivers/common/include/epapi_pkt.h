/**
 * @file pkt.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */

#ifndef PKT_H_
#define PKT_H_

//dev support
#ifndef EPAPI_H_
	#include "epapi.h"
#endif


	/**
	 * Packet class
	 */
	class Pkt: public epapiBase {


	private:
		static const int DSZ = 128;

	protected:

		//RX packet type
		int sz;
		char *buf;
		int len;

		//TX packet type
		ei_x_buff *tbuf;

	public:

		/**
		 * Creates a RX packet
		 */
		Pkt();

		/**
		 * Destroys either RX or TX
		 * packet types
		 */
		~Pkt();

		/**
		 * Gets the pointer to
		 * the internal buffer
		 */
		unsigned char *getBuf(void);

		/**
		 * Gets the pointer to the internal
		 * buffer and reallocs, if necessary,
		 * to 'size'
		 */
		unsigned char *getBuf(int size);

		/**
		 * Returns the Erlang specific
		 * TX buffer
		 */
		ei_x_buff *getTxBuf(void);


		/**
		 * Sets the packet length
		 * ie. not the buffer length
		 *
		 * This method really only
		 * applies to RX packet type and usually
		 * used by the PktHandler.
		 *
		 */
		void setLength(int len);

		/**
		 * Returns the length
		 * of the packet
		 *
		 * This method really only
		 * applies to RX packet type
		 */
		int getLength(void);
	};

	/**
	 * Packet Handler
	 */
	class PktHandler: public epapiBase {

	protected:
		int ifd;
		int ofd;


	public:
		PktHandler();

		/**
		 * @param ifd input file descriptor
		 * @param ofd output file descriptor
		 */
		PktHandler(int ifd, int ofd);

		~PktHandler();

		/**
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int rx(Pkt **p);

		/**
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int tx(Pkt *p);

	protected:
		/**
		 * @return >0  LEN read
		 * @return <=0 ERROR, check errno
		 */
		int rx_exact(Pkt **p, int len);

		/**
		 * @return >0   LEN written
		 * @return <=0  ERROR, check errno
		 */
		int tx_exact(char *buf, int len);
	};


#endif /* PKT_H_ */
