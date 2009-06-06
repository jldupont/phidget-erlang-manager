/**
 * @file pkt.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */

#ifndef PKT_H_
#define PKT_H_

#include <ei.h>

	/**
	 * Packet Base
	 */
	class PktBase {
	private:
		static const char *errors[];

	protected:
		int last_error;

	public:
		/**
		 * Returns the pointer
		 * to the human readable
		 * message corresponding
		 * to the last error
		 */
		const char *strerror(void);

		/**
		 * Returns the error code
		 * of the last error
		 */
		int error(void);
	};


	/**
	 * Packet class
	 */
	class Pkt: public PktBase {

	private:
		static const int DSZ = 128;

	protected:

		//RX packet type
		int sz;
		char *buf;
		int len;

		//TX packet type
		ei_x_buff tbuf;

	public:

		/**
		 * Creates a RX packet
		 */
		Pkt();

		/**
		 * Creates a TX packet
		 * with the a fixed
		 * field for the packet length
		 */
		Pkt(int lenSz);

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
		 */
		void setLength(int len);

		/**
		 * Returns the length
		 * of the packet
		 */
		int getLength(void);
	};

	/**
	 * Packet Handler
	 */
	class PktHandler: public PktBase {

	protected:
		int ifd;
		int ofd;
		int sz;

	public:
		PktHandler();
		PktHandler(int size);
		PktHandler(int ifd, int ofd);
		PktHandler(int size, int ifd, int ofd);
		~PktHandler();

		int rx(Pkt **p);
		int tx(Pkt *p);

	protected:
		int rx_exact(Pkt **p, int len);
		int tx_exact(Pkt *p, int len);
	};


#endif /* PKT_H_ */
