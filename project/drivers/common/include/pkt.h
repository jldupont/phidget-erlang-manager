/**
 * @file pkt.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */

#ifndef PKT_H_
#define PKT_H_

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
		int sz;
		char *buf;

	public:
		Pkt();
		~Pkt();

		unsigned char *getBuf(void);
		unsigned char *getBuf(int size);
	};

	/**
	 * Packet Handler
	 */
	class PktHandler: public PktBase {

	protected:
		int ifd;
		int ofd;

	public:
		PktHandler();
		PktHandler(int ifd, int ofd);
		~PktHandler();

		int rx(Pkt **p);
		int send(Pkt *p);
	};


#endif /* PKT_H_ */
