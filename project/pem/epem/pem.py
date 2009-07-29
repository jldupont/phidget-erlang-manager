#!/usr/bin/env python
"""
    PEM - daemon control 
    
    @author: Jean-Lou Dupont
"""
import os
import sys
import subprocess
from optparse import OptionParser

usage = \
"""
        pem [options] Cmd

         Cmd="start"  : start daemon
         Cmd="stop"   : stop daemon
"""


    
class Command(object):
    """ Communicates Command to the PEM daemon
    """
    erladmincmd  = "erl -noshell            -pa ./ebin /usr/share/pem/bin -s pem_admin start %s"
    erldaemoncmd = "erl -noshell -sname pem -pa ./ebin /usr/share/pem/bin -detached -boot start_sasl -config elog.config -sname pem -run pem_app start %s"
    
    codes = {   0: {"m":"can start",            "start":True,    "stop":False    },
                1: {"m":"stop sent",            "start":False,   "stop":True     },
                2: {"m":"unknown command",      "start":False,   "stop":False    },
                3: {"m":"cannot stop",          "start":True,    "stop":False    },
                4: {"m":"communication error",  "start":True,    "stop":False    },
                5: {"m":"daemon present",       "start":False,   "stop":True     },
                6: {"m":"no daemon",            "start":True,    "stop":False    },
                10:{"m":"unknown error",        "start":False,   "stop":False    }
             }
    
    def __init__(self, verbose=False):
        self.verbose=verbose
        self.dsn=None
    
    def erladmin(self, cmd):
        """Executes the Erlang administration program for PEM
        """
        proc = subprocess.Popen(self.erladmincmd % cmd, shell=True)
        return proc.wait()
    
    def erldaemon(self):
        cmd = self.erldaemoncmd % self.dsn
        proc = subprocess.Popen(cmd, shell=True)
        return proc
    
    def cmd_start(self):
        """ Starts the daemon (if possible)
        """
        ret = self.erladmin("start")
        proceed = self.lookup("start", ret)
        print ret
        if proceed:
            self.cprint("""pem: attempting to start daemon""")
            
            proc = self.erldaemon()
            
            not_terminated = (proc.returncode == None)
            if not_terminated:
                self.cprint( """pem: daemon started""" )
                
        else:
            self.cprint("""pem: daemon cannot be started/already present""")
            
        
    def cmd_stop(self):
        """ Stops the daemon (if possible)
        """
        ret = self.erladmin("stop")
        proceed = self.lookup("stop", ret)
        if proceed:
            self.cprint("""pem: daemon stop issued""")
        else:
            self.cprint("""pem: daemon not present/cannot be stopped""")
    
    def lookup(self, context, retcode):
        state=self.codes.get(retcode, 10)
        return state[context]

    def cprint(self, msg):
        if self.verbose:
            print msg
            
    

def main():
    """ Main entry point
    """
    parser=OptionParser(usage)

    parser.add_option("-v", "--verbose",  action="store_true",  dest="verbose")
    parser.add_option("-q", "--quiet",    action="store_false", dest="verbose")
    
    #database related
    parser.add_option("-d", "--dsn",      action="store",       dest="dsn")

    (options, args) = parser.parse_args()
    
    if len(args) != 1:
        parser.error("incorrect number of arguments")
      
    context = args[0]
    fun = "cmd_%s" % context
    cmd = Command(verbose=options.verbose)
    
    # start command? then we need database details
    if context == "start":
        cmd.dsn = getattr(options, "dsn", None)
        
        if cmd.dsn is None:
            parser.error("missing ODBC DSN option")
            
        
    try:
        func = getattr(cmd, fun)
    except:
        parser.error("invalid command")
        sys.exit(1)

    try:
        ret = func()
    except Exception,e:
        if not options.quiet:
            print "Error in command [%s]" % (str(e))
        sys.exit(1)
        
    
    sys.exit(ret)
    

if __name__=="__main__":
    main()
