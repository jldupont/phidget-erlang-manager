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
    erladmincmd  = "erl -noshell -pa ./ebin /usr/share/pem/bin -s pem_admin start %s"
    erldaemoncmd = "erl -noshell -pa ./ebin /usr/share/pem/bin -detached -boot start_sasl -config elog.config -s pem_app start %s %s %s"
    #erldaemoncmd = "erl -noshell -detached -boot start_sasl -pa ./ebin /usr/share/pem/bin -s pem_app"
    
    codes = {   0: {"m":"cannot start",         "start":False,   "stop":False    },
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
        self.database=None
        self.username=None
        self.password=None
    
    def erladmin(self, cmd):
        """Executes the Erlang administration program for PEM
        """
        proc = subprocess.Popen(self.erladmincmd % cmd, shell=True)
        return proc.wait()
    
    def erldaemon(self):
        cmd = self.erldaemoncmd % (self.database, self.username, self.password)
        proc = subprocess.Popen(cmd, shell=True)
        return proc
    
    def cmd_start(self):
        """ Starts the daemon (if possible)
        """
        ret = self.erladmin("start")
        proceed = self.lookup("start", ret)
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
    parser.add_option("-d", "--database", action="store",       dest="database")
    parser.add_option("-u", "--username", action="store",       dest="username")
    parser.add_option("-p", "--password", action="store",       dest="password")

    (options, args) = parser.parse_args()
    
    if len(args) != 1:
        parser.error("incorrect number of arguments")
      
    context = args[0]
    fun = "cmd_%s" % context
    cmd = Command(verbose=options.verbose)
    
    # start command? then we need database details
    if context == "start":
        cmd.database = getattr(options, "database", None)
        cmd.username = getattr(options, "username", None)
        cmd.password = getattr(options, "password", None)
        
        if cmd.database is None:
            parser.error("missing MySQL database name")
            
        if cmd.username is None:
            parser.error("missing username for MySQL database access")
            
        if cmd.password is None:
            parser.error("missing password for MySQL database access")
        
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
