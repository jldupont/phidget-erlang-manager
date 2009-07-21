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
    erldaemoncmd = "erl -noshell -detached -boot start_sasl -config elog.config -pa ./ebin /usr/share/pem/bin -s pem_app"
    
    codes = {   0: {"m":"cannot start",         "start":False,   "stop":True     },
                1: {"m":"stop sent",            "start":False,   "stop":True     },
                2: {"m":"unknown command",      "start":False,   "stop":False    },
                3: {"m":"cannot stop",          "start":True,    "stop":False    },
                4: {"m":"communication error",  "start":True,    "stop":False    },
                5: {"m":"daemon present",       "start":False,   "stop":True     },
                6: {"m":"no daemon",            "start":True,    "stop":False    },
                10:{"m":"unknown error",        "start":False,   "stop":False    }
             }
    
    def erladmin(self, cmd):
        """Executes the Erlang administration program for PEM
        """
        proc = subprocess.Popen(self.erladmincmd % cmd, shell=True)
        return proc.wait()
    
    def cmd_start(self):
        """ Starts the daemon (if possible)
        """
        ret = self.erladmin("start")
        action = self.lookup("start", ret)
        return ret
        
    def cmd_stop(self):
        """ Stops the daemon (if possible)
        """
        ret = self.erladmin("stop")
        action = self.lookup("start", ret)
        return ret
    
    def lookup(self, context, retcode):
        """
        """
        state=self.codes.get(retcode, 10)
        return state[context]
        
    

def main():
    """ Main entry point
    """
    parser=OptionParser(usage)

    parser.add_option("-v", "--verbose", action="store_true",  dest="verbose")
    parser.add_option("-q", "--quiet",   action="store_false", dest="verbose")

    (options, args) = parser.parse_args()
    
    if len(args) != 1:
        parser.error("incorrect number of arguments")
    
    fun = "cmd_%s" % args[0]
    cmd = Command()
    
    try:
        func = getattr(cmd, fun)
    except:
        parser.error("invalid command")
        sys.exit(1)

    try:
        ret = func()
    except Exception,e:
        print "Error in command [%s]" % (str(e))
        sys.exit(1)
        
    print ret
    sys.exit(ret)
    

if __name__=="__main__":
    main()
