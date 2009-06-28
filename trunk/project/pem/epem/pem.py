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
    erlcmd = "erl -noshell -pa ./ebin -s pem_admin start %s"
    
    codes = {   0:{m:"cannot start"            },
                1:{m:"stop sent"               },
                2:{m:"unknown command"         },
                3:{m:"cannot stop"             },
                4:{m:"communication error"     },
                5:{m:"daemon present"          },
                6:{m:"no daemon"               },
                10:{m:"unknown error"          }
             }
    
    def erl(self, cmd):
        """Executes the Erlang administration program for PEM
        """
        proc = subprocess.Popen(erlcmd % cmd, shell=True)
        return proc.wait()
    
    def cmd_start(self):
        """
        """
        print "start!"
        return 0
        
    def cmd_stop(self):
        """
        """
        print "stop!"
        return 0
    

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
        # DISPATCH COMMAND
        ret = getattr(cmd, fun)()
    except:
        parser.error("invalid command")
        sys.exit(1)
        
    sys.exit(ret)
    

if __name__=="__main__":
    main()
