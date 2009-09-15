#!/usr/bin/python
### BEGIN INIT INFO
# Provides:          epem
# Required-Start:    
# Required-Stop:     
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start daemon at boot time
# Description:       Enable service provided by daemon.
### END INIT INFO
# NOTE: DO NOT USE /usr/bin/env since this might break /etc/init.d script
"""
    epem daemon management
    
    @author: Jean-Lou Dupont
"""
import sys
import subprocess
from optparse import OptionParser

###< CUSTOMIZE BELOW
bridge="erl +d -pa ebin -noshell -sname epem_py -s epem_bridge cmd %s"
usage = """epem [-q] command"""
environ = {"HOME":"/root", "PATH":"/sbin:/usr/sbin:/bin:/usr/bin" }
###>


try:
    import json
except:
    try:
        import simplejson as json
    except:
        print "This script requires module 'json' or 'simplejson'\n"
        sys.exit(1)
        


def exec_cmd(cmd, args):
    """
    Send a command through the bridge
    
    @returns: (stdout, stderr, exit_code)
    """
    try:
        proc = subprocess.Popen(cmd % args, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=environ)
        proc.wait()
        exit_code=proc.returncode
        (stdout, stderr)=proc.communicate()
                
    except Exception,e:
        #print "Exception: "+str(e)
        exit_code = 127 # command not found
        stderr = "'erl' command not found... Erlang is required."
        stdout = ""
        
    return (stdout, stderr, exit_code)

def do_cmd(cmds):
    """
    Executes a command and returns the result as a Python object
    
    @returns (exit_code, Result)
    """
    (resp, errmsg, exit_code)= exec_cmd(bridge, cmds)
    
    if exit_code==127:
        return (exit_code, errmsg)
    
    #print "resp: %s\n" % str(resp)
    
    try:
        rj=json.loads(resp)
        reply=(exit_code, rj)
    except Exception,e:
        #print "! Exception <%s>\n" % str(e)
        reply=(1, "error: maybe the installation is broken?")
        
    return reply


def main():
    """ Main entry point
    """
    parser=OptionParser(usage)
    parser.add_option("-q", "--quiet",  action="store_true",  dest="quiet", help="disable output to stdout")
    
    (options, args) = parser.parse_args()
    
    if len(args) < 1:
        parser.error("! incorrect number of arguments")
      
    cmds=""
    for cmd in args:
        cmds=cmds + " " + cmd  
    
    (exit_code, message)=do_cmd(cmds)
    if not options.quiet:
        print "> %s\n" % str( message )
            
    sys.exit(exit_code)
    
# execute
main()

