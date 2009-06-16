"""
    SCONS helper functions
    
    @author: Jean-Lou Dupont
"""
import os
import sys
from string import Template


def read_version():
    file = open('./VERSION')
    version = file.read()
    file.close()
    return version

def replace_params(path_src, path_dest, params):
    """
    Replace the parameters in the target path
    """
    file = open(path_src,"r")
    contents = file.read()
    file.close()
    
    tpl=Template(contents)
    updated_content = tpl.safe_substitute( **params )
    
    file = open(path_dest, "w")
    file.write(updated_content)
    file.close()
    
    
def adjust_control_files(params, c_path):
    """
    Replace the $version parameter in the control files
    """
    files = ['control', 'postinst', 'postrm', 'preinst', 'prerm']
    for file in files:
        path = "%s/%s" % (c_path, file)
        print "scons: adjusting [%s]" % path
        replace_params(path,path,params)

def get_gcpwd():
    path = os.path.expanduser("~")
    file = open("%s/.gcpwd" % path)
    pwd = file.read().strip()
    file.close()
    return pwd

def get_gcuser():
    path = os.path.expanduser("~")
    file = open("%s/.gcuser" % path)
    user = file.read().strip()
    file.close()
    return user

def get_contents(path):
    """
    Retrieves the contents of the file,
    substituting the environment variables (eg. ~)
    in the target path
    """ 
    _path = os.path.expanduser(path)
    file = open(_path)
    contents = file.read().strip()
    file.close()
    return contents

    