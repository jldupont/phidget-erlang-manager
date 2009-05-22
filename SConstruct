"""
scons build file

@author: Jean-Lou Dupont
"""
import os
import shutil

try:
	from pyjld.os import recursive_chmod, safe_copytree
except:
	print "*** Missing package: use easy_install pyjld.os"
	exit(1)


Help("""\
 Type:	
   'scons' to build the libraries (release and debug),
   'scons deb' to build the .deb package
   'scons release' to release the package to tags/debian repository
   'scons install' to install on local machine
""")

# LIST MODULES
# ============
list_modules = [	'#project/main',
					'#project/drivers/phidgetinterfacekit'
				]


modules = []
for module in list_modules:
	modules.append( {	'script'  : "%s/SConscript" % module,
						'build'   : module,
						'release' : Environment(CPPPATH=module+'/include'),
						'debug'   : Environment(CPPPATH=module+'/include', CPPFLAGS="-D_DEBUG -g", _DEBUG='1')
					} )

for module in modules:
	SConscript(module['script'], build_dir=module['build']+'/release', exports={'env':module['release']})
	SConscript(module['script'], build_dir=module['build']+'/debug',   exports={'env':module['debug']})



# INSTALLING on LOCAL MACHINE
if 'install' in COMMAND_LINE_TARGETS:
	print "scons: INSTALLING LOCALLY"


