CSP-Tracker: Generator of CSP tracks
=================================

Pending 1

Getting CSP-Tracker
----------------
The code of CSP-Tracker is contained in a GIT repository on GitHub. The URL is 
https://github.com/mistupv/csp_tracker

To get a copy of the repository you only have to write in your Linux/MacOS 
console:

    $ git clone https://github.com/mistupv/csp_tracker

This will create a folder called 'csp_tracker' in your current directory containing the
files of the generator of CSP tracks.


Compiling CSP-Tracker
-------------

The CSP-Tracker is written in Erlang, so you will need an Erlang
system installed in you computer. To compile the CSP-Tracker source first move to the source
directory of the repository (for example /home/john/git/csp_tracker/src) and compile all files:

    $ erlc *.erl
    1> 

Write the following commands to compile, load and run the compiler and 
installer: 

    1> c(edd_comp).
    {ok,edd_comp}
    2> edd_comp:compile().
    ok

The 'edd_comp:compile()' function simply automates the process. The files 
src/smerl.erl, src/edd_lib.erl and src/edd.erl will be compiled into the 'ebin' 
directory, and the edoc documentation will be generated into the folder 'edoc'.






