CSP-Tracker: Generator of CSP tracks
=================================

CSP-Tracker implements a CSP interpreter with a tracker.
The interpreter executes a CSP specification and simultaneously produces the track associated to the performed derivation.

CSP-Tracker incorporates mechanisms to produce colored graphs that represent the tracks in a very intuitive way.
CSP-Tracker can generate the track of a (partial) derivation until it finishes or is stopped. This is specially useful for the analysis of non-termination.
In CSP-Tracker, the tracking process is completely automatic.
Once the user has loaded a CSP specification, she can (automatically) produce a derivation and the tool internally generates the associated track.
Both the track and the trace can be stored in a file, or displayed in the screen by generating [Graphviz](http://www.graphviz.org/) graphs.

You can try it online [here](http://kaz.dsic.upv.es/csp_tracker.html).

Getting CSP-Tracker
----------------
The code of CSP-Tracker is contained in a GIT repository on GitHub. The URL is 
https://github.com/mistupv/csp_tracker

To get a copy of the repository you only have to write in your Linux/MacOS 
terminal:

    $ git clone https://github.com/mistupv/csp_tracker

This will create a folder called 'csp_tracker' in your current directory containing the
files of the generator of CSP tracks.


Compiling CSP-Tracker
-------------

The CSP-Tracker is written in Erlang, so you will need an Erlang
system installed in you computer. To compile the CSP-Tracker source first move to the main directory of the repository (for example /home/john/git/csp_tracker/csp_tracker) and compile all files:

	$ erlc *.erl
    1> 

Then, move the binaries to this directory according to your OS. For instance, if it is Mac OsX:

	$ mv bin_macos/* .

After this two steps, CSP-Tracker is ready to be used.

Using CSP-Tracker
-------------

First of all, you need to write down your CSP specification using the syntax used by [ProB](http://www.stups.uni-duesseldorf.de/ProB/index.php5/CSP-M_Syntax). You can find some examples in the '[examples](https://github.com/mistupv/csp_tracker/csp_tracker/examples)' directory, and in our [web interface](http://kaz.dsic.upv.es/csp_tracker.html).







