CSP-Tracker: Generator of CSP tracks
=================================

CSP-Tracker implements a CSP interpreter with a tracker.
The interpreter executes a CSP specification and simultaneously produces the track associated with the performed derivation.

CSP-Tracker incorporates mechanisms to produce colored graphs that represent the tracks in a very intuitive way.
CSP-Tracker can generate the track of a (partial) derivation until it finishes or is stopped. This is specially useful for the analysis of non-termination.
In CSP-Tracker, the tracking process is completely automatic.
Once the user has loaded a CSP specification, she can (automatically) produce a derivation and the tool internally generates the associated track.
Both the track and the trace can be stored in a file, or displayed in the screen by generating [Graphviz](http://www.graphviz.org/) graphs.

You can also try it online [here](http://kaz.dsic.upv.es/csp_tracker.html).

Getting CSP-Tracker
----------------
The code of CSP-Tracker is distributed through a GIT repository on GitHub. The URL is 
https://github.com/mistupv/csp_tracker

To get a copy of the repository you only have to type the following command in your Linux/MacOS 
terminal:

    $ git clone https://github.com/mistupv/csp_tracker

This will create a folder called 'csp_tracker' in your current directory containing the
files of the generator of CSP tracks.

Compiling CSP-Tracker
----------------

CSP-Tracker is written in Erlang, so you will need an Erlang
system installed in you computer. To compile the CSP-Tracker' sources, first move to the main directory of the repository (for instance /home/john/git/csp_tracker/csp_tracker) and compile all files:

	$ erlc *.erl
    1> 

Then, move the binaries to this directory according to your OS. For instance, if it is Mac OsX:

	$ mv bin_macos/* .

After this two steps, CSP-Tracker is ready to be used.

Using CSP-Tracker
-------------

First of all, you need to write down your CSP specification using the syntax used by [ProB](http://www.stups.uni-duesseldorf.de/ProB/index.php5/CSP-M_Syntax). You can find some examples in the directory '[examples](https://github.com/mistupv/csp_tracker/tree/master/csp_tracker/examples)', and also in our [web interface](http://kaz.dsic.upv.es/csp_tracker.html).

In order to execute and generate the track of a CSP specification into a file, move or create this file to the main directory (for example /home/john/git/csp_tracker/csp_tracker), and run erlang from there.

	$erl
	....
	1>

Then, suppose that we want to generate the track of '[ex3.csp](https://github.com/mistupv/csp_tracker/blob/master/csp_tracker/examples/ex3.csp)', we should call function track of module '[csp_tracker](https://github.com/mistupv/csp_tracker/blob/master/csp_tracker/csp_tracker.erl)'.

	1> csp_tracker:track('ex3.csp').
	Creating the Erlang representation of the CSP file...
	...
	Created.

	-> START_TRACE

	   tau -> Call to process MAIN
	   tau -> Call to process P
	a
	   tau
	   tau
	   tau
	   tick

	<- FINISH_TRACE


After the execution, a file called 'track.dot' will be created in the directory. If you have installed [Graphviz](http://www.graphviz.org/), a equivalent pdf file will be created. Both files represent the track of the specification with a different format.

If we are not interested in the internal events occurring during the execution, we can call the same function, but with an option indicating our preferences.

	2> csp_tracker:track('ex3.csp',[only_externals]).
	Creating the Erlang representation of the CSP file...
	...
	Created.

	-> START_TRACE

	a

	<- FINISH_TRACE

Some specifications produce a deadlock, and our tool will stop them automatically when the deadlock is detected. For instance, using '[ex1.csp](https://github.com/mistupv/csp_tracker/blob/master/csp_tracker/examples/ex1.csp)':


	3> csp_tracker:track('ex1.csp',[only_externals]).
	Creating the Erlang representation of the CSP file...
	...
	Created.

	-> START_TRACE

	a.s1
	b.s2
	b.s0
	a.s1
	b.s2
	not_valid

	<- STOPPED_TRACE (deadlock)


Finally, when the specification produces an infinite computation, we can define a timeout to stop automatically this execution. This is the case of '[ex6.csp](https://github.com/mistupv/csp_tracker/blob/master/csp_tracker/examples/ex6.csp)'. Assume that we want to execute it during 5 seconds.


	4> csp_tracker:track('ex6.csp',[only_externals,5000]).
	Creating the Erlang representation of the CSP file...
	...
	Created.

	-> START_TRACE

	a
	b
	...
	a
	b

	Timeout.
