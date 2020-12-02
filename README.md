CSP-Tracker: Generator of CSP tracks
=================================

CSP-Tracker implements a CSP interpreter with a tracker.
The interpreter executes a CSP specification and simultaneously produces the track associated with the performed derivation.

CSP-Tracker incorporates mechanisms to produce colored graphs that represent the tracks in a very intuitive way.
CSP-Tracker can generate the track of a (partial) derivation until it finishes or is stopped. This is specially useful for the analysis of non-termination.
In CSP-Tracker, the tracking process is completely automatic.
Once the user has loaded a CSP specification, she can (automatically) produce a derivation, and the tool internally generates the associated track.
Both the track and the trace can be stored in a file, or displayed in the screen by generating [Graphviz](http://www.graphviz.org/) graphs.

You can also try our tool [online](http://kaz.dsic.upv.es/csp_tracker).

The following video shows compilation, run and slicing using CSP-Tracker with a very simple example:


[![asciicast](https://asciinema.org/a/7vmrtodxv1fmpstt8dzz50m1a.png)](https://asciinema.org/a/7vmrtodxv1fmpstt8dzz50m1a)

Getting CSP-Tracker
----------------
The code of CSP-Tracker is distributed through a GIT repository on GitHub.

To get a copy of the repository you only have to type the following command in your Linux/MacOS 
terminal:

    $ git clone https://github.com/mistupv/csp_tracker

This will create a folder called 'csp_tracker' in your current directory containing the
files of the generator of CSP tracks.

Compiling CSP-Tracker
----------------

CSP-Tracker is written in Erlang, so you will need an Erlang system installed in you computer. To compile the CSP-Tracker' sources, perform the following steps:

    $ git clone https://github.com/mistupv/csp_tracker
    $ cd csp_tracker
	$ make

It will detect your operating system and move the appropriate binaries (from [ProB][ProB]) to the root of the repository. For platforms where there aren't compatible binaries available, it will prompt you to start a docker daemon, so that these binaries can be run in a Debian container.

CSP-Tracker is now ready to be used.

[ProB]: http://www.stups.uni-duesseldorf.de/ProB/

Using CSP-Tracker
-------------

First of all, you need to write down your CSP specification using the syntax used by [ProB][ProB-Syntax]. You can find some examples in the directory '[examples](/examples)', the '[benchmarks](/benchmarks)' suite directory and our [web interface][webUI].

[ProB-Syntax]: http://www.stups.uni-duesseldorf.de/ProB/index.php5/CSP-M_Syntax
[webUI]: http://kaz.dsic.upv.es/csp_tracker.html 

In order to execute and generate the track of a CSP specification into a file the first step is to run Erlang with the following command from the main directory of the repository.

	$ erl -pa ebin
	....
	1>

Then, suppose that we want to generate the track of '[ex3.csp](/examples/ex3.csp)', we should call function track/2 of module '[csp_tracker](/src/csp_tracker.erl)' with first argument the CSP specification to be tracked and as second argument the initial process.

	1> csp_tracker:track('examples/ex3.csp', 'MAIN').
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
	Total of time converting: 44.699 ms
	Total of time executing: 633.202 ms
	Total of time: 677.901 ms
	Total of node: 13 nodes
	Total of control edges: 9 edges
	Total of synchronization edges: 1 edges
	Total of edges: 10 edges
	Size of DOT file: 1324 bytes
	******************************


After the execution, a file called 'track.dot' will be created in the directory. If you have installed [Graphviz](http://www.graphviz.org/), an equivalent pdf file will be created. Both files represent the track of the specification with a different format.

If we are not interested in the internal events occurring during the execution, we can call the same function, but with the option 'only_externals' indicating this preference.

	2> csp_tracker:track('examples/ex3.csp','MAIN',[only_externals]).
	Creating the Erlang representation of the CSP file...
	...
	Created.

	-> START_TRACE

	a

	<- FINISH_TRACE

Some specifications produce a deadlock, and our tool will stop their execution automatically when the deadlock is detected. For instance, using '[ex1.csp](/examples/ex1.csp)':


	3> csp_tracker:track('examples/ex1.csp','MAIN',[only_externals]).
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


Finally, in those cases when the specification produces an infinite computation, we can define a timeout to stop automatically this execution. This is the case of '[ex6.csp](/examples/ex6.csp)'. Assume that we want to execute it during 5 milliseconds.


	4> csp_tracker:track('examples/ex6.csp','MAIN',[only_externals,5]).
	Creating the Erlang representation of the CSP file...
	...
	Created.

	-> START_TRACE
	
	
	<- STOPPED_TRACE (timeout)

	************Trace*************
	
	b
	a
	a
	b
	b
	a
	a
	b
	b
	a
	a
	b
	b
	a
	b
	b
	a
	b
	a
	
	******************************
	Total of time converting: 50.538 ms
	Total of time executing: 5.552 ms
	Total of time: 56.09 ms
	Total of node: 87 nodes
	Total of control edges: 58 edges
	Total of synchronization edges: 8 edges
	Total of edges: 66 edges
	Size of DOT file: 9014 bytes
	******************************
	
Dynamic Slicing
-------------
In order to slice a CSP specification the user should place a special channel named 'slice' immediately after a channel occurrence of interest. Note that this special channel *must* be declared in the channel section of the specification. 

	channel a,b,c, slice
	
	MAIN = P ||| Q
	
	P = a -> b -> slice -> SKIP
	
	Q = a -> b -> c -> slice -> SKIP

Then, we can run the tool as usual. After the track is created, the tool will ask the user a question like this:

	*********** Slice ************
	The slicing criterion was executed 2 times.
	
	Which execution are you interested? 
	
According to the user's answer an output with the slice will be generated:
	
	Which execution are you interested? 2
	Total of time generating slice: 0.176 ms
	
	********* Gaps Slice **********
	
	MAIN  = (Q  ||| ___)
	
	Q  = a -> b -> c -> ___
	
	
	*******************************
	
	******* Executable Slice ******
	
	MAIN  = (Q  ||| STOP)
	
	Q  = a -> b -> c -> STOP
	
	
	*******************************
	Total of time creating output:  0.163 ms
	*******************************
