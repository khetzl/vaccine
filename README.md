Vaccine
============

Vaccine is a command line interface framework to call commands on remote nodes. Vaccine connects to the running node, compiles and injects code of the command module. As the command will be evaluated on the remote node, it won't suffer any limitation like it would do in the case of escript or rpc call.

Try Vaccine if you need:
------------------------

*   Independent deployment: Since Vaccine commands are not part of the target system, a package can be built and deployed anytime. 
*   Testing: Existing modules of the target system can be used in command modules, so it can access internal states, statuses, configuration data, that useful when comes to testing, but isn't meant to be exposed.
*   No left-over: Migration code often left in the codebase, resulting dependant code to maintain. Vaccine command modules won't be the part of the product. 
*   Configurable set of commands: Since it's modular, it can be built and deployed with different set of commands.


Requirements
------------
*   OS X or Linux operating system
*   Erlang R15 or newer
