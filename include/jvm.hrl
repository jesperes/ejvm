-record(jvm, {
	  initclass
	 }).

-record(javathread, {
	  frames = [],
	  current_method
	 }).

-record(frame, {
	 }).

%%% The code attribute.
%%% http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3
-record(code, {
	  max_stack,
	  max_locals,
	  bytecodes,
	  exception_table = [],
	  attributes = []
	 }).
