-module(jvm).
-export([jvm/2]).

-include_lib("class.hrl").
-include_lib("jvm.hrl").
-include_lib("eunit/include/eunit.hrl").

jvm(InitClass, ClassPath) ->
    JVM = #jvm{
	     initclass = loader:load_class(InitClass, ClassPath)
	    },
    initialize_class(JVM, JVM#jvm.initclass).

%% Load class and execute initialization method
initialize_class(JVM, Class) ->
    ClassInitMethod = classfile:lookup_method("<clinit>", "()V", Class#class.classfile),
    InitThread = #javathread{current_method = ClassInitMethod, 
			     frames = [#frame{}]},
    execute(JVM, Class, InitThread).

execute(_JVM, Class, JavaThread) ->
    Code = classfile:get_method_code(JavaThread#javathread.current_method, Class#class.classfile),
    ?debugFmt("~nExecuting: ~w~n", [Code]),
    
%%%
%%% Unit tests.
jvm_test() ->
    jvm("Test", ["../priv"]).

