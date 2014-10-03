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
    CF = Class#class.classfile,
    ClassInitMethod = classfile:lookup_method("<clinit>", "()V", CF),
    InitThread = #javathread{current_method = ClassInitMethod, 
			     frames = [#frame{}]},
    Code = classfile:get_method_code(ClassInitMethod, CF),
    ?debugFmt("~nExecuting: ~w~n", [Code]).
    
%%%
%%% Unit tests.

jvm_test() ->
    jvm("Test", ["../priv"]).

