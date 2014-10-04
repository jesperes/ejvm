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
    interpret(JVM, Code, ClassInitMethod, CF).

interpret(JVM, Code, Method, CF) ->
    ?debugFmt("~nExecuting: ~w~n", [Code]),
    {code, _, _, Bytes, _, _} = Code,
    interpret_bytecodes(binary_to_list(Bytes), JVM).

interpret_bytecodes([], JVM) ->
    ok;
interpret_bytecodes([B|Rest], JVM) ->
    ?debugFmt("Interpreting: ~w", [B]),
    interpret_bytecodes(Rest, JVM).
    
%%%
%%% Unit tests.

jvm_test() ->
    jvm("Test", ["../priv"]).

