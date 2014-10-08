-module(jvm).
-export([jvm/2]).

-include_lib("class.hrl").
-include_lib("classfile.hrl").
-include_lib("jvm.hrl").
-include_lib("eunit/include/eunit.hrl").

jvm(InitClass, ClassPath) ->
    %% Load the initial class, and initialize it. This includes
    %% executing the bytecode instructions of the initialization
    %% function <clinit>.
    Class = loader:load_class(InitClass, ClassPath),
    JVM = initialize_class(#jvm{}, Class),

    %% Lookup main.
    CF = Class#class.classfile,
    Method = classfile:lookup_method("main", "([Ljava/lang/String;)V", CF),
    Code = classfile:get_method_code(Method, CF),
    ?debugFmt("Main code: ~w~n", [Code]),

    %% Setup thread to execute main
    Frame = #frame{},
    Thread = #javathread{
		current_method = Method, 
		frames = [Frame]
	       },

    {void, JVM0, Thread0} = interpret(JVM, Code, Method, CF, Thread),
    [ActiveFrame|_] = Thread0#javathread.frames,
    ?debugFmt("Operand stack after method exit: ~w~n", [ActiveFrame#frame.stack]),
    JVM0.

%%% Push operand on stack
push_operand(Stack, Op) ->
    [Op|Stack].

pop_operand([Op|Stack]) ->
    {Op, Stack}.

%%% Push operand on stack of given frame. Returns new frame.
frame_push_operand(Frame, Op) ->
    Frame#frame{stack = push_operand(Frame#frame.stack, Op)}.

frame_pop_operand(Frame) ->
    {Op, NewStack} = pop_operand(Frame#frame.stack),
    {Op, Frame#frame{stack = NewStack}}.

%%%
%%% Initialize static class field
jvm_initialize_static(JVM, _CF, CPEntry, Value) -> 
    %% Store value in jvm.statics using CPEntry as key.
    JVM#jvm{statics = dict:store(CPEntry, Value, JVM#jvm.statics)}.


%%% Push operand on stack of thread's active frame. Returns new
%%% thread.
thread_push_operand(Thread, Op) ->
    [ActiveFrame|Rest] = Thread#javathread.frames,
    NewFrame = frame_push_operand(ActiveFrame, Op),
    Thread#javathread{frames = [NewFrame|Rest]}.
    
thread_pop_operand(Thread) ->
    [ActiveFrame|Rest] = Thread#javathread.frames,
    {Op, NewFrame} = frame_pop_operand(ActiveFrame),
    {Op, Thread#javathread{frames = [NewFrame|Rest]}}.
  
%% Initialize class
initialize_class(JVM, Class) ->
    CF = Class#class.classfile,
    ClassInitMethod = classfile:lookup_method("<clinit>", "()V", CF),
    Frame = #frame{},
    InitThread = #javathread{
		    current_method = ClassInitMethod, 
		    frames = [Frame]
		   },
    Code = classfile:get_method_code(ClassInitMethod, CF),
    {void, JVM0, _Thread} = interpret(JVM, Code, ClassInitMethod, CF, InitThread),
    JVM0.

interpret(JVM, Code, _Method, CF, Thread) ->
    {code, _, _, Bytes, _, _} = Code,
    {_, JVM0, Thread0} = interpret_bytecodes(Bytes, JVM, Thread, CF).

interpret_bytecodes(<<>>, _, _, _) ->
    throw({unexpected_end_of_code});

interpret_bytecodes(<<?ICONST_2:?U1, Rest/binary>>, JVM, Thread, CF) ->
    interpret_bytecodes(Rest, JVM, thread_push_operand(Thread, 2), CF);

interpret_bytecodes(<<?LDC:?U1, Index:?U1, Rest/binary>>, JVM, Thread, CF) ->
    CPEntry = classfile:lookup_constant(Index, CF),
    ?debugFmt("LDC: ~w~n", [CPEntry]),
    case CPEntry of
	{string, StringIndex} ->
	    interpret_bytecodes(Rest, JVM, 
				thread_push_operand(Thread, {stringref, StringIndex}), CF);
	%% TODO
	_ ->
	    interpret_bytecodes(Rest, JVM, Thread, CF)
    end;
interpret_bytecodes(<<?GETSTATIC:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF) ->
    interpret_bytecodes(Rest, JVM, Thread, CF);

interpret_bytecodes(<<?PUTSTATIC:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF) ->
    CPEntry = classfile:lookup_constant(Index, CF),
    {Op, NewThread} = thread_pop_operand(Thread),
    JVM0 = jvm_initialize_static(JVM, CF, CPEntry, Op),
    interpret_bytecodes(Rest, JVM0, NewThread, CF);

interpret_bytecodes(<<?NEW:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF) ->
    interpret_bytecodes(Rest, JVM, Thread, CF);

interpret_bytecodes(<<?DUP:?U1, Rest/binary>>, JVM, Thread, CF) ->
    interpret_bytecodes(Rest, JVM, Thread, CF);

interpret_bytecodes(<<?INVOKEVIRTUAL:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF) ->
    interpret_bytecodes(Rest, JVM, Thread, CF);

interpret_bytecodes(<<?INVOKESPECIAL:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF) ->
    interpret_bytecodes(Rest, JVM, Thread, CF);

interpret_bytecodes(<<?RETURN:?U1>>, JVM, Thread, _CF) ->
    %% void return
    {void, JVM, Thread};

interpret_bytecodes(<<X:?U1, _Rest/binary>>, _JVM, _Thread, _CF) ->
    %% [_ActiveFrame|_] = Thread#javathread.frames,
    %% ?debugFmt("Operand stack: ~w~n", [ActiveFrame#frame.stack]),
    throw({invalid_bytecode, X}).
    
%%%
%%% Unit tests.
jvm_test() ->
    JVM0 = jvm("Test", ["../priv"]),
    Statics = JVM0#jvm.statics,
    %% Statics should now contain an assignment of {fieldref,11,32} -> 2.
    ?assertEqual(2, dict:fetch({fieldref,11,32}, Statics)).

push_operand_test() ->
    ?assertEqual([4,1,2,3], push_operand([1,2,3], 4)).

pop_operand_test() ->
    ?assertEqual({4, [1,2,3]}, pop_operand([4,1,2,3])).

frame_push_operand_test() ->
    Result = frame_push_operand(#frame{stack = [1,2,3]}, 4),
    Expected = #frame{stack = [4,1,2,3]},
    ?assertEqual(Expected, Result).

frame_pop_operand_test() ->
    Expected = {4, #frame{stack = [1,2,3]}},
    Actual = frame_pop_operand(#frame{stack = [4,1,2,3]}),
    ?assertEqual(Expected, Actual).

thread_push_operand_test() ->
    Thread = #javathread{frames = [#frame{}, #frame{}]},
    Result = thread_push_operand(Thread, 1),
    ?assertEqual(#javathread{frames = [#frame{stack=[1]}, #frame{}]}, Result),
    Result1 = thread_push_operand(Result, 2),
    ?assertEqual(#javathread{frames = [#frame{stack=[2, 1]}, #frame{}]}, Result1).
    
thread_pop_operand_test() ->
    T = #javathread{frames = [#frame{stack=[2,1,3,4]}, #frame{}]},
    Expected = {2, #javathread{frames = [#frame{stack=[1,3,4]}, #frame{}]}},
    Actual = thread_pop_operand(T),
    ?assertEqual(Expected, Actual).
			    
jvm_initialize_static_test() ->
    Expected = #jvm{statics = dict:store({fieldref,1,2}, 42, dict:new())},    
    Actual = jvm_initialize_static(#jvm{}, nil, {fieldref, 1, 2}, 42),
    ?assertEqual(Expected, Actual).
