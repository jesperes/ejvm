-module(jvm).
-export([jvm/2]).

-include_lib("class.hrl").
-include_lib("classfile.hrl").
-include_lib("jvm.hrl").
-include_lib("eunit/include/eunit.hrl").

jvm(InitClass, ClassPath) ->
    JVM = #jvm{
	     initclass = loader:load_class(InitClass, ClassPath)
	    },
    initialize_class(JVM, JVM#jvm.initclass).

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
  
%% Load class and execute initialization method
initialize_class(JVM, Class) ->
    CF = Class#class.classfile,
    ClassInitMethod = classfile:lookup_method("<clinit>", "()V", CF),
    
    Frame = #frame{},

    InitThread = #javathread{
		    current_method = ClassInitMethod, 
		    frames = [Frame]
		   },

    Code = classfile:get_method_code(ClassInitMethod, CF),
    interpret(JVM, Code, ClassInitMethod, CF, InitThread).

interpret(JVM, Code, _Method, CF, Thread) ->
    %% ?debugFmt("~nExecuting: ~w~n", [Code]),
    {code, _, _, Bytes, _, _} = Code,
    interpret_bytecodes(Bytes, JVM, Thread, CF).

interpret_bytecodes(<<>>, _JVM, _, _) ->
    ok;

interpret_bytecodes(<<?ICONST_2:?U1, Rest/binary>>, JVM, Thread, CF) ->
    %% Push 2 on the operand stack.
    %%?debugMsg("Push 2 on operand stack."),
    interpret_bytecodes(Rest, JVM, thread_push_operand(Thread, 2), CF);
interpret_bytecodes(<<?PUTSTATIC:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF) ->
    _CPEntry = classfile:lookup_constant(Index, CF),
    {_Op, NewThread} = thread_pop_operand(Thread),
    %%?debugFmt("Putstatic: ~w (~w) ~w", [Index, Op, CPEntry]),
    %% initialize the field specified by CPEntry to the value Op
    interpret_bytecodes(Rest, JVM, NewThread, CF);
interpret_bytecodes(<<?RETURN:?U1, Rest/binary>>, JVM, Thread, CF) ->
    %%?debugMsg("Return."),
    interpret_bytecodes(Rest, JVM, Thread, CF);
interpret_bytecodes(<<X:?U1, _Rest/binary>>, _JVM, _Thread, _CF) ->
    %% [_ActiveFrame|_] = Thread#javathread.frames,
    %% ?debugFmt("Operand stack: ~w~n", [ActiveFrame#frame.stack]),
    throw({invalid_bytecode, X}).
    
%%%
%%% Unit tests.
jvm_test() ->
    jvm("Test", ["../priv"]).


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
			    
