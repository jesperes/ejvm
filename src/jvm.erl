-module(jvm).
-export([jvm/2]).

-include_lib("class.hrl").
-include_lib("classfile.hrl").
-include_lib("jvm.hrl").
-include_lib("eunit/include/eunit.hrl").

jvm(InitClass, ClassPath) ->
    Class = loader:load_class(InitClass, ClassPath),
    JVM = #jvm{
       classes = dict:store(InitClass, Class, dict:new())
      },
    initialize_class(JVM, Class).

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
jvm_initialize_static(JVM, ClassName, CPEntry, _Value) -> 
    {fieldref, CIndex, NTIndex} = CPEntry,
    ?debugFmt("Fieldref = ~w~n", [CPEntry]),
    %%InitClass = dict:fetch(ClassName, JVM#jvm.classes),
    %%dict:store(CPEntry# InitClass#class.statics
    JVM.

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
    {void, JVM0} = interpret(JVM, Code, ClassInitMethod, CF, InitThread, Class),
    JVM0.

interpret(JVM, Code, _Method, CF, Thread, Class) ->
    %% ?debugFmt("~nExecuting: ~w~n", [Code]),
    {code, _, _, Bytes, _, _} = Code,
    interpret_bytecodes(Bytes, JVM, Thread, CF, Class).

interpret_bytecodes(<<>>, _JVM, _, _, _) ->
    throw({unexpected_end_of_code});
interpret_bytecodes(<<?ICONST_2:?U1, Rest/binary>>, JVM, Thread, CF, Class) ->
    %% Push 2 on the operand stack.
    %%?debugMsg("Push 2 on operand stack."),
    interpret_bytecodes(Rest, JVM, thread_push_operand(Thread, 2), CF, Class);
interpret_bytecodes(<<?PUTSTATIC:?U1, Index:?U2, Rest/binary>>, JVM, Thread, CF, Class) ->
    CPEntry = classfile:lookup_constant(Index, CF),
    {Op, NewThread} = thread_pop_operand(Thread),
    %%?debugFmt("Putstatic: ~w (~w) ~w", [Index, Op, CPEntry]),
    %% initialize the field specified by CPEntry to the value Op
    JVM0 = jvm_initialize_static(JVM, Class#class.name, CPEntry, Op),
    interpret_bytecodes(Rest, JVM0, NewThread, CF, Class);
interpret_bytecodes(<<?RETURN:?U1>>, JVM, _Thread, _CF, _Class) ->
    %% void return
    {void, JVM};
interpret_bytecodes(<<X:?U1, _Rest/binary>>, _JVM, _Thread, _CF, _Class) ->
    %% [_ActiveFrame|_] = Thread#javathread.frames,
    %% ?debugFmt("Operand stack: ~w~n", [ActiveFrame#frame.stack]),
    throw({invalid_bytecode, X}).
    
%%%
%%% Unit tests.
jvm_test() ->
    _JVM0 = jvm("Test", ["../priv"]).

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
			    
