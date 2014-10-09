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

    TranslatedCode = translate(Code),
    JVM.


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
    TranslatedCode = translate(Code),
    JVM.

translate(Code) ->
    {code, _, _, Bytes, _, _} = Code,
    translate_bytecodes(Bytes).

translate_bytecodes(<<?ICONST_0:?U1, Rest/binary>>) ->
    [{iconst, 0}|translate_bytecodes(Rest)];

translate_bytecodes(<<?ICONST_2:?U1, Rest/binary>>) ->
    [{iconst, 0}|translate_bytecodes(Rest)];

translate_bytecodes(<<?ISTORE_0:?U1, Rest/binary>>) ->
    [{istore, 0}|translate_bytecodes(Rest)];

translate_bytecodes(<<?ISTORE_1:?U1, Rest/binary>>) ->
    [{istore, 1}|translate_bytecodes(Rest)];

translate_bytecodes(<<?ILOAD_1:?U1, Rest/binary>>) ->
    [{iload, 1}|translate_bytecodes(Rest)];

translate_bytecodes(<<?BIPUSH:?U1, Byte:?U1, Rest/binary>>) ->
    [{bipush, Byte}|translate_bytecodes(Rest)];

translate_bytecodes(<<?LDC:?U1, Index:?U1, Rest/binary>>) ->
    [{ldc, Index}|translate_bytecodes(Rest)];

translate_bytecodes(<<?IF_CMPGE:?U1, BranchOffset:?U2, Rest/binary>>) ->
    [{if_cmpge, BranchOffset}|translate_bytecodes(Rest)];

translate_bytecodes(<<?GOTO:?U1, BranchOffset:?U2, Rest/binary>>) ->
    [{goto, BranchOffset}|translate_bytecodes(Rest)];

translate_bytecodes(<<?IINC:?U1, Index:?U1, Incr:?U1, Rest/binary>>) ->
    [{iinc, Index, Incr}|translate_bytecodes(Rest)];

translate_bytecodes(<<?GETSTATIC:?U1, Index:?U2, Rest/binary>>) ->
    [{getstatic, Index}|translate_bytecodes(Rest)];

translate_bytecodes(<<?PUTSTATIC:?U1, Index:?U2, Rest/binary>>) ->
    [{putstatic, Index}|translate_bytecodes(Rest)];

translate_bytecodes(<<?NEW:?U1, Index:?U2, Rest/binary>>) ->
    [{new, Index}|translate_bytecodes(Rest)];

translate_bytecodes(<<?DUP:?U1, Rest/binary>>) ->
    [{dup}|translate_bytecodes(Rest)];

translate_bytecodes(<<?INVOKEVIRTUAL:?U1, Index:?U2, Rest/binary>>) ->
    [{invokevirtual, Index}|translate_bytecodes(Rest)];

translate_bytecodes(<<?INVOKESPECIAL:?U1, Index:?U2, Rest/binary>>) ->
    [{invokespecial, Index}|translate_bytecodes(Rest)];

translate_bytecodes(<<?RETURN:?U1>>) ->
    [{return}];

translate_bytecodes(<<>>) ->
    throw({unexpected_end_of_code});

translate_bytecodes(<<X:?U1, _Rest/binary>>) ->
    %% [{invalid_bytecode, X}],
    throw({invalid_bytecode, X}).



%%%
%%% Unit tests.
jvm_test() ->
    JVM0 = jvm("Test", ["../priv"]).
    %% Statics = JVM0#jvm.statics,
    %% Statics should now contain an assignment of {fieldref,11,32} -> 2.
    %% ?assertEqual(2, dict:fetch({fieldref,11,32}, Statics)).

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

translate_bytecode_test() ->
    CF = classfile:load_classfile("../priv/Test.class"),
    Method = classfile:lookup_method("main", "([Ljava/lang/String;)V", CF),
    Code = classfile:get_method_code(Method, CF),
    TCode = translate(Code),
    ?debugFmt("Code = ~w~n", [TCode]),
    ?assertEqual(true, is_list(TCode)).



    
