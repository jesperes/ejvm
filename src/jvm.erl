-module(jvm).
-export([jvm/2]).

-include_lib("classfile.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Record to hold interpreter state.
-record(state, {
	  stack = [],
	  code = [],				% List of funs implementing bytecodes
	  pc = 0,				% Current PC.
	  branch_offset = 1,			% Defaults to next instruction
	  return = false
	 }).

jvm(InitClassName, ClassPath) when is_list(ClassPath) and is_list(InitClassName) ->
    %% Load the initial class, and initialize it. This includes
    %% executing the bytecode instructions of the initialization
    %% function <clinit>.
    Class = loader:load_class(InitClassName, ClassPath),

    %% Initialize initial class.
    State0 = initialize_class(#state{}, Class),

    %% Lookup main.
    State1 = load_method_code(State0, Class, "main", "([Ljava/lang/String;)V"),
    
    %% Start executing main entry point.
    execute_bytecode(State1).


load_method_code(State, Class, Name, Descr) ->
    Method = classfile:lookup_method(Name, Descr, Class),
    Code = classfile:get_method_code(Method, Class),
    State#state{code = translate_bytecodes(Code#code.bytecodes)}.

%% Initialize class
initialize_class(State, Class) ->
    State1 = load_method_code(State, Class, "<clinit>", "()V"),
    execute_bytecode(State1).


%%%
%%% Main interpreter loop. Fetch next instruction, execute it,
%%% iterate.
execute_bytecode(State) ->
    Fun = fetch_instruction(State),
    %% ?debugFmt("Executing ~w~n", [Fun]),
    State0 = Fun(State),
    State1 = calculate_next_pc(State0),
    case State1#state.return of
	true ->
	    State1#state{return = false};
	_ ->
	    execute_bytecode(State1)
    end.

fetch_instruction(State) ->
    Fun = lists:nth(State#state.pc + 1, State#state.code). % lists starts at index 1
    
calculate_next_pc(State) ->
    NextPC = State#state.pc + State#state.branch_offset,
    State#state{pc = NextPC, branch_offset = 1}.

trace(X) ->
    io:format(user, "[TRACE] ~w~n", [X]).

%%% Returns a fun which pushes Op on the operand stack.
get_op_push_fun(Op) ->
    fun(State) ->
	    State#state{stack = [Op|State#state.stack]}
    end.

get_istore_fun(Op) ->
    fun(State) ->
	    trace({istore, Op}),
	    %% TODO
	    State
    end.

get_iload_fun(Op) ->
    fun(State) ->
	    trace({iload, Op}),
	    %% TODO
	    State
    end.

get_bipush_fun(Op) ->
    fun(State) ->
	    trace({bipush, Op}),
	    %% TODO
	    State
    end.
    
get_ldc_fun(Op) ->
    fun(State) ->
	    %% TODO
	    trace({ldc, Op}),
	    State
    end.

get_if_cmpge_fun(Op) ->
    fun(State) ->
	    %% TODO
	    trace({if_cmpge, Op}),
	    State
    end.

get_goto_fun(Op) ->
    fun(State) ->
	    trace({goto, Op}),
	    %% TODO
	    State
    end.

get_iinc_fun(Op1, Op2) ->
    fun(State) ->
	    trace({iinc, Op1, Op2}),
	    %% TODO
	    State
    end.

get_getstatic_fun(Op) ->
    fun(State) ->
	    trace({getstatic, Op}),
	    %% TODO
	    State
    end.

get_putstatic_fun(Op) ->
    fun(State) ->
	    trace({putstatic, Op}),
	    %% TODO
	    State
    end.
	    
get_new_fun(Op) ->
    fun(State) ->
	    trace({new, Op}),
	    %% TODO
	    State
    end.

get_dup_fun() ->
    fun(State) ->
	    trace({dup}),
	    %% TODO
	    State
    end.

get_invokevirtual_fun(Op) ->
    fun(State) ->
	    trace({invokevirtual, Op}),
	    %% TODO
	    State
    end.

get_invokespecial_fun(Op) ->
    fun(State) ->
	    trace({invokespecial, Op}),
	    %% TODO
	    State
    end.

get_return_fun() ->
    fun(State) ->
	    trace({return}),
	    %% TODO
	    State#state{return = true}
    end.


%%%
%%% translate_bytecodes(Code) iterates through the bytecode and
%%% returns a list of funs which takes and returns a State parameter
%%% representing the state of the JVM.
translate_bytecodes(<<?ICONST_0:?U1, Rest/binary>>) ->
    [get_op_push_fun(0)|translate_bytecodes(Rest)];

translate_bytecodes(<<?ICONST_2:?U1, Rest/binary>>) ->
    [get_op_push_fun(2)|translate_bytecodes(Rest)];

translate_bytecodes(<<?ISTORE_0:?U1, Rest/binary>>) ->
    [get_istore_fun(0)|translate_bytecodes(Rest)];

translate_bytecodes(<<?ISTORE_1:?U1, Rest/binary>>) ->
    [get_istore_fun(1)|translate_bytecodes(Rest)];

translate_bytecodes(<<?ILOAD_1:?U1, Rest/binary>>) ->
    [get_iload_fun(1)|translate_bytecodes(Rest)];

translate_bytecodes(<<?BIPUSH:?U1, Byte:?U1, Rest/binary>>) ->
    [get_bipush_fun(Byte)|translate_bytecodes(Rest)];

translate_bytecodes(<<?LDC:?U1, Index:?U1, Rest/binary>>) ->
    [get_ldc_fun(Index)|translate_bytecodes(Rest)];

translate_bytecodes(<<?IF_CMPGE:?U1, BranchOffset:?U2, Rest/binary>>) ->
    [get_if_cmpge_fun(BranchOffset)|translate_bytecodes(Rest)];

translate_bytecodes(<<?GOTO:?U1, BranchOffset:?U2, Rest/binary>>) ->
    [get_goto_fun(BranchOffset)|translate_bytecodes(Rest)];

translate_bytecodes(<<?IINC:?U1, Index:?U1, Incr:?U1, Rest/binary>>) ->
    [get_iinc_fun(Index, Incr)|translate_bytecodes(Rest)];

translate_bytecodes(<<?GETSTATIC:?U1, Index:?U2, Rest/binary>>) ->
    [get_getstatic_fun(Index)|translate_bytecodes(Rest)];

translate_bytecodes(<<?PUTSTATIC:?U1, Index:?U2, Rest/binary>>) ->
    [get_putstatic_fun(Index)|translate_bytecodes(Rest)];

translate_bytecodes(<<?NEW:?U1, Index:?U2, Rest/binary>>) ->
    [get_new_fun(Index)|translate_bytecodes(Rest)];

translate_bytecodes(<<?DUP:?U1, Rest/binary>>) ->
    [get_dup_fun()|translate_bytecodes(Rest)];

translate_bytecodes(<<?INVOKEVIRTUAL:?U1, Index:?U2, Rest/binary>>) ->
    [get_invokevirtual_fun(Index)|translate_bytecodes(Rest)];

translate_bytecodes(<<?INVOKESPECIAL:?U1, Index:?U2, Rest/binary>>) ->
    [get_invokespecial_fun(Index)|translate_bytecodes(Rest)];

translate_bytecodes(<<?RETURN:?U1>>) ->
    [get_return_fun()];

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

%%push_operand_test() ->
%%    ?assertEqual([4,1,2,3], push_operand([1,2,3], 4)).

%%pop_operand_test() ->
%%    ?assertEqual({4, [1,2,3]}, pop_operand([4,1,2,3])).

%% frame_push_operand_test() ->
%%     Result = frame_push_operand(#frame{stack = [1,2,3]}, 4),
%%     Expected = #frame{stack = [4,1,2,3]},
%%     ?assertEqual(Expected, Result).

%% frame_pop_operand_test() ->
%%     Expected = {4, #frame{stack = [1,2,3]}},
%%     Actual = frame_pop_operand(#frame{stack = [4,1,2,3]}),
%%     ?assertEqual(Expected, Actual).

%% thread_push_operand_test() ->
%%     Thread = #javathread{frames = [#frame{}, #frame{}]},
%%     Result = thread_push_operand(Thread, 1),
%%     ?assertEqual(#javathread{frames = [#frame{stack=[1]}, #frame{}]}, Result),
%%     Result1 = thread_push_operand(Result, 2),
%%     ?assertEqual(#javathread{frames = [#frame{stack=[2, 1]}, #frame{}]}, Result1).
    
%% thread_pop_operand_test() ->
%%     T = #javathread{frames = [#frame{stack=[2,1,3,4]}, #frame{}]},
%%     Expected = {2, #javathread{frames = [#frame{stack=[1,3,4]}, #frame{}]}},
%%     Actual = thread_pop_operand(T),
%%     ?assertEqual(Expected, Actual).
			    
%% jvm_initialize_static_test() ->
%%     Expected = #jvm{statics = dict:store({fieldref,1,2}, 42, dict:new())},    
%%     Actual = jvm_initialize_static(#jvm{}, nil, {fieldref, 1, 2}, 42),
%%     ?assertEqual(Expected, Actual).

%% translate_bytecode_test() ->
%%     CF = classfile:load_classfile("../priv/Test.class"),
%%     Method = classfile:lookup_method("main", "([Ljava/lang/String;)V", CF),
%%     Code = classfile:get_method_code(Method, CF),
%%     TCode = translate(Code),
%%     ?debugFmt("Code = ~w~n", [TCode]),
%%     ?assertEqual(true, is_list(TCode)).



    
