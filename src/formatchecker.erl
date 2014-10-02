-module(formatchecker).
-export([check/1]).

-include("classfile.hrl").

%%% Checks the format of a classfile record. See section 4.8 in the VM spec.
check(CF) ->
    %% Magic number is checked when loading.
    %% TODO Attribute length checking 

    %% Check: no trailing bytes
    <<>> = CF#classfile.tail,
    
    %% Check: constant pool constraints
    ok = check_constant_pool(CF#classfile.constant_pool, CF).

check_constant_pool([], _) ->
    ok;
check_constant_pool([{methodref, CIndex, NTIndex}|Rest], CF) ->
    {class, _} = classfile:lookup_constant(CIndex, CF),
    %% TODO Class must not be interface, how do we check this?
    {nameandtype, _, _} = classfile:lookup_constant(NTIndex, CF),
    %% TODO If method name begins with "<", it must be "<init>"
    %% TODO descriptor must be a method descriptor
    check_constant_pool(Rest, CF);
check_constant_pool([{fieldref, CIndex, NTIndex}|Rest], CF) ->
    {class, _} = classfile:lookup_constant(CIndex, CF),
    {nameandtype, _, _} = classfile:lookup_constant(NTIndex, CF),
    %% TODO descriptor must be a field descriptor
    check_constant_pool(Rest, CF);
check_constant_pool([{class, Index}|Rest], CF) ->
    {utf8, _, _} = classfile:lookup_constant(Index, CF),
    %% TODO check validity of class name
    check_constant_pool(Rest, CF);
check_constant_pool([{string, Index}|Rest], CF) ->
    {utf8, _, _} = classfile:lookup_constant(Index, CF),
    check_constant_pool(Rest, CF);
check_constant_pool([{utf8, Length, Bytes}|Rest], CF) ->
    Length = byte_size(Bytes),
    %% TODO check valid UTF-8 string
    check_constant_pool(Rest, CF);
check_constant_pool([{nameandtype, NameIndex, DescriptorIndex}|Rest], CF) ->
    %% TODO check that the names are valid
    {utf8, _, _} = classfile:lookup_constant(NameIndex, CF),
    {utf8, _, _} = classfile:lookup_constant(DescriptorIndex, CF),
    check_constant_pool(Rest, CF);
check_constant_pool([CPInfo|Rest], CF) ->    
    io:format("Constant pool format check not implemented for ~w~n", [CPInfo]),
    check_constant_pool(Rest, CF).

    
