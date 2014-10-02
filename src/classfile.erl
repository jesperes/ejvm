-module(classfile).
-export([load_classfile/1,
	 lookup_constant/2]).

-include("../include/classfile.hrl").

load_classfile(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    CF0 = load_header(#classfile{tail = Bin}),
    CF1 = load_constant_pool(CF0),
    CF2 = load_flags(CF1),
    CF3 = load_interfaces(CF2),
    CF4 = load_fields(CF3),
    CF5 = load_methods(CF4),
    load_attributes(CF5).
    
load_header(CF) ->
    <<?CLASSFILE_MAGIC:?U4, 
      Minor:?U2, 
      Major:?U2,
      Rest/binary>> = CF#classfile.tail,
    #classfile{version = {Major, Minor}, tail = Rest}.

load_constant_pool(CF) ->
    <<Count:?U2, Bin0/binary>> = CF#classfile.tail,
    {CP, Bin1} = load_constant_pool(Bin0, 1, Count, []),
    CF#classfile{tail = Bin1, constant_pool = CP}.

load_constant_pool(Bin, X, X, Acc) ->
    {lists:reverse(Acc), Bin};
load_constant_pool(<<?CONSTANT_Methodref:8, CIndex:?U2, NTIndex:?U2, Bin/binary>>, Index, Count, Acc) ->
    load_constant_pool(Bin, Index + 1, Count, [{methodref, CIndex, NTIndex}|Acc]);
load_constant_pool(<<?CONSTANT_Fieldref:8, CIndex:?U2, NTIndex:?U2, Bin/binary>>, Index, Count, Acc) ->
    load_constant_pool(Bin, Index + 1, Count, [{fieldref, CIndex, NTIndex}|Acc]);
load_constant_pool(<<?CONSTANT_Class:8, NIndex:?U2, Bin/binary>>, Index, Count, Acc) ->
    load_constant_pool(Bin, Index + 1, Count, [{class, NIndex}|Acc]);
load_constant_pool(<<?CONSTANT_String:8, SIndex:?U2, Bin/binary>>, Index, Count, Acc) ->
    load_constant_pool(Bin, Index + 1, Count, [{string, SIndex}|Acc]);
load_constant_pool(<<?CONSTANT_Utf8:8, Length:?U2, Bin/binary>>, Index, Count, Acc) ->
    <<Bytes:Length/bytes, Bin0/binary>> = Bin,
    load_constant_pool(Bin0, Index + 1, Count, [{utf8, Length, Bytes}|Acc]);
load_constant_pool(<<?CONSTANT_NameAndType:8, NIndex:?U2, DIndex:?U2, Bin/binary>>, Index, Count, Acc) ->
    load_constant_pool(Bin, Index + 1, Count, [{nameandtype, NIndex, DIndex}|Acc]).

load_flags(CF) ->
    <<AccessFlags:?U2,
      ThisClass:?U2,
      SuperClass:?U2,
      Bin0/binary>> = CF#classfile.tail,
    CF#classfile{
      access_flags = AccessFlags,
      this_class = ThisClass,
      super_class = SuperClass,
      tail = Bin0
     }.

load_interfaces(CF) ->
    <<Count:?U2, Bin0/binary>> = CF#classfile.tail,
    ByteCount = Count * 2, %% U2
    <<Bytes:ByteCount/bytes, Bin1/binary>> = Bin0,
    CF#classfile{
      interfaces = load_interfaces0(Bytes),
      tail = Bin1
     }.

load_interfaces0(<<>>) ->
    [];
load_interfaces0(<<Index:?U2, Rest/binary>>) ->
    [{interface, Index}|load_interfaces0(Rest)].

load_fields(CF) ->
    <<FieldsCount:?U2, Bin0/binary>> = CF#classfile.tail,
    {Fields, Bin1} = load_fields(FieldsCount, Bin0, []),
    CF#classfile{
      fields = Fields,
      tail = Bin1
     }.

load_fields(0, Bin, Acc) ->
    {lists:reverse(Acc), Bin};
load_fields(Count, Bin, Acc) ->
    <<AccessFlags:?U2,
      NameIndex:?U2,
      DescriptorIndex:?U2,
      AttributesCount:?U2,
      Bin1/binary>> = Bin,
    {Attributes, Bin2} = load_attributes(AttributesCount, Bin1, []),
    load_fields(Count - 1, Bin2, [{field, AccessFlags, NameIndex, DescriptorIndex, Attributes}|Acc]).

load_methods(CF) ->
    <<MethodsCount:?U2, Bin0/binary>> = CF#classfile.tail,
    {Methods, Bin1} = load_methods(MethodsCount, Bin0, []),
    CF#classfile{
      methods = Methods,
      tail = Bin1
     }.

load_methods(0, Bin, Acc) ->
    {lists:reverse(Acc), Bin};
load_methods(Count, Bin, Acc) ->
    <<AccessFlags:?U2,
      NameIndex:?U2,
      DescriptorIndex:?U2,
      AttributesCount:?U2,
      Bin1/binary>> = Bin,
    {Attributes, Bin2} = load_attributes(AttributesCount, Bin1, []),
    load_methods(Count - 1, Bin2, [{method, AccessFlags, NameIndex, DescriptorIndex, Attributes}|Acc]).

load_attributes(CF) ->
    <<AttributesCount:?U2, Bin/binary>> = CF#classfile.tail,
    {Attributes, Bin0} = load_attributes(AttributesCount, Bin, []),
    CF#classfile{
      attributes = Attributes,
      tail = Bin0
     }.

load_attributes(0, Bin, Acc) ->
    {Acc, Bin};
load_attributes(Count, Bin, Acc) ->
    <<AttributeNameIndex:?U2,
      AttributeLength:?U4,
      Bin1/binary>> = Bin,
    <<Info:AttributeLength/bytes, Bin2/binary>> = Bin1,
    load_attributes(Count - 1, Bin2, [{attribute, AttributeNameIndex, Info}|Acc]).

%%% Lookup functions

lookup_constant(Index, CF) ->
    lists:nth(Index, CF#classfile.constant_pool).
