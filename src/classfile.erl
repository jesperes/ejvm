-module(classfile).
-export([load_classfile/1,
	 lookup_constant/2,
	 lookup_method/3,
	 get_method_code/2
	]).

%% -compile(export_all).

-include_lib("classfile.hrl").
-include_lib("eunit/include/eunit.hrl").

load_classfile(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    CF0 = load_header(#classfile{tail = Bin}),
    CF1 = load_constant_pool(CF0),
    CF2 = load_flags(CF1),
    CF3 = load_interfaces(CF2),
    CF4 = load_fields(CF3),
    CF5 = load_methods(CF4),
    load_classfile_attributes(CF5).
    
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

load_classfile_attributes(CF) ->
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

lookup_method(Name, Signature, CF) ->
    Methods = CF#classfile.methods,
    lookup_method0(Methods, Name, Signature, CF).

lookup_method0([], Name, Signature, _) ->
    throw({no_such_method, Name, Signature});
lookup_method0([Method = {method, _, NIndex, DIndex, _}|Methods], Name, Signature, CF) ->
    {utf8, _, NameUtf8Bin} = lookup_constant(NIndex, CF),
    {utf8, _, DescrUtf8Bin} = lookup_constant(DIndex, CF),
    Name0 = unicode:characters_to_list(NameUtf8Bin, utf8),
    Descr0 = unicode:characters_to_list(DescrUtf8Bin, utf8),
    %% ?debugFmt("~s.~s~n", [Name0, Descr0]),
    case {Name0, Descr0} of
	{Name, Signature} ->
	    Method;
	_ ->
	    lookup_method0(Methods, Name, Signature, CF)
    end.
    
%%% Looks up an UTF8 element in the constant pool by its string
%%% representation.
lookup_utf8(String, CF) ->
    lookup_utf80(CF#classfile.constant_pool, String).

lookup_utf80([], String) ->
    throw({no_such_utf8_String, String});
lookup_utf80([UTF8 = {utf8, _, Utf8Binary}|Entries], String) ->
    case unicode:characters_to_list(Utf8Binary, utf8) of
	String ->
	    UTF8;
	_ ->
	    lookup_utf80(Entries, String)
    end;
lookup_utf80([_|Entries], S) ->
    lookup_utf80(Entries, S).

%%%
%%% Looks up method attribute by name
lookup_method_attribute({method, _, _, _, Attributes}, String, CF) ->
    lookup_method_attribute0(Attributes, String, CF).

lookup_method_attribute0([], String, _) ->
    throw({no_such_attribute, String});
lookup_method_attribute0([Attr = {attribute, NIndex, _Data}|Attributes], String, CF) ->
    {utf8, _, AttrNameUtf8} = lookup_constant(NIndex, CF),
    case unicode:characters_to_list(AttrNameUtf8, utf8) of
	String ->
	    Attr;
	_ ->
	    lookup_method_attribute0(Attributes, String, CF)
    end.



%%%
%%% 
get_method_code(Method, CF) ->
    {method, _, _, _, Attributes} = Method,
    {attribute, _, CodeAttr} = lookup_method_attribute0(Attributes, "Code", CF),
    decode_code_attribute(CodeAttr).

%%%
%%% Decodes a "Code" attribute block.
decode_code_attribute(Bin) ->
    <<MaxStack:?U2,
      MaxLocals:?U2,
      CodeLength:?U4,
      Bin0/binary>> = Bin,

    <<Code:CodeLength/bytes, 
      ExcTableLength:?U2,
      Bin1/binary>> = Bin0,
    
    ExcTableBytes = ExcTableLength * 8,		% 8 bytes per exception table entry
    <<ExcTable:ExcTableBytes/bytes,
      AttrCount:?U2,
      Bin2/binary>> = Bin1,

    {Attributes, <<>>} = load_attributes(AttrCount, Bin2, []),

    #code{max_stack = MaxStack,
	  max_locals = MaxLocals,
	  bytecodes = Code,
	  exception_table = ExcTable,
	  attributes = Attributes}.

%%%
%%% Unit tests.
classfile_load_test() ->
    CF = load_classfile("../priv/Test.class"),
    MethodIndex = 1, %% Index of method ref of base class ctor, java.lang.Object#init.
    {methodref, NameIdx, DescrIdx} = lookup_constant(MethodIndex, CF),
    {class, ClassIdx} = lookup_constant(NameIdx, CF),
    {utf8, _, <<"java/lang/Object">>} = lookup_constant(ClassIdx, CF),
    {nameandtype, NIdx, DIdx} = lookup_constant(DescrIdx, CF),
    {utf8, _, <<"<init>">>} = lookup_constant(NIdx, CF),
    {utf8, _, <<"()V">>} = lookup_constant(DIdx, CF).

classfile_lookup_method_test() ->
    CF = load_classfile("../priv/Test.class"),
    {method, ?ACC_STATIC, _NameIndex, _DescrIndex, _Attributes} = lookup_method("<clinit>", "()V", CF).

classfile_lookup_utf8_test() ->
    CF = load_classfile("../priv/Test.class"),
    {utf8, _, <<"java/lang/Object">>} = lookup_utf8("java/lang/Object", CF).


classfile_lookup_method_attribute_test() ->
    CF = load_classfile("../priv/Test.class"),
    Method = lookup_method("<clinit>", "()V", CF),
    {attribute, _, _} = lookup_method_attribute(Method, "Code", CF).

classfile_decode_code_attribute_test() ->
    CF = load_classfile("../priv/Test.class"),
    Method = lookup_method("<clinit>", "()V", CF),
    {attribute, _, CodeAttr} = lookup_method_attribute(Method, "Code", CF),
    _Code = decode_code_attribute(CodeAttr).
    %% ?assertEqual(<<5,179,0,7,177>>, Code#code.bytecodes).
    %% ?debugFmt("~nCode = ~w~n", [Code]).

    
