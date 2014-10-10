-define(CLASSFILE_MAGIC, 16#cafebabe).

-define(U4, 32/big).
-define(U2, 2/big-signed-integer-unit:8).
-define(U1, 8).

-define(CONSTANT_Utf8, 1).
-define(CONSTANT_Integer, 3).
-define(CONSTANT_Float, 4).
-define(CONSTANT_Long, 5).
-define(CONSTANT_Double, 6).
-define(CONSTANT_Class, 7).
-define(CONSTANT_String, 8).
-define(CONSTANT_Fieldref, 9).
-define(CONSTANT_Methodref, 10).
-define(CONSTANT_InterfaceMethodref, 11).
-define(CONSTANT_NameAndType, 12).
-define(CONSTANT_MethodHandle, 15).
-define(CONSTANT_MethodType, 16).
-define(CONSTANT_InvokeDynamic, 18).

-define(ACC_PUBLIC, 16#0001).
-define(ACC_STATIC, 16#0008).
-define(ACC_FINAL, 16#0010).
-define(ACC_SUPER, 16#0020).
-define(ACC_INTERFACE, 16#0x0200).
-define(ACC_ABSTRACT, 16#0x0400).
-define(ACC_SYNTHETIC, 16#0x1000).
-define(ACC_ANNOTATION, 16#0x2000).
-define(ACC_ENUM, 16#0x4000).

-define(ICONST_M1, 2).
-define(ICONST_0, 3).
-define(ICONST_1, 4).
-define(ICONST_2, 5).
-define(ICONST_3, 6).
-define(ICONST_4, 7).
-define(ICONST_5, 8).
-define(BIPUSH, 16).
-define(LDC, 18).
-define(ILOAD_0, 26).
-define(ILOAD_1, 27).
-define(ILOAD_2, 28).
-define(ILOAD_3, 29).
-define(ISTORE_0, 59).
-define(ISTORE_1, 60).
-define(ISTORE_2, 61).
-define(ISTORE_3, 62).
-define(DUP, 89).
-define(IINC, 132).
-define(IF_CMPGE, 162).
-define(GOTO, 167).
-define(RETURN, 177).
-define(GETSTATIC, 178).
-define(PUTSTATIC, 179).
-define(INVOKEVIRTUAL, 182).
-define(INVOKESPECIAL, 183).
-define(NEW, 187).

-record(classfile,
	{
	  version,
	  constant_pool,
	  access_flags,
	  this_class,
	  super_class,
	  interfaces,
	  fields,
	  methods, 
	  attributes,
	  tail
	}).

%%% The code attribute.
%%% http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3
-record(code, 
	{
	  max_stack,
	  max_locals,
	  bytecodes,
	  exception_table = [],
	  attributes = []
	}).
