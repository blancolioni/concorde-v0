with Nazar.Interfaces.Text_Writer;

package Nazar.Interfaces.Commands is

   type Command_Interface is interface;

   type Arguments_Interface is interface;

   procedure Execute
     (Command   : Command_Interface;
      Arguments : Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is abstract;

   function Argument_Count
     (Arguments : Arguments_Interface)
      return Natural
      is abstract;

   function Argument
     (Arguments : Arguments_Interface;
      Index     : Positive)
      return String
   is abstract
     with Pre'Class => Index <= Argument_Count (Arguments);

   function Has_Binding
     (Argument : Arguments_Interface;
      Name     : String)
      return Boolean
   is abstract;

   function Binding
     (Arguments : Arguments_Interface;
      Name      : String)
      return String
   is abstract
     with Pre'Class => Arguments.Has_Binding (Name);

   function Binding
     (Arguments : Arguments_Interface'Class;
      Name      : String;
      Default   : String)
      return String;

private

   function Binding
     (Arguments : Arguments_Interface'Class;
      Name      : String;
      Default   : String)
      return String
   is (if Arguments.Has_Binding (Name)
       then Arguments.Binding (Name)
       else Default);

end Nazar.Interfaces.Commands;
