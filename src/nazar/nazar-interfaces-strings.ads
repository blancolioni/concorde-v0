package Nazar.Interfaces.Strings is

   type String_Interface is interface;

   function Get_String
     (From : String_Interface) return String
      is abstract;

   type String_Environment_Interface is interface;

   function Get_String_Value
     (From : String_Environment_Interface;
      Name : String)
      return String
     is abstract;

   procedure Set_String_Value
     (To    : in out String_Environment_Interface;
      Name  : String;
      Value : String)
   is abstract;

   function Expand_Environment
     (Environment : String_Environment_Interface'Class;
      Source_Text : String)
      return String;

end Nazar.Interfaces.Strings;
