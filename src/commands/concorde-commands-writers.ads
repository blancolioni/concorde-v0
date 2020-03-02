private with Ada.Strings.Unbounded;

with Nazar.Json;
with Concorde.Writers;

package Concorde.Commands.Writers is

   type String_Writer is
     new Concorde.Writers.Writer_Interface with private;

   overriding procedure Put
     (Writer : in out String_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out String_Writer);

   overriding procedure Put_Error
     (Writer  : in out String_Writer;
      Message : String);

   function To_String (Writer : String_Writer) return String;

   type Json_Writer is
     new Concorde.Writers.Writer_Interface with private;

   overriding procedure Put
     (Writer : in out Json_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out Json_Writer);

   overriding procedure Put_Error
     (Writer  : in out Json_Writer;
      Message : String);

   overriding procedure Control
     (Writer : in out Json_Writer;
      Packet : Nazar.Json.Json_Value'Class);

   overriding procedure Return_Value
     (Writer : in out Json_Writer;
      Value  : Nazar.Json.Json_Value'Class);

   function To_Json
     (Writer : Json_Writer)
      return Nazar.Json.Json_Value'Class;

private

   type String_Writer is
     new Concorde.Writers.Writer_Interface with
      record
         Target : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Json_Writer is
     new Concorde.Writers.Writer_Interface with
      record
         Output_Lines     : Nazar.Json.Json_Array;
         Error_Lines      : Nazar.Json.Json_Array;
         Current_Output   : Ada.Strings.Unbounded.Unbounded_String;
         Control          : Nazar.Json.Json_Array;
         Result           : Nazar.Json.Json_Object;
      end record;

end Concorde.Commands.Writers;
