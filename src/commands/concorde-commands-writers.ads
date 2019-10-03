private with Ada.Strings.Unbounded;

package Concorde.Commands.Writers is

   type String_Writer is
     new Writer_Interface with private;

   function To_String (Writer : String_Writer) return String;

private

   type String_Writer is
     new Writer_Interface with
      record
         Target : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Put
     (Writer : in out String_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out String_Writer);

   overriding procedure Put_Error
     (Writer  : in out String_Writer;
      Message : String);

end Concorde.Commands.Writers;
