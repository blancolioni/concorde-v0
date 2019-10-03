package body Concorde.Commands.Writers is

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line (Writer : in out String_Writer) is
      use Ada.Strings.Unbounded;
   begin
      Writer.Target := Writer.Target
        & Character'Val (10);
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put (Writer : in out String_Writer; Text : String) is
      use Ada.Strings.Unbounded;
   begin
      Writer.Target := Writer.Target & Text;
   end Put;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer  : in out String_Writer;
      Message : String)
   is
   begin
      Writer.Put_Line (Message);
   end Put_Error;

   ---------------
   -- To_String --
   ---------------

   function To_String (Writer : String_Writer) return String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Writer.Target);
   end To_String;

end Concorde.Commands.Writers;
