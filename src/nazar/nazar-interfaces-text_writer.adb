package body Nazar.Interfaces.Text_Writer is

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Writer : in out Text_Writer_Interface'Class) is
   begin
      Writer.New_Line (Standard_Text);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Writer : in out Text_Writer_Interface'Class;
                  Text   : String)
   is
   begin
      Writer.Put (Standard_Text, Text);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Class  : Text_Class;
      Line   : String)
   is
   begin
      Writer.Put (Class, Line);
      Writer.New_Line (Class);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Line   : String)
   is
   begin
      Writer.Put_Line (Standard_Text, Line);
   end Put_Line;

   ---------------
   -- Put_Lines --
   ---------------

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String_Lists.List)
   is
   begin
      for Line of Lines loop
         Writer.Put_Line (Line);
      end loop;
   end Put_Lines;

   ---------------
   -- Put_Lines --
   ---------------

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String;
      Separator : Character := Character'Val (10))
   is
      Start : Positive := Lines'First;
   begin
      for I in Lines'Range loop
         if Lines (I) = Separator then
            Writer.Put_Line (Lines (Start .. I - 1));
            Start := I + 1;
         end if;
      end loop;

      if Start <= Lines'Last then
         Writer.Put (Lines (Start .. Lines'Last));
      end if;
   end Put_Lines;

   procedure Put_Names
     (Writer          : in out Text_Writer_Interface'Class;
      Names           : String_Lists.List;
      Available_Width : Natural := 72;
      Sorted          : Boolean := True;
      Down_First      : Boolean := True)
   is
      pragma Unreferenced (Down_First);

      package Sorting is
        new String_Lists.Generic_Sorting ("<");

      Ids          : String_Lists.List := Names;
      Longest      : Natural := 0;
      Cols         : Positive := 1;
      Col_Index    : Positive;
      Field_Width  : Positive;

   begin

      if Sorted then
         Sorting.Sort (Ids);
      end if;

      for Id of Ids loop
         if Id'Length > Longest then
            Longest := Id'Length;
         end if;
      end loop;

      if Longest = 0 then
         return;
      end if;

      Cols := Natural'Max
        (Natural'Min (Available_Width / (Longest + 2), 6), 1);
      Field_Width := Available_Width / Cols;

      Col_Index := 1;

      for Id of Ids loop
         declare
            Field : String (1 .. Field_Width) := (others => ' ');
         begin
            Field (1 .. Id'Length) := Id;
            Writer.Put (Field);
         end;
         if Col_Index = Cols then
            Col_Index := 1;
            Writer.New_Line;
         else
            Col_Index := Col_Index + 1;
         end if;
      end loop;

      if Col_Index /= 1 then
         Writer.New_Line;
      end if;

   end Put_Names;

end Nazar.Interfaces.Text_Writer;
