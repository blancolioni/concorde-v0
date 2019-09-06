with Ada.Strings.Fixed;
with Ada.Text_IO;

with Concorde.Commands;
with Concorde.Contexts;

package body Concorde.Repl is

   type Repl_Writer is
     new Concorde.Commands.Writer_Interface with null record;

   overriding procedure Put
     (Writer : Repl_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : Repl_Writer);

   overriding procedure Put_Error
     (Writer  : Repl_Writer;
      Message : String);

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Session : Concorde.Sessions.Concorde_Session)
   is
      Writer  : Repl_Writer;
   begin
      loop
         Ada.Text_IO.Put
           (Session.Current_Context.Name
            & (if Session.Administrator then "# " else "> "));
         Ada.Text_IO.Flush;
         begin
            declare
               Line : constant String := Ada.Text_IO.Get_Line;
            begin
               exit when Line = "exit";

               Concorde.Commands.Execute_Command_Line
                 (Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both),
                  Session, Writer);
            end;
         exception
            when Ada.Text_IO.End_Error =>
               exit;
         end;
      end loop;
   end Execute;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Writer : Repl_Writer)
   is
      pragma Unreferenced (Writer);
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Writer : Repl_Writer;
      Text   : String)
   is
      pragma Unreferenced (Writer);
   begin
      Ada.Text_IO.Put (Text);
   end Put;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer  : Repl_Writer;
      Message : String)
   is
      pragma Unreferenced (Writer);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Message);
   end Put_Error;

   ----------
   -- Read --
   ----------

   procedure Read
     (Session : Concorde.Sessions.Concorde_Session;
      Path    : String)
   is
      use Ada.Text_IO;
      File : File_Type;
      Writer  : Repl_Writer;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Concorde.Commands.Execute_Command_Line
              (Line, Session, Writer);
         end;
      end loop;
      Close (File);
   end Read;

end Concorde.Repl;
