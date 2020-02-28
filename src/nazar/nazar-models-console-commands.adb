package body Nazar.Models.Console.Commands is

   type Echo_Command_Record is new Internal_Command with null record;

   overriding procedure Execute
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type Cat_Command_Record is new Internal_Command with null record;

   overriding procedure Execute
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   -----------------
   -- Cat_Command --
   -----------------

   function Cat_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Cat_Command_Record'(Scope => Scope);
   end Cat_Command;

   ------------------
   -- Echo_Command --
   ------------------

   function Echo_Command
     return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Echo_Command_Record'(Scope => null);
   end Echo_Command;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      pragma Unreferenced (Command);
   begin
      if Arguments.Has_Binding ("help") then
         Writer.Put_Line ("Usage: echo [SHORT-OPTION]... [STRING]...");
         Writer.Put_Line ("  or:  echo LONG-OPTION");
         Writer.Put_Line ("Echo the STRING(s) to standard output.");
         Writer.New_Line;
         Writer.Put_Line
           ("  -n             do not output the trailing newline");
         Writer.Put_Line
           ("      --help     display this help and exit");
         Writer.Put_Line
           ("      --version  output version information and exit");
      elsif Arguments.Has_Binding ("version") then
         Internal_Command_Version ("echo", Writer);
      else
         for I in 1 .. Arguments.Argument_Count loop
            if I > 1 then
               Writer.Put (" ");
            end if;
            Writer.Put (Arguments.Argument (I));
         end loop;

         if not Arguments.Has_Binding ("n") then
            Writer.New_Line;
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
   begin
      if Arguments.Has_Binding ("help") then
         Writer.Put_Line ("Usage: cat [OPTION]... [FILE]...");
         Writer.Put_Line ("Concatenate FILE(s) to standard output.");
         Writer.New_Line;
         Writer.Put_Line
           ("      --help     display this help and exit");
         Writer.Put_Line
           ("      --version  output version information and exit");
      elsif Arguments.Has_Binding ("version") then
         Internal_Command_Version ("cat", Writer);
      else
         for I in 1 .. Arguments.Argument_Count loop
            declare
               File : constant Nazar.Interfaces.Hierarchy.Node_Reference_Class
                 := Command.Scope.Get_Node (Arguments.Argument (I));
            begin
               if File.Is_Empty then
                  Writer.Put_Line
                    (Nazar.Interfaces.Text_Writer.Error_Text,
                     Arguments.Argument (I) & ": no such file or directory");
               else
                  declare
                     procedure Process_Line (Line : String);

                     ------------------
                     -- Process_Line --
                     ------------------

                     procedure Process_Line (Line : String) is
                     begin
                        Writer.Put_Line (Line);
                     end Process_Line;

                  begin

                     Iterate_Lines (File.Get.Contents, Process_Line'Access);

                  exception
                     when others =>
                        Writer.Put_Line
                          (Nazar.Interfaces.Text_Writer.Error_Text,
                           Arguments.Argument (I) & ": cannot read");
                  end;
               end if;
            end;
         end loop;
      end if;
   end Execute;

end Nazar.Models.Console.Commands;
