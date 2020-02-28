with Nazar.Interfaces.Text_Writer;

package body Nazar.Models.Console.Commands is

   function Line (S : String) return String
                  renames Nazar.Interfaces.Text_Writer.Line;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Cat_Command_Record is new Internal_Command with null record;

   overriding function Usage
     (Command : Cat_Command_Record)
      return String;

   overriding function Help
     (Command : Cat_Command_Record)
      return String;

   overriding function Check
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type Change_Scope_Command_Record is new Internal_Command with null record;

   overriding function Usage
     (Command : Change_Scope_Command_Record)
      return String;

   overriding function Help
     (Command : Change_Scope_Command_Record)
      return String;

   overriding function Check
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type Echo_Command_Record is new Internal_Command with null record;

   overriding function Usage
     (Command : Echo_Command_Record)
      return String;

   overriding function Help
     (Command : Echo_Command_Record)
      return String;

   overriding function Check
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type List_Command_Record is new Internal_Command with null record;

   overriding function Usage
     (Command : List_Command_Record)
      return String;

   overriding function Help
     (Command : List_Command_Record)
      return String;

   overriding function Check
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is null;

   -----------------
   -- Cat_Command --
   -----------------

   function Cat_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Cat_Command_Record'(Name => +"cat", Scope => Scope);
   end Cat_Command;

   --------------------------
   -- Change_Scope_Command --
   --------------------------

   function Change_Scope_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Change_Scope_Command_Record'(Name => +"cd", Scope => Scope);
   end Change_Scope_Command;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access);
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access)
        and then Arguments.Argument_Count = 1;
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version" or else Name = "n";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access);
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access);
   end Check;

   ------------------
   -- Echo_Command --
   ------------------

   function Echo_Command
     return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Echo_Command_Record'(Name => +"echo", Scope => null);
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
      for I in 1 .. Arguments.Argument_Count loop
         if I > 1 then
            Writer.Put (" ");
         end if;
         Writer.Put (Arguments.Argument (I));
      end loop;

      if not Arguments.Has_Binding ("n") then
         Writer.New_Line;
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
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
   begin
      null;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : Cat_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("Concatenate FILE(s) to standard output.")
        & Line ("")
        & Line ("      --help     display this help and exit")
        & Line ("      --version  output version information and exit");
   end Help;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : Change_Scope_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("Change the console's active scope")
        & Line ("")
        & Line ("Change the active scope to PATH.  The default PATH is the")
        & Line ("value of the HOME environment variable");
   end Help;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : Echo_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("Echo the STRING(s) to standard output.")
        & Line ("")
        & Line ("  -n             do not output the trailing newline")
        & Line ("      --help     display this help and exit")
        & Line ("      --version  output version information and exit");
   end Help;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : List_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("List information about the FILEs "
                   & "(the current scope by default).")
        & Line ("Sort entries alphabetically unless overridden");
   end Help;

   ------------------
   -- List_Command --
   ------------------

   function List_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return List_Command_Record'(Name => +"ls", Scope => Scope);
   end List_Command;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : Cat_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: cat FILE");
   end Usage;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : Change_Scope_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: cd [PATH]");
   end Usage;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : Echo_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: echo [ -n ] [STRING]...");
   end Usage;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : List_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: ls [OPTION]... [FILE]...");
   end Usage;

end Nazar.Models.Console.Commands;
