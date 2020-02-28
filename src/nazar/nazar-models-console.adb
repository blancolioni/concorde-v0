with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Nazar.Version;

with Nazar.Models.Console.Commands;

package body Nazar.Models.Console is

   procedure Iterate_Words
     (Model   : Root_Console_Model'Class;
      Text    : String;
      Process : not null access
        procedure (Word : String));

   function Parse_Console_Command
     (Model : in out Root_Console_Model'Class;
      Line  : String)
      return Console_Command;

   procedure Scan_Arguments
     (Model         : in out Root_Console_Model'Class;
      Argument_Line : String;
      Vector        : in out String_Vectors.Vector;
      Bindings      : in out String_Maps.Map;
      Success       : out Boolean);

   --------------------
   -- Append_History --
   --------------------

   procedure Append_History
     (Model : in out Root_Console_Model'Class;
      Item  : String)
   is
   begin
      if Model.History.Is_Empty
        or else Model.History.Last_Element /= Item
      then
         Model.History.Append (Item);
      end if;
   end Append_History;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Model   : in out Root_Console_Model;
      Command : Nazar.Models.Console.Console_Command'Class)
   is
   begin
      if Model.Commands.Contains (Command.Name) then
         Model.Commands.Element (Command.Name).Execute
           (Command, Model);
      else
         Model.Put_Line
           (Nazar.Interfaces.Text_Writer.Error_Text,
            Command.Name & ": command not found");
      end if;
   end Execute;

   --------------------------
   -- Execute_Command_Line --
   --------------------------

   procedure Execute_Command_Line
     (Model : in out Root_Console_Model'Class;
      Line  : String)
   is

      function Is_Integer (Image : String) return Boolean;

      ----------------
      -- Is_Integer --
      ----------------

      function Is_Integer (Image : String) return Boolean is
         First : Boolean := True;
      begin
         for Ch of Image loop
            if Ch in '+' | '-' then
               if not First then
                  return False;
               end if;
            elsif Ch not in '0' .. '9' then
               return False;
            end if;
            First := False;
         end loop;
         return True;
      end Is_Integer;

   begin
      if Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both) = "" then
         return;
      end if;

      if Line = "!!" then
         if Model.History_Length > 0 then
            declare
               Command : constant String := Model.Get_History (-1);
            begin
               Model.Put_Line (Command);
               Model.Execute_Single_Command (Command);
            end;
         else
            Model.Put_Line
              (Nazar.Interfaces.Text_Writer.Error_Text,
               "!!: event not found");
         end if;
      elsif Line (Line'First) = '!'
        and then Is_Integer (Line (Line'First + 1 .. Line'Last))
      then
         declare
            Image : constant String := Line (Line'First + 1 .. Line'Last);
            X     : constant Integer := Integer'Value (Image);
         begin
            if abs X <= Model.History_Length then
               declare
                  Command : constant String := Model.Get_History (X);
               begin
                  Model.Put_Line (Command);
                  Model.Execute_Single_Command (Command);
               end;
            else
               Model.Put_Line
                 (Nazar.Interfaces.Text_Writer.Error_Text,
                  Image & ": event not found");
            end if;
         end;
      else
         Model.Append_History (Line);
         Model.Execute_Single_Command (Line);
      end if;

      Model.Notify_Observers;

   end Execute_Command_Line;

   ----------------------------
   -- Execute_Single_Command --
   ----------------------------

   procedure Execute_Single_Command
     (Model : in out Root_Console_Model'Class;
      Line  : String)
   is
      Command : constant Console_Command :=
        Model.Parse_Console_Command (Line);
   begin
      if Command.Valid then
         Model.Execute (Command);
      end if;
   end Execute_Single_Command;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Model   : Root_Console_Model'Class;
      Offset  : Integer)
      return String
   is
   begin
      if Offset < 0 then
         return Model.History.Element
           (Model.History.Last_Index + 1 + Offset);
      else
         return Model.History.Element (Offset);
      end if;
   end Get_History;

   --------------------
   -- History_Length --
   --------------------

   function History_Length
     (Model : Root_Console_Model'Class)
      return Natural
   is
   begin
      return Model.History.Last_Index;
   end History_Length;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model         : in out Root_Console_Model;
      Root          : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
      Environment   : not null access Nazar.Interfaces.Strings
      .String_Environment_Interface'Class;
      Default_Scope : String)
   is
   begin
      Model.Environment :=
        Environment_Access (Environment);
      Model.Scope :=
        Nazar.Models.Scope.Scope_Model (Root, Default_Scope);
      Model.Environment.Set_String_Value
        ("CURRENT_SCOPE", Default_Scope);

      Model.Set_Command
        ("echo", Commands.Echo_Command);

      Model.Set_Command
        ("cat", Commands.Cat_Command (Model.Scope));

   end Initialize;

   ------------------------------
   -- Internal_Command_Version --
   ------------------------------

   procedure Internal_Command_Version
     (Command_Name : String;
      Writer       : in out
        Nazar.Interfaces.Text_Writer.Text_Writer_Interface'Class)
   is
   begin
      Writer.Put_Line (Command_Name
                       & " (" & Nazar.Version.Repository_Name
                       & " builtins) "
                       & Nazar.Version.Version_String);
   end Internal_Command_Version;

   -------------------
   -- Iterate_Lines --
   -------------------

   procedure Iterate_Lines
     (Lines   : String;
      Process : not null access
        procedure (Line : String))
   is
      Start : Positive := Lines'First;
   begin
      for I in Lines'Range loop
         if Character'Pos (Lines (I)) = 10 then
            Process (Lines (Start .. I - 1));
            Start := I + 1;
         end if;
      end loop;

      if Start <= Lines'Last then
         Process (Lines (Start .. Lines'Last));
      end if;
   end Iterate_Lines;

   -------------------
   -- Iterate_Words --
   -------------------

   procedure Iterate_Words
     (Model   : Root_Console_Model'Class;
      Text    : String;
      Process : not null access
        procedure (Word : String))
   is
      use Ada.Characters.Handling;
      Double_Quote : Boolean := False;
      Single_Quote : Boolean := False;
      Escape       : Boolean := False;
      Variable     : Boolean := False;
      Skipping     : Boolean := True;
      Buffer       : String (1 .. 1024);
      Index        : Natural := 0;
      Var_Index    : Natural := 0;

      procedure Add (Ch : Character);

      procedure Check_Variable;

      ---------
      -- Add --
      ---------

      procedure Add (Ch : Character) is
      begin
         Index := Index + 1;
         Buffer (Index) := Ch;
      end Add;

      --------------------
      -- Check_Variable --
      --------------------

      procedure Check_Variable is
      begin
         if Variable then
            declare
               Name  : constant String := Buffer (Var_Index .. Index);
               Value : constant String :=
                 Model.Environment.Get_String_Value (Name);
            begin
               Index := Var_Index - 1;
               for Ch of Value loop
                  Add (Ch);
               end loop;
            end;
            Variable := False;
         end if;
      end Check_Variable;

   begin
      for Ch of Text loop
         if Skipping then
            if not Is_Space (Ch) then
               Skipping := False;
               Index := Buffer'First - 1;
            end if;
         end if;

         if not Skipping then
            if Escape then
               Add (Ch);
               Escape := False;
            elsif Double_Quote then
               if Ch = '"' then
                  Double_Quote := False;
               else
                  if Ch = '$' then
                     Variable := True;
                     Var_Index := Index + 1;
                  else
                     Add (Ch);
                  end if;
               end if;
            elsif Single_Quote then
               if Ch = ''' then
                  Single_Quote := False;
               else
                  Add (Ch);
               end if;
            elsif Ch = '\' then
               Escape := True;
            elsif Ch = ''' then
               Single_Quote := True;
            elsif Ch = '"' then
               Double_Quote := True;
            elsif Is_Space (Ch) then
               Check_Variable;
               if Index >= Buffer'First then
                  Process (Buffer (Buffer'First .. Index));
               end if;
               Index := 0;
               Skipping := True;
            else
               if Variable
                 and then not Is_Alphanumeric (Ch)
                 and then Ch not in '-' | '_'
               then
                  Check_Variable;
               end if;
               if Ch = '$' then
                  Variable := True;
                  Var_Index := Index + 1;
               else
                  Add (Ch);
               end if;
            end if;
         end if;
      end loop;

      if not Skipping then
         Check_Variable;
         if Index >= Buffer'First then
            Process (Buffer (Buffer'First .. Index));
         end if;
      end if;

   end Iterate_Words;

   ---------------------------
   -- Parse_Console_Command --
   ---------------------------

   function Parse_Console_Command
     (Model : in out Root_Console_Model'Class;
      Line  : String)
      return Console_Command
   is
      Extended_Line : constant String := Line & ' ';
      First         : constant Positive :=
        Ada.Strings.Fixed.Index_Non_Blank (Extended_Line);
      Index         : constant Positive :=
        Ada.Strings.Fixed.Index (Extended_Line, " ", First);
      Command_Name  : constant String :=
        Extended_Line
          (Extended_Line'First .. Index - 1);
   begin
      if Command_Name = "" then
         return (others => <>);
      end if;

      return Command : Console_Command := Console_Command'
        (Command => Ada.Strings.Unbounded.To_Unbounded_String (Command_Name),
         others  => <>)
      do
         Model.Scan_Arguments
           (Argument_Line => Extended_Line (Index + 1 .. Extended_Line'Last),
            Vector        => Command.Vector,
            Bindings      => Command.Map,
            Success       => Command.Valid);
      end return;
   end Parse_Console_Command;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (Model         : in out Root_Console_Model'Class;
      Argument_Line : String;
      Vector        : in out String_Vectors.Vector;
      Bindings      : in out String_Maps.Map;
      Success       : out Boolean)
   is

      procedure Process_Argument
        (Arg : String);

      procedure Set_Flag
        (Flag      : String);

      procedure Set_Named_Value
        (Name      : String;
         Value     : String);

      procedure Set_Value
        (Value     : String);

      ----------------------
      -- Process_Argument --
      ----------------------

      procedure Process_Argument
        (Arg : String)
      is
      begin
         if Arg = "-" or else Arg = "--" then
            null;
         elsif Arg (Arg'First) = '-'
           and then Arg (Arg'First + 1) = '-'
         then
            declare
               Equal_Index : constant Natural :=
                 Ada.Strings.Fixed.Index (Arg, "=");
            begin
               if Equal_Index = 0 then
                  Set_Flag (Arg (Arg'First + 2 .. Arg'Last));
               else
                  declare
                     Name  : constant String :=
                       Arg (Arg'First + 2 .. Equal_Index - 1);
                     Value : constant String :=
                       Arg (Equal_Index + 1 .. Arg'Last);
                  begin
                     Set_Named_Value (Name, Value);
                  end;
               end if;
            end;
         elsif Arg (Arg'First) = '-' then
            for Ch of Arg (Arg'First + 1 .. Arg'Last) loop
               Set_Flag ((1 => Ch));
            end loop;
         else
            Set_Value (Arg);
         end if;

      end Process_Argument;

      --------------
      -- Set_Flag --
      --------------

      procedure Set_Flag
        (Flag      : String)
      is
      begin
         Bindings.Insert (Flag, "");
      end Set_Flag;

      ---------------------
      -- Set_Named_Value --
      ---------------------

      procedure Set_Named_Value
        (Name      : String;
         Value     : String)
      is
      begin
         Bindings.Insert (Name, Value);
      end Set_Named_Value;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Value     : String)
      is
      begin
         Vector.Append (Value);
      end Set_Value;

   begin
      Success := True;
      Iterate_Words (Model, Argument_Line, Process_Argument'Access);
   end Scan_Arguments;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (Model   : in out Root_Console_Model;
      Name    : String;
      Command : Nazar.Interfaces.Commands.Command_Interface'Class)
   is
   begin
      if Model.Commands.Contains (Name) then
         Model.Commands.Replace (Name, Command);
      else
         Model.Commands.Insert (Name, Command);
      end if;
   end Set_Command;

end Nazar.Models.Console;