with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Concorde.Commands is

   type Null_Writer_Record is
     new Writer_Interface with null record;

   overriding procedure Put
     (Writer : in out Null_Writer_Record;
      Text   : String)
   is null;

   overriding procedure New_Line
     (Writer : in out Null_Writer_Record)
   is null;

   overriding procedure Put_Error
     (Writer : in out Null_Writer_Record;
      Text   : String);

   package Command_Maps is
     new WL.String_Maps (Root_Concorde_Command'Class);

   Map : Command_Maps.Map;

   procedure Set_Flag
     (Arguments : in out Argument_List;
      Flag      : String);

   procedure Set_Named_Value
     (Arguments : in out Argument_List;
      Name      : String;
      Value     : String);

   procedure Set_Value
     (Arguments : in out Argument_List;
      Value     : String);

   function Scan_Arguments
     (Session       : Concorde.Sessions.Concorde_Session;
      Argument_Line : String)
      return Argument_List;

   procedure Iterate_Words
     (Session : Concorde.Sessions.Concorde_Session;
      Text    : String;
      Process : not null access
        procedure (Word : String));

   procedure Execute_Single_Command
     (Command : String;
      Session : in out Concorde.Sessions.Concorde_Session;
      Writer  : in out Writer_Interface'Class);

   ---------
   -- Add --
   ---------

   procedure Add
     (To         : in out Identifier_List;
      Identifier : String)
   is
   begin
      To.List.Append (Identifier);
   end Add;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Command   : Root_Concorde_Command'Class;
      Session   : in out Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is
   begin
      if Command.Administrator_Only
        and then not Session.Is_Administrator
      then
         Writer.Put_Error
           ("You must be an administrator to perform this action");
         return;
      end if;
      Command.Perform (Session, Writer, Arguments);
   end Execute;

   --------------------------
   -- Execute_Command_Line --
   --------------------------

   procedure Execute_Command_Line
     (Line    : String;
      Session : in out Concorde.Sessions.Concorde_Session;
      Writer  : in out Writer_Interface'Class)
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
         if Session.History_Length > 0 then
            declare
               Command : constant String := Session.History (-1);
            begin
               Writer.Put_Line (Command);
               Execute_Single_Command (Command, Session, Writer);
            end;
         else
            Writer.Put_Error ("!!: event not found");
         end if;
         return;
      end if;

      if Line (Line'First) = '!'
        and then Is_Integer (Line (Line'First + 1 .. Line'Last))
      then
         declare
            Image : constant String := Line (Line'First + 1 .. Line'Last);
            X     : constant Integer := Integer'Value (Image);
         begin
            if abs X <= Session.History_Length then
               declare
                  Command : constant String :=
                              Session.History (X);
               begin
                  Writer.Put_Line (Command);
                  Execute_Single_Command (Command, Session, Writer);
               end;
            else
               Writer.Put_Error (Line & ": event not found");
            end if;
            return;
         end;
      end if;

      Session.Add_To_History (Line);
      Execute_Single_Command (Line, Session, Writer);

   end Execute_Command_Line;

   ----------------------------
   -- Execute_Single_Command --
   ----------------------------

   procedure Execute_Single_Command
     (Command : String;
      Session : in out Concorde.Sessions.Concorde_Session;
      Writer  : in out Writer_Interface'Class)
   is
      Extended_Line : constant String := Command & ' ';
      First         : constant Positive :=
                        Ada.Strings.Fixed.Index_Non_Blank (Extended_Line);
      Index         : constant Positive :=
                        Ada.Strings.Fixed.Index (Extended_Line, " ", First);
      Command_Name  : constant String :=
                        Extended_Line
                          (Extended_Line'First .. Index - 1);
   begin
      if not Map.Contains (Command_Name) then
         Writer.Put_Error (Command_Name & ": command not found");
         return;
      end if;

      declare
         Command   : constant Root_Concorde_Command'Class :=
                       Map.Element (Command_Name);
         Arguments : constant Argument_List :=
                       Scan_Arguments
                         (Session,
                          Extended_Line (Index + 1 .. Extended_Line'Last));
      begin
         Command.Execute (Session, Writer, Arguments);
      end;
   end Execute_Single_Command;

   -------------------
   -- Iterate_Words --
   -------------------

   procedure Iterate_Words
     (Session : Concorde.Sessions.Concorde_Session;
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
               Name : constant String := Buffer (Var_Index .. Index);
               Value : constant String :=
                         Session.Environment_Value (Name, "");
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
               Process (Buffer (Buffer'First .. Index));
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
         Process (Buffer (Buffer'First .. Index));
      end if;

   end Iterate_Words;

   -----------------
   -- Null_Writer --
   -----------------

   function Null_Writer return Writer_Interface'Class is
   begin
      return Writer : Null_Writer_Record;
   end Null_Writer;

   ---------------
   -- Put_Error --
   ---------------

   overriding procedure Put_Error
     (Writer : in out Null_Writer_Record;
      Text   : String)
   is
      pragma Unreferenced (Writer);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "ERROR: " & Text);
   end Put_Error;

   procedure Put_Identifier_List
     (Writer : in out Writer_Interface'Class;
      List   : Identifier_List)
   is
      package Sorting is
        new String_Lists.Generic_Sorting ("<");

      Ids : String_Lists.List := List.List;
      Longest : Natural := 0;
      Cols    : Positive := 1;
      Col_Index : Positive;
      Field_Width : Positive;
      Screen_Width : constant := 72;

   begin
      Sorting.Sort (Ids);
      for Id of Ids loop
         if Id'Length > Longest then
            Longest := Id'Length;
         end if;
      end loop;

      if Longest = 0 then
         return;
      end if;

      Cols := Natural'Max (Natural'Min (Screen_Width / (Longest + 2), 6), 1);
      Field_Width := Screen_Width / Cols;

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
   end Put_Identifier_List;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Writer_Interface'Class;
      Text   : String)
   is
   begin
      Writer.Put (Text);
      Writer.New_Line;
   end Put_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Command_Name : String;
      Command      : Root_Concorde_Command'Class)
   is
   begin
      Map.Insert (Command_Name, Command);
   end Register;

   --------------------
   -- Scan_Arguments --
   --------------------

   function Scan_Arguments
     (Session       : Concorde.Sessions.Concorde_Session;
      Argument_Line : String)
      return Argument_List
   is

      Arguments : Argument_List;

      procedure Process_Argument
        (Arg : String);

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
                  Set_Flag (Arguments, Arg (Arg'First + 2 .. Arg'Last));
               else
                  declare
                     Name  : constant String :=
                               Arg (Arg'First + 2 .. Equal_Index - 1);
                     Value : constant String :=
                               Arg (Equal_Index + 1 .. Arg'Last);
                  begin
                     Set_Named_Value
                       (Arguments => Arguments,
                        Name      => Name,
                        Value     => Value);
                  end;
               end if;
            end;
         elsif Arg (Arg'First) = '-' then
            for Ch of Arg (Arg'First + 1 .. Arg'Last) loop
               Set_Flag (Arguments, (1 => Ch));
            end loop;
         else
            Set_Value (Arguments, Arg);
         end if;

      end Process_Argument;

   begin
      Iterate_Words (Session, Argument_Line, Process_Argument'Access);
      return Arguments;
   end Scan_Arguments;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Arguments : in out Argument_List;
      Flag      : String)
   is
   begin
      Arguments.Map.Insert (Flag, "");
   end Set_Flag;

   ---------------------
   -- Set_Named_Value --
   ---------------------

   procedure Set_Named_Value
     (Arguments : in out Argument_List;
      Name      : String;
      Value     : String)
   is
   begin
      Arguments.Map.Insert (Name, Value);
   end Set_Named_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Arguments : in out Argument_List;
      Value     : String)
   is
   begin
      Arguments.Vector.Append (Value);
   end Set_Value;

end Concorde.Commands;
