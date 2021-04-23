with WL.String_Maps;

with Reiko.Control;

with Nazar.Interfaces.Text;
with Nazar.Interfaces.Text_Writer;

with Concorde.Calendar;

package body Concorde.Commands is

   package Info_Maps is new WL.String_Maps (String);

   Command_Usage : Info_Maps.Map;
   Command_Help  : Info_Maps.Map;

   type Time_Acceleration_Command is
     new System_Command with null record;

   overriding function Name
     (Command : Time_Acceleration_Command)
      return String
   is ("set-speed");

   overriding function Check
     (Command   : Time_Acceleration_Command;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command     : Time_Acceleration_Command;
      Arguments   : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer      : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type Pause_Resume_Command is
     new System_Command with
      record
         Pause : Boolean;
      end record;

   overriding function Name
     (Command : Pause_Resume_Command)
      return String
   is (if Command.Pause then "pause" else "resume");

   overriding function Check
     (Command   : Pause_Resume_Command;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command     : Pause_Resume_Command;
      Arguments   : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer      : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Time_Acceleration_Command;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
   begin
      if Arguments.Argument_Count /= 1 then
         return False;
      end if;

      declare
         Arg : constant String := Arguments.Argument (1);
      begin
         return Arg = "slow" or else Arg = "medium" or else Arg = "fast"
           or else Arg = "maximum"
           or else (for all Ch of Arg => Ch in '0' .. '9');
      end;
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Pause_Resume_Command;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
   begin
      return Arguments.Argument_Count = 0;
   end Check;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command     : Time_Acceleration_Command;
      Arguments   : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer      : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      pragma Unreferenced (Command, Environment, Writer);
      Speed : constant String := Arguments.Argument (1);
      Acc   : Concorde_Duration := 0.0;
   begin
      if Speed = "slow" then
         Acc := 3600.0;
      elsif Speed = "medium" then
         Acc := 6.0 * 3600.0;
      elsif Speed = "fast" then
         Acc := Concorde.Calendar.Days (1);
      elsif Speed = "maximum" then
         Acc := Concorde.Calendar.Days (10);
      else
         Acc := Concorde_Duration'Value (Speed);
      end if;

      Reiko.Control.Set_Acceleration (Reiko.Reiko_Duration (Acc));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command     : Pause_Resume_Command;
      Arguments   : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer      : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      pragma Unreferenced (Arguments, Environment, Writer);
   begin
      if Command.Pause then
         Reiko.Control.Pause;
      else
         Reiko.Control.Resume;
      end if;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : System_Command)
      return String
   is
      Command_Class : System_Command'Class renames
        System_Command'Class (Command);
   begin
      if Command_Help.Contains (Command_Class.Name) then
         return Command_Help.Element (Command_Class.Name);
      else
         return "no help information";
      end if;
   end Help;

   ---------------------------
   -- Set_Time_Acceleration --
   ---------------------------

   function Set_Time_Acceleration
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Command : Time_Acceleration_Command;
   end Set_Time_Acceleration;

   -------------------
   -- Start_Updates --
   -------------------

   function Start_Updates
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Command : constant Pause_Resume_Command :=
        Pause_Resume_Command'
          (Pause => False);
   end Start_Updates;

   ------------------
   -- Stop_Updates --
   ------------------

   function Stop_Updates
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Command : constant Pause_Resume_Command :=
        Pause_Resume_Command'
          (Pause => True);
   end Stop_Updates;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : System_Command)
      return String
   is
      Command_Class : System_Command'Class renames
        System_Command'Class (Command);
   begin
      if Command_Usage.Contains (Command_Class.Name) then
         return Command_Usage.Element (Command_Class.Name);
      else
         return "no usage information";
      end if;
   end Usage;

end Concorde.Commands;
