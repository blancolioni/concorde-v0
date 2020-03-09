with Nazar.Interfaces.Commands;

with Concorde.Version;

package Concorde.Commands is

   function Set_Time_Acceleration
     return Nazar.Interfaces.Commands.Command_Interface'Class;

   function Start_Updates
     return Nazar.Interfaces.Commands.Command_Interface'Class;

   function Stop_Updates
     return Nazar.Interfaces.Commands.Command_Interface'Class;

private

   type System_Command is
     abstract new Nazar.Interfaces.Commands.Command_Interface with null record;

   overriding function Version
     (Command : System_Command) return String
   is (Concorde.Version.Version_String);

   overriding function Usage
     (Command : System_Command)
      return String;

   overriding function Help
     (Command : System_Command)
      return String;

end Concorde.Commands;
