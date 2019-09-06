with Concorde.Sessions;

package Concorde.UI.Models.Commands is

   type Root_Command_Type is abstract tagged private;
   type Command_Type is access all Root_Command_Type'Class;

   procedure On_Activated
     (Command : Root_Command_Type;
      Session : Concorde.Sessions.Concorde_Session;
      Value   : String)
   is abstract;

private

   type Root_Command_Type is abstract tagged null record;

end Concorde.UI.Models.Commands;
