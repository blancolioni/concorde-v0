private with Concorde.UI.Views;

private package Concorde.Commands.Views is

   procedure Load_View_Commands;

private

   type Load_View_Command is
     abstract new Root_Concorde_Command with null record;

   function Create_View
     (Command   : Load_View_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
      is abstract;

   overriding procedure Perform
     (Command   : Load_View_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

end Concorde.Commands.Views;
