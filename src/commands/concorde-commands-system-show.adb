with Concorde.Json;

package body Concorde.Commands.System.Show is

   type Show_Command_Record is
     new Root_Concorde_Command with null record;

   overriding procedure Perform
     (Command   : Show_Command_Record;
      Session   : in out Concorde.Sessions.Concorde_Session;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List);

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Show_Command_Record;
      Session   : in out Concorde.Sessions.Concorde_Session;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Session, Context);
      Response : Concorde.Json.Json_Object;
      View_Name : constant String :=
        Argument (Arguments, "view", "Table");
      Model_Name : constant String :=
        Argument (Arguments, "model");
      Model_Args : constant String :=
        Argument (Arguments, "model-args", "");
   begin
      Response.Set_Property
        ("control", "replace-view");
      Response.Set_Property
        ("view", View_Name);
      Response.Set_Property
        ("model", Model_Name);
      Response.Set_Property
        ("modelArg", Model_Args);
      Writer.Control (Response);
      Writer.Put_Line ("Loading " & View_Name & "/" & Model_Name &
                         " " & Model_Args);
   end Perform;

   ------------------
   -- Show_Command --
   ------------------

   function Show_Command return Root_Concorde_Command'Class is
   begin
      return Command : Show_Command_Record;
   end Show_Command;

end Concorde.Commands.System.Show;
