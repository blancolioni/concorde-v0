package body Concorde.UI.Web_UI.Handlers.Sessions is

   ----------------
   -- Handle_Get --
   ----------------

   overriding function Handle_Get
     (Handler    : Environment_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
   begin
      return State.Environment_Value (Parameters.Parameter ("name"));
   end Handle_Get;

end Concorde.UI.Web_UI.Handlers.Sessions;
