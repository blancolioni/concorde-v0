package body Nazar.Web_UI.Handlers.Sessions is

   ----------------
   -- Handle_Get --
   ----------------

   overriding function Handle_Get
     (Handler    : Environment_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
   begin
      return State.Environment_Value (Parameters.Parameter ("name"));
   end Handle_Get;

end Nazar.Web_UI.Handlers.Sessions;
