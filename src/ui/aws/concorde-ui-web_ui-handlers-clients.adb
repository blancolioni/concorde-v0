package body Concorde.UI.Web_UI.Handlers.Clients is

   -----------------
   -- Handle_Post --
   -----------------

   overriding function Handle_Post
     (Handler    : New_Client_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler, Parameters);
      New_Client_Id : constant Client_Id :=
        State.New_Client;
   begin
      return Response : Concorde.Json.Json_Object do
         Response.Set_Property ("clientId", Natural (New_Client_Id));
      end return;
   end Handle_Post;

end Concorde.UI.Web_UI.Handlers.Clients;
