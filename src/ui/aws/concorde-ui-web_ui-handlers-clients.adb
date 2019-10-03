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
      pragma Unreferenced (Handler);
      Model_Name    : constant String :=
        Parameters.Parameter ("model");
      New_Client_Id : constant Client_Id :=
        State.New_Client (Model_Name);
   begin
      return Response : Concorde.Json.Json_Object do
         if New_Client_Id = 0 then
            if Model_Name = "" then
               Response.Set_Property
                 ("error", "missing required argument: model");
            else
               Response.Set_Property
                 ("error", "no such model: " & Model_Name);
            end if;
         else
            Response.Set_Property ("clientId", Natural (New_Client_Id));
         end if;
      end return;
   end Handle_Post;

   -----------------
   -- Handle_Post --
   -----------------

   overriding function Handle_Post
     (Handler    : Client_Request_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (Handler);
      Request : Concorde.Json.Json_Object;
   begin
      Request.Set_Property ("data", Parameters.Parameter ("data"));
      return State.Handle_Client_Request
        (Client  => Client_Id'Value (Parameters.Parameter ("client")),
         Request => Request);
   end Handle_Post;

end Concorde.UI.Web_UI.Handlers.Clients;