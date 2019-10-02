with Concorde.UI.Web_UI.Handlers.Clients;
with Concorde.UI.Web_UI.Handlers.Login;

package body Concorde.UI.Web_UI.Handlers is

   ------------------
   -- Handle_Login --
   ------------------

   function Handle_Login
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Login.Login_Handler;
   end Handle_Login;

   -----------------------
   -- Handle_New_Client --
   -----------------------

   function Handle_New_Client
     return Routes.Request_Handler'Class
   is
   begin
      return Handler : Clients.New_Client_Handler;
   end Handle_New_Client;

end Concorde.UI.Web_UI.Handlers;
