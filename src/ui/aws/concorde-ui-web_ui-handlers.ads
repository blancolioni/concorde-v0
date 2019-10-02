with Concorde.UI.Web_UI.Routes;

package Concorde.UI.Web_UI.Handlers is

   Handler_Error : exception;

   function Handle_Login
     return Routes.Request_Handler'Class;

   function Handle_New_Client
     return Routes.Request_Handler'Class;

end Concorde.UI.Web_UI.Handlers;
