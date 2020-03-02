with Nazar.Web_UI.Routes;

package Nazar.Web_UI.Handlers is

   Handler_Error : exception;

   function Handle_Login
     return Routes.Request_Handler'Class;

   function Handle_New_Client
     return Routes.Request_Handler'Class;

   function Handle_Environment_Request
     return Routes.Request_Handler'Class;

   function Handle_Client_Request
     return Routes.Request_Handler'Class;

end Nazar.Web_UI.Handlers;
