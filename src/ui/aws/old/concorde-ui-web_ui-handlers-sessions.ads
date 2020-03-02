with Nazar.Json;

package Concorde.UI.Web_UI.Handlers.Sessions is

   type Environment_Handler is
     new Routes.Request_Handler with private;

private

   type Environment_Handler is
     new Routes.Request_Handler with null record;

   overriding function Handle_Get
     (Handler    : Environment_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class;

end Concorde.UI.Web_UI.Handlers.Sessions;
