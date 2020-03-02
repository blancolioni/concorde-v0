private with WL.String_Maps;

with AWS.Response;
with AWS.Status;

with Nazar.Json;
with Nazar.Sessions;

package Nazar.Web_UI.Routes is

   Route_Error : exception;

   type Parameter_Container is tagged private;

   function Parameter
     (Container : Parameter_Container;
      Name      : String)
      return String;

   function To_Json
     (Container : Parameter_Container)
      return Nazar.Json.Json_Value'Class;

   type Request_Handler_Interface is interface;

   function Handle_Get
     (Handler    : Request_Handler_Interface;
      Session    : Nazar.Sessions.Nazar_Session;
      Parameters : Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class
      is abstract;

   function Handle_Post
     (Handler    : Request_Handler_Interface;
      Session    : Nazar.Sessions.Nazar_Session;
      Parameters : Parameter_Container'Class)
      return Nazar.Json.Json_Value'Class
   is abstract;

   procedure Add_Route
     (Method  : AWS.Status.Request_Method;
      Path    : String;
      Handler : not null access Request_Handler_Interface'Class);

   function Handle_Http_Request
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

   function Handle_Socket_Message
     (Message : String)
      return String;

private

   package String_Maps is
     new WL.String_Maps (String);

   type Parameter_Container is
     new String_Maps.Map with null record;

end Nazar.Web_UI.Routes;
